use byteorder::{LittleEndian, WriteBytesExt};
use compression::compress_buf;
use err::Error;
use fxhash::{FxBuildHasher, FxHashMap as Map};
use gdk;
use gdk_pixbuf::{Colorspace, Pixbuf, PixbufExt};
use gio::FileExt;
use gtk::{
    self,
    prelude::*,
    CellLayoutExt,
    TreeStoreExtManual,
    TreeViewExt,
    WidgetExt,
};
use nx::{self, GenericNode};
use nx_utils::NxBreadthPathIter;
use pango::{EllipsizeMode, WrapMode};
use std::{
    collections::HashMap,
    fmt,
    fs::File,
    io::{prelude::*, BufWriter, Cursor, SeekFrom},
    mem,
    path,
    sync::{Arc, Mutex, MutexGuard},
};
use ui::{get_wrap_width, run_msg_dialog, Content, NodeDisplay, View};

pub struct AppState {
    pub open_files:   OpenFiles,
    pub window_width: u32,
}

pub struct OpenFiles {
    files: Vec<Arc<Mutex<OpenFile>>>,
    icons: Arc<Icons>,
}

pub struct OpenFile {
    nx_file:           nx::File,
    curr_selection:    Option<gtk::TreeIter>,
    name_buffer_dirty: bool,
    val_buffer_dirty:  bool,
    diff:              FileDiff,
}

pub struct Icons {
    pub str_type:    Pixbuf,
    pub int_type:    Pixbuf,
    pub float_type:  Pixbuf,
    pub vector_type: Pixbuf,
    pub img_type:    Pixbuf,
    pub audio_type:  Pixbuf,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FileDiff {
    modifications: Map<Vec<i32>, ChangedNode>,
    history:       Vec<(Vec<i32>, NodeDiff)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ChangedNode {
    pub name:     Option<String>,
    pub val:      Option<NodeValue>,
    pub children: Vec<NewNode>,
    pub deleted:  bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct NewNode {
    pub id:       usize,
    pub name:     String,
    pub val:      NodeValue,
    pub children: Vec<NewNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeDiff {
    NameChange(Option<String>, String),
    ValueChange(Option<NodeValue>, NodeValue),
    AddChild(NewNode),
    Delete,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ForwardNodeDiff {
    NameChange(String),
    ValueChange(NodeValue),
    AddChild(NewNode),
    Delete,
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeValue {
    Empty,
    Str(String),
    Int(i64),
    Float(f64),
    Vector(i32, i32),
    Img(Pixbuf),
    Audio(u8), // TODO
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum NodeType {
    Empty = 0u8,
    Int = 1,
    Float = 2,
    Str = 3,
    Vector = 4,
    Img = 5,
    Audio = 6,
}

pub enum Image<'a> {
    NxBitmap(nx::bitmap::Bitmap<'a>),
    Compressed(Vec<u8>),
}

impl AppState {
    #[inline]
    pub fn new() -> Self {
        Self {
            open_files:   OpenFiles::new(),
            window_width: 0,
        }
    }
}

impl OpenFiles {
    #[inline]
    pub fn new() -> Self {
        Self {
            files: Vec::with_capacity(2),
            icons: Arc::new(Icons::new(24)),
        }
    }

    pub fn new_file<S: AsRef<path::Path>>(
        &mut self,
        nf: nx::File,
        filename: S,
        content: &Arc<Mutex<Content>>,
        window: &gtk::ApplicationWindow,
        window_width: u32,
    ) {
        self.files
            .push(Arc::new(Mutex::new(OpenFile::new(nf, None))));
        let of = self.files.last().unwrap();

        let of_unwrapped = of.lock().unwrap();
        let root = of_unwrapped.nx_file().root();
        let tree_store = gtk::TreeStore::new(&[
            String::static_type(),
            Pixbuf::static_type(),
            String::static_type(),
            Pixbuf::static_type(),
            u8::static_type(),
        ]);

        let root_iter =
            nx_onto_tree_store(&root, &tree_store, None, &self.icons);

        for n in root.iter() {
            nx_onto_tree_store(&n, &tree_store, Some(&root_iter), &self.icons);
        }

        let tree_view = gtk::TreeView::new_with_model(&tree_store);
        tree_view.set_halign(gtk::Align::Center);
        tree_view.set_valign(gtk::Align::Start);
        tree_view.set_hexpand(true);
        tree_view.set_vexpand(true);
        tree_view.set_hexpand_set(true);
        tree_view.set_vexpand_set(true);
        tree_view.set_headers_visible(false);
        tree_view.set_enable_tree_lines(true);
        tree_view.set_vscroll_policy(gtk::ScrollablePolicy::Natural);

        append_text_column(&tree_view, 0, window_width, false);
        append_pixbuf_column(&tree_view, 1);
        append_text_column(&tree_view, 2, window_width, false);
        append_pixbuf_column(&tree_view, 3);

        let mut c = content.lock().unwrap();
        let view_struct = View::new(
            c.notebook(),
            tree_view.clone(),
            filename
                .as_ref()
                .to_str()
                .expect("filename is not valid UTF-8"),
        );

        {
            let of = Arc::clone(&of);
            let icons = Arc::clone(&self.icons);
            tree_view.connect_test_expand_row(move |tv, titer, tpath| {
                let model_store: gtk::TreeStore = tv
                    .get_model()
                    .clone()
                    .expect("gtk::TreeView expected to have a gtk::TreeModel")
                    .downcast()
                    .expect(
                        "failed to downcast gtk::TreeModel => gtk::TreeStore",
                    );

                let immed_child_tree_iter =
                    if let Some(ti) = model_store.iter_children(titer) {
                        if model_store.iter_has_child(&ti) {
                            // We've already got the necessary data in the
                            // store.
                            return Inhibit(false);
                        } else {
                            ti
                        }
                    } else {
                        // No data.
                        return Inhibit(true);
                    };

                let mut expanded_path = tpath.get_indices();
                let of = of.lock().unwrap();
                let file_diff = &of.diff;
                if let Some(expanded_node) =
                    get_node_from_indices(of.nx_file().root(), &expanded_path)
                {
                    // Actual modifications happen here.
                    for (i, immed_child_node) in
                        expanded_node.iter().enumerate()
                    {
                        expanded_path.push(i as i32);

                        if let Some(immed_child_diff) =
                            file_diff.get_modified(&expanded_path)
                        {
                            for new_child in immed_child_diff.children.iter() {
                                new_node_onto_tree_store(
                                    new_child,
                                    &model_store,
                                    Some(&immed_child_tree_iter),
                                    &icons,
                                );
                            }
                        }

                        for (j, sndry_child_node) in
                            immed_child_node.iter().enumerate()
                        {
                            expanded_path.push(j as i32);

                            if let Some(sndry_child_diff) =
                                file_diff.get_modified(&expanded_path)
                            {
                                if !sndry_child_diff.deleted {
                                    changed_nx_onto_tree_store(
                                        &sndry_child_node,
                                        &model_store,
                                        Some(&immed_child_tree_iter),
                                        &icons,
                                        sndry_child_diff.name.as_ref(),
                                        sndry_child_diff.val.as_ref(),
                                    );
                                }
                            } else {
                                nx_onto_tree_store(
                                    &sndry_child_node,
                                    &model_store,
                                    Some(&immed_child_tree_iter),
                                    &icons,
                                );
                            }

                            expanded_path.pop();
                        }

                        model_store.iter_next(&immed_child_tree_iter);
                        expanded_path.pop();
                    }
                } else {
                    unimplemented!();
                }

                Inhibit(false)
            });
        }

        tree_view.connect_test_collapse_row(|tv, titer, _| {
            let model_store: gtk::TreeStore = tv
                .get_model()
                .clone()
                .expect("gtk::TreeView expected to have a gtk::TreeModel")
                .downcast()
                .expect("failed to downcast gtk::TreeModel => gtk::TreeStore");

            let immed_child_tree_iter =
                if let Some(ti) = model_store.iter_children(titer) {
                    ti
                } else {
                    // No children at all, nothing to deallocate.
                    return Inhibit(false);
                };

            // Actual modifications happen here.
            if let Some(sndry_child_tree_iter) =
                model_store.iter_children(&immed_child_tree_iter)
            {
                while model_store.remove(&sndry_child_tree_iter) {}
            }
            while model_store.iter_next(&immed_child_tree_iter) {
                if let Some(sndry_child_tree_iter) =
                    model_store.iter_children(&immed_child_tree_iter)
                {
                    while model_store.remove(&sndry_child_tree_iter) {}
                }
            }

            Inhibit(false)
        });

        // Monitor name text buffer for changes.
        {
            let of = Arc::clone(of);
            view_struct
                .node_view
                .name_display
                .get_buffer()
                .unwrap()
                .connect_changed(move |_| {
                    of.lock().unwrap().name_buffer_dirty = true;
                });
        }

        let view = Arc::new(Mutex::new(view_struct));

        let v = view.lock().unwrap();

        {
            let view = Arc::clone(&view);
            let of = Arc::clone(&of);
            tree_view.connect_cursor_changed(move |tv| {
                let path = if let (Some(p), _) = tv.get_cursor() {
                    p
                } else {
                    return;
                };

                let model =
                    tv.get_model().expect("no model for gtk::TreeView");

                if let Some(iter) = model.get_iter(&path) {
                    if let Ok(mut of) = of.try_lock() {
                        of.set_curr_selection(iter.clone());
                        of.name_buffer_dirty = false;
                        of.val_buffer_dirty = false;
                    } else {
                        return;
                    }

                    let (name, text_val, img_val) = (
                        model.get_value(&iter, 0),
                        model.get_value(&iter, 2),
                        model.get_value(&iter, 3),
                    );

                    let mut v = if let Ok(v) = view.try_lock() {
                        v
                    } else {
                        return;
                    };
                    v.node_view
                        .name_display
                        .get_buffer()
                        .unwrap()
                        .set_text(name.get().unwrap());

                    if let Some(text) = text_val.get::<&str>() {
                        let new_text_view =
                            v.node_view.set_text(text, path.get_indices());

                        // Monitor this new text buffer for changes.
                        {
                            let of = Arc::clone(&of);
                            new_text_view
                                .get_buffer()
                                .unwrap()
                                .connect_changed(move |_| {
                                    if let Ok(mut of) = of.try_lock() {
                                        of.val_buffer_dirty = true;
                                    }
                                });
                        }
                    } else if let Some(pixbuf) = img_val.get::<Pixbuf>() {
                        v.node_view.set_img(
                            gtk::Image::new_from_pixbuf(&pixbuf),
                            path.get_indices(),
                        );
                    } else {
                        return;
                    }

                    v.node_view.show();
                }
            });
        }

        // Hook up NodeView buttons.
        {
            let view = Arc::clone(&view);
            let of = Arc::clone(of);
            let w = window.clone();
            v.node_view.buttons.record_button.connect_clicked(move |_| {
                let v = view.lock().unwrap();
                let mut of = of.lock().unwrap();

                let (name_edited, val_edited) =
                    (of.name_buffer_dirty, of.val_buffer_dirty);
                if !name_edited && !val_edited {
                    return;
                }

                let model = v.tree_view.gtk_tree_view.get_model().unwrap();

                let (path, ntype) =
                    if let Some(ref curr_selection) = of.curr_selection {
                        let path = model
                            .get_path(curr_selection)
                            .expect("curr_selection has no path")
                            .get_indices();
                        let ntype: u8 =
                            model.get_value(curr_selection, 4).get().unwrap();

                        (path, ntype)
                    } else {
                        return;
                    };

                if name_edited && !val_edited {
                    of.name_buffer_dirty = false;

                    let name_buffer =
                        v.node_view.name_display.get_buffer().unwrap();
                    let new_name = name_buffer
                        .get_text(
                            &name_buffer.get_start_iter(),
                            &name_buffer.get_end_iter(),
                            true,
                        )
                        .expect("TextBufferExt::get_text failed");

                    if let Some(ref curr_selection) = of.curr_selection {
                        let store: gtk::TreeStore = model.downcast().expect(
                            "failed to downcast gtk::TreeModel => \
                             gtk::TreeStore",
                        );

                        store.set(curr_selection, &[0], &[&new_name]);
                    } else {
                        return;
                    }

                    of.record_name(path, new_name);
                } else if !name_edited && val_edited {
                    match v.node_view.node_display {
                        NodeDisplay::Empty(_) => of.val_buffer_dirty = false,
                        NodeDisplay::Text(_, ref text_view) => {
                            let ntype: NodeType = ntype.into();

                            let text_buffer = text_view.get_buffer().unwrap();
                            let mut text_content = text_buffer
                                .get_text(
                                    &text_buffer.get_start_iter(),
                                    &text_buffer.get_end_iter(),
                                    true,
                                )
                                .expect("TextBufferExt::get_text failed");

                            match of.record_val_from_str(
                                ntype,
                                path,
                                &text_content,
                            ) {
                                Ok(Some(formatted)) => {
                                    text_buffer.set_text(&formatted);
                                    text_content = formatted;
                                },
                                Err(e) => {
                                    run_msg_dialog(
                                        &w,
                                        "record error",
                                        &e.to_string(),
                                        gtk::MessageType::Error,
                                    );

                                    return;
                                },
                                _ => (),
                            }

                            of.val_buffer_dirty = false;

                            if let Some(ref curr_selection) = of.curr_selection
                            {
                                let store: gtk::TreeStore =
                                        model.downcast()
                                            .expect(
                                                "failed to downcast \
                                                 gtk::TreeModel => \
                                                 gtk::TreeStore"
                                            );
                                store.set(
                                    curr_selection,
                                    &[2],
                                    &[&text_content],
                                );
                            }
                        },
                        NodeDisplay::Image(_) => unimplemented!(
                            "TODO: implement Image modifications"
                        ),
                        NodeDisplay::Audio(_) => unimplemented!(
                            "TODO: implement Audio modifications"
                        ),
                    }
                } else {
                    let name_buffer =
                        v.node_view.name_display.get_buffer().unwrap();
                    let new_name = name_buffer
                        .get_text(
                            &name_buffer.get_start_iter(),
                            &name_buffer.get_end_iter(),
                            true,
                        )
                        .expect("TextBufferExt::get_text failed");

                    match v.node_view.node_display {
                        NodeDisplay::Empty(_) => of.val_buffer_dirty = false,
                        NodeDisplay::Text(_, ref text_view) => {
                            let ntype: NodeType = ntype.into();

                            let text_buffer = text_view.get_buffer().unwrap();
                            let mut text_content = text_buffer
                                .get_text(
                                    &text_buffer.get_start_iter(),
                                    &text_buffer.get_end_iter(),
                                    true,
                                )
                                .expect("TextBufferExt::get_text failed");

                            match of.record_val_from_str(
                                ntype,
                                path.clone(),
                                &text_content,
                            ) {
                                Ok(Some(formatted)) => {
                                    text_buffer.set_text(&formatted);
                                    text_content = formatted;
                                },
                                Err(e) => {
                                    run_msg_dialog(
                                        &w,
                                        "record error",
                                        &e.to_string(),
                                        gtk::MessageType::Error,
                                    );

                                    return;
                                },
                                _ => (),
                            }

                            of.val_buffer_dirty = false;

                            if let Some(ref curr_selection) = of.curr_selection
                            {
                                let store: gtk::TreeStore =
                                        model.downcast()
                                            .expect(
                                                "failed to downcast \
                                                 gtk::TreeModel => \
                                                 gtk::TreeStore"
                                            );
                                store.set(
                                    curr_selection,
                                    &[0, 2],
                                    &[&new_name, &text_content],
                                );
                            }
                        },
                        NodeDisplay::Image(_) => unimplemented!(
                            "TODO: implement Image modifications"
                        ),
                        NodeDisplay::Audio(_) => unimplemented!(
                            "TODO: implement Audio modifications"
                        ),
                    }

                    of.record_name(path, new_name);
                    of.name_buffer_dirty = false;
                }
            });
        }
        {
            let view = Arc::clone(&view);
            v.node_view
                .buttons
                .insert_button
                .connect_clicked(move |button| {
                    let v = view.lock().unwrap();

                    v.node_view.buttons.insert_menu.menu.show_all();
                    v.node_view.buttons.insert_menu.menu.popup_at_widget(
                        button,
                        gdk::Gravity::NorthWest,
                        gdk::Gravity::SouthWest,
                        None,
                    );
                });
        }

        v.show();

        (|_| {})(v); // I know, I'm a terrible person. I just didn't want
                     // another level of indentation.

        c.add_view(view);
    }

    #[inline]
    pub fn get_file(&self, index: usize) -> Option<MutexGuard<OpenFile>> {
        self.files.get(index).map(|of| of.lock().unwrap())
    }
}

impl Icons {
    pub fn new(size: i32) -> Self {
        Self {
            str_type:    Pixbuf::new_from_file_at_size(
                "img/str.svg",
                size,
                size,
            ).unwrap(),
            int_type:    Pixbuf::new_from_file_at_size(
                "img/int.svg",
                size,
                size,
            ).unwrap(),
            float_type:  Pixbuf::new_from_file_at_size(
                "img/float.svg",
                size,
                size,
            ).unwrap(),
            vector_type: Pixbuf::new_from_file_at_size(
                "img/vector.svg",
                size,
                size,
            ).unwrap(),
            img_type:    Pixbuf::new_from_file_at_size(
                "img/img.svg",
                size,
                size,
            ).unwrap(),

            audio_type: Pixbuf::new_from_file_at_size(
                "img/audio.svg",
                size,
                size,
            ).unwrap(),
        }
    }

    #[inline]
    pub fn get(&self, ntype: NodeType) -> Option<&Pixbuf> {
        match ntype {
            NodeType::Empty => None,
            NodeType::Str => Some(&self.str_type),
            NodeType::Int => Some(&self.int_type),
            NodeType::Float => Some(&self.float_type),
            NodeType::Vector => Some(&self.vector_type),
            NodeType::Img => Some(&self.img_type),
            NodeType::Audio => Some(&self.audio_type),
        }
    }
}

impl ::std::ops::Index<NodeType> for Icons {
    type Output = Pixbuf;

    #[inline]
    fn index(&self, index: NodeType) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl OpenFile {
    #[inline]
    pub fn new<S: Into<Option<gtk::TreeIter>>>(
        nx_file: nx::File,
        curr_selection: S,
    ) -> Self {
        Self {
            nx_file,
            curr_selection: curr_selection.into(),
            name_buffer_dirty: false,
            val_buffer_dirty: false,
            diff: FileDiff::new(),
        }
    }

    #[inline]
    pub fn nx_file(&self) -> &nx::File {
        &self.nx_file
    }

    #[inline]
    pub fn set_curr_selection(&mut self, iter: gtk::TreeIter) {
        self.curr_selection = Some(iter);
    }

    #[inline]
    pub fn record_name(&mut self, path: Vec<i32>, name: String) {
        self.diff
            .add_modification(path, ForwardNodeDiff::NameChange(name));
    }

    pub fn record_val_from_str(
        &mut self,
        ntype: NodeType,
        path: Vec<i32>,
        string: &str,
    ) -> Result<Option<String>, Error> {
        match ntype {
            NodeType::Str => self.diff.add_modification(
                path,
                ForwardNodeDiff::ValueChange(NodeValue::Str(
                    string.to_owned(),
                )),
            ),
            NodeType::Int => {
                let trimmed = string.trim();
                self.diff.add_modification(
                    path,
                    ForwardNodeDiff::ValueChange(NodeValue::Int(
                        trimmed.parse()?
                    )),
                );
                if trimmed != string {
                    return Ok(Some(trimmed.to_owned()));
                }
            },
            NodeType::Float => {
                let trimmed = string.trim();
                self.diff.add_modification(
                    path,
                    ForwardNodeDiff::ValueChange(NodeValue::Float(
                        trimmed.parse()?
                    )),
                );
                if trimmed != string {
                    return Ok(Some(trimmed.to_owned()));
                }
            },
            NodeType::Vector => {
                let (x, y) = parse_vector(&string)?;
                self.diff.add_modification(
                    path,
                    ForwardNodeDiff::ValueChange(NodeValue::Vector(x, y)),
                );
                return Ok(Some(format!("[{}, {}]", x, y)));
            },
            nt =>
                return Err(Error::LogicError(format!(
                    "cannot parse {:?} from string",
                    nt
                ))),
        }

        Ok(None)
    }

    pub fn write_to_file(
        &self,
        window: &gtk::ApplicationWindow,
    ) -> Result<Option<path::PathBuf>, Error> {
        let file_dialog = gtk::FileChooserDialog::with_buttons(
            Some("save as *.nx file"),
            Some(window),
            gtk::FileChooserAction::Save,
            &[
                ("save as", gtk::ResponseType::Accept),
                ("cancel", gtk::ResponseType::Cancel),
            ],
        );
        let file_filter = gtk::FileFilter::new();
        FileFilterExt::set_name(&file_filter, "NX files");
        file_filter.add_pattern("*.nx");
        file_dialog.add_filter(&file_filter);

        let dialog_res: gtk::ResponseType = file_dialog.run().into();
        match dialog_res {
            gtk::ResponseType::Accept => if let Some(file) =
                file_dialog.get_file()
            {
                let path = file.get_path().ok_or_else(|| {
                    Error::Gio("gio::File has no path".to_owned())
                })?;

                file_dialog.destroy();

                if path.extension().and_then(|os| os.to_str()) == Some("nx") {
                    let md = gtk::MessageDialog::new(
                        Some(window),
                        gtk::DialogFlags::from_bits(0b11).unwrap(),
                        gtk::MessageType::Info,
                        gtk::ButtonsType::None,
                        &format!(
                            "saving to {:?}, please wait (this may take a \
                             while)...",
                            path
                        ),
                    );
                    md.set_title("saving, please wait");
                    md.show_now();

                    let mut f = File::create(&path)?;
                    let mut f_buf =
                        BufWriter::with_capacity(64 * 1024, &mut f);
                    self.write(&mut f_buf)?;
                    f_buf.into_inner()?.sync_all()?;

                    md.destroy();

                    Ok(Some(path))
                } else {
                    Err(Error::FileChooser(
                        r#"filename doesn't match "*.nx""#.to_owned(),
                    ))
                }
            } else {
                Err(Error::FileChooser("no filename was chosen".to_owned()))
            },
            gtk::ResponseType::DeleteEvent => return Ok(None),
            _ => {
                file_dialog.destroy();

                Ok(None)
            },
        }
    }

    fn write<O: Write + Seek>(&self, out: &mut O) -> Result<(), Error> {
        // ================ Header ================ \\
        out.write_all(b"PKG4")?;

        let dummy_offsets = [0u8; (4 + 8) * 4];
        out.write_all(&dummy_offsets)?;
        // !!!!!!!!!!!!!!!! End header !!!!!!!!!!!!!!!! \\

        let mut offset = 52u64;
        let node_block_offset = offset;
        let mut node_count = 0u32;

        let orig_root = self.nx_file.root();

        let mut strings =
            Vec::with_capacity(self.nx_file.string_count() as usize);
        let mut string_id_map: HashMap<&str, _, _> =
            HashMap::with_capacity_and_hasher(
                self.nx_file.string_count() as usize,
                FxBuildHasher::default(),
            );
        let mut bitmaps =
            Vec::with_capacity(self.nx_file.bitmap_count() as usize);
        let mut audios =
            Vec::with_capacity(self.nx_file.audio_count() as usize);

        let breadth_iter = NxBreadthPathIter::new(&orig_root);
        let mut child_id = 1u32;
        let mut string_id = 0u32;
        let mut bitmap_id = 0u32;
        let mut audio_id = 0u32;

        // ================ Node data ================ \\
        for _ in 0..1 {
            let n = orig_root; // First write the root node, which is not part
                               // of the iterator.
            let p = vec![0];

            let modified = self.diff.modifications.get(&p);
            if modified.map(|cn| cn.deleted).unwrap_or(false) {
                continue;
            }

            let mut node_buf = [0u8; 20];
            let mut node_cur = Cursor::new(node_buf.as_mut());

            let name = modified
                .and_then(|cn| cn.name())
                .map(|name| name.as_str())
                .unwrap_or_else(|| n.name());
            let mapped_string_id =
                string_id_map.entry(name).or_insert_with(|| {
                    strings.push(name);
                    let s_id = string_id;
                    string_id += 1;
                    s_id
                });
            node_cur.write_u32::<LittleEndian>(*mapped_string_id)?; // Name

            node_cur.write_u32::<LittleEndian>(child_id)?; // First child ID

            let child_count = n.iter().count();
            child_id += child_count as u32;
            // Child count
            node_cur.write_u16::<LittleEndian>(child_count as u16)?;

            // Data
            if let Some(nv) = modified.and_then(|cn| cn.val()) {
                match nv {
                    NodeValue::Empty => {
                        node_cur.write_all(&[0u8; 10])?;
                    },
                    NodeValue::Int(i) => {
                        node_cur.write_u16::<LittleEndian>(1)?;
                        node_cur.write_i64::<LittleEndian>(*i)?;
                    },
                    NodeValue::Float(f) => {
                        node_cur.write_u16::<LittleEndian>(2)?;
                        node_cur.write_f64::<LittleEndian>(*f)?;
                    },
                    NodeValue::Str(s) => {
                        node_cur.write_u16::<LittleEndian>(3)?;
                        node_cur.write_u32::<LittleEndian>(string_id)?;
                        node_cur.write_all(&[0u8; 4])?;

                        strings.push(s.as_str());
                        string_id += 1;
                    },
                    NodeValue::Vector(x, y) => {
                        node_cur.write_u16::<LittleEndian>(4)?;
                        node_cur.write_i32::<LittleEndian>(*x)?;
                        node_cur.write_i32::<LittleEndian>(*y)?;
                    },
                    NodeValue::Img(i) => {
                        node_cur.write_u16::<LittleEndian>(5)?;
                        node_cur.write_u32::<LittleEndian>(bitmap_id)?;
                        node_cur
                            .write_u16::<LittleEndian>(i.get_width() as u16)?;
                        node_cur
                            .write_u16::<LittleEndian>(i.get_height() as u16)?;

                        bitmaps.push(Image::compress_pixbuf(i)?);
                        bitmap_id += 1;
                    },
                    NodeValue::Audio(_) => unimplemented!(),
                }
            } else {
                match n.dtype() {
                    nx::Type::Empty => {
                        node_cur.write_all(&[0u8; 10])?;
                    },
                    nx::Type::Integer => {
                        node_cur.write_u16::<LittleEndian>(1)?;
                        node_cur
                            .write_i64::<LittleEndian>(n.integer().unwrap())?;
                    },
                    nx::Type::Float => {
                        node_cur.write_u16::<LittleEndian>(2)?;
                        node_cur
                            .write_f64::<LittleEndian>(n.float().unwrap())?;
                    },
                    nx::Type::String => {
                        node_cur.write_u16::<LittleEndian>(3)?;
                        node_cur.write_u32::<LittleEndian>(string_id)?;
                        node_cur.write_all(&[0u8; 4])?;

                        strings.push(n.string().unwrap());
                        string_id += 1;
                    },
                    nx::Type::Vector => {
                        node_cur.write_u16::<LittleEndian>(4)?;
                        let (x, y) = n.vector().unwrap();
                        node_cur.write_i32::<LittleEndian>(x)?;
                        node_cur.write_i32::<LittleEndian>(y)?;
                    },
                    nx::Type::Bitmap => {
                        node_cur.write_u16::<LittleEndian>(5)?;
                        node_cur.write_u32::<LittleEndian>(bitmap_id)?;
                        let bm = n.bitmap().unwrap();
                        node_cur.write_u16::<LittleEndian>(bm.width())?;
                        node_cur.write_u16::<LittleEndian>(bm.height())?;

                        bitmaps.push(Image::NxBitmap(bm));
                        bitmap_id += 1;
                    },
                    nx::Type::Audio => {
                        node_cur.write_u16::<LittleEndian>(6)?;
                        node_cur.write_u32::<LittleEndian>(audio_id)?;
                        let a = n.audio().unwrap();
                        node_cur
                            .write_u32::<LittleEndian>(a.data().len() as u32)?;

                        audios.push(a);
                        audio_id += 1;
                    },
                }
            }

            out.write_all(&node_buf)?;
            offset += 20;
            node_count += 1;
        }
        for (n, p) in breadth_iter {
            let modified = self.diff.modifications.get(&p);
            if modified.map(|cn| cn.deleted).unwrap_or(false) {
                continue;
            }

            let mut node_buf = [0u8; 20];
            let mut node_cur = Cursor::new(node_buf.as_mut());

            let name = modified
                .and_then(|cn| cn.name())
                .map(|name| name.as_str())
                .unwrap_or_else(|| n.name());
            let mapped_string_id =
                string_id_map.entry(name).or_insert_with(|| {
                    strings.push(name);
                    let s_id = string_id;
                    string_id += 1;
                    s_id
                });
            node_cur.write_u32::<LittleEndian>(*mapped_string_id)?; // Name

            node_cur.write_u32::<LittleEndian>(child_id)?; // First child ID

            let child_count = n.iter().count();
            child_id += child_count as u32;
            // Child count
            node_cur.write_u16::<LittleEndian>(child_count as u16)?;

            // Data
            if let Some(nv) = modified.and_then(|cn| cn.val()) {
                match nv {
                    NodeValue::Empty => {
                        node_cur.write_all(&[0u8; 10])?;
                    },
                    NodeValue::Int(i) => {
                        node_cur.write_u16::<LittleEndian>(1)?;
                        node_cur.write_i64::<LittleEndian>(*i)?;
                    },
                    NodeValue::Float(f) => {
                        node_cur.write_u16::<LittleEndian>(2)?;
                        node_cur.write_f64::<LittleEndian>(*f)?;
                    },
                    NodeValue::Str(s) => {
                        node_cur.write_u16::<LittleEndian>(3)?;
                        node_cur.write_u32::<LittleEndian>(string_id)?;
                        node_cur.write_all(&[0u8; 4])?;

                        strings.push(s.as_str());
                        string_id += 1;
                    },
                    NodeValue::Vector(x, y) => {
                        node_cur.write_u16::<LittleEndian>(4)?;
                        node_cur.write_i32::<LittleEndian>(*x)?;
                        node_cur.write_i32::<LittleEndian>(*y)?;
                    },
                    NodeValue::Img(i) => {
                        node_cur.write_u16::<LittleEndian>(5)?;
                        node_cur.write_u32::<LittleEndian>(bitmap_id)?;
                        node_cur
                            .write_u16::<LittleEndian>(i.get_width() as u16)?;
                        node_cur
                            .write_u16::<LittleEndian>(i.get_height() as u16)?;

                        bitmaps.push(Image::compress_pixbuf(i)?);
                        bitmap_id += 1;
                    },
                    NodeValue::Audio(_) => unimplemented!(),
                }
            } else {
                match n.dtype() {
                    nx::Type::Empty => {
                        node_cur.write_all(&[0u8; 10])?;
                    },
                    nx::Type::Integer => {
                        node_cur.write_u16::<LittleEndian>(1)?;
                        node_cur
                            .write_i64::<LittleEndian>(n.integer().unwrap())?;
                    },
                    nx::Type::Float => {
                        node_cur.write_u16::<LittleEndian>(2)?;
                        node_cur
                            .write_f64::<LittleEndian>(n.float().unwrap())?;
                    },
                    nx::Type::String => {
                        node_cur.write_u16::<LittleEndian>(3)?;
                        node_cur.write_u32::<LittleEndian>(string_id)?;
                        node_cur.write_all(&[0u8; 4])?;

                        strings.push(n.string().unwrap());
                        string_id += 1;
                    },
                    nx::Type::Vector => {
                        node_cur.write_u16::<LittleEndian>(4)?;
                        let (x, y) = n.vector().unwrap();
                        node_cur.write_i32::<LittleEndian>(x)?;
                        node_cur.write_i32::<LittleEndian>(y)?;
                    },
                    nx::Type::Bitmap => {
                        node_cur.write_u16::<LittleEndian>(5)?;
                        node_cur.write_u32::<LittleEndian>(bitmap_id)?;
                        let bm = n.bitmap().unwrap();
                        node_cur.write_u16::<LittleEndian>(bm.width())?;
                        node_cur.write_u16::<LittleEndian>(bm.height())?;

                        bitmaps.push(Image::NxBitmap(bm));
                        bitmap_id += 1;
                    },
                    nx::Type::Audio => {
                        node_cur.write_u16::<LittleEndian>(6)?;
                        node_cur.write_u32::<LittleEndian>(audio_id)?;
                        let a = n.audio().unwrap();
                        node_cur
                            .write_u32::<LittleEndian>(a.data().len() as u32)?;

                        audios.push(a);
                        audio_id += 1;
                    },
                }
            }

            out.write_all(&node_buf)?;
            offset += 20;
            node_count += 1;
        }
        // !!!!!!!!!!!!!!!! End node data !!!!!!!!!!!!!!!! \\

        if offset % 8 != 0 {
            // Align string offset table to 8 bytes.
            out.write_all(&[0u8; 4])?; // We know it's already aligned to 4.
            offset += 4;
        }
        debug_assert_eq!(offset % 8, 0);

        let string_count = strings.len() as u32;
        let string_offset_table_offset = offset;
        let mut string_data_offset = offset + 8 * strings.len() as u64;

        // ================ String offset table ================ \\
        for s in strings.iter() {
            out.write_u64::<LittleEndian>(string_data_offset)?;
            let s_len = 2 + s.len() as u64;
            string_data_offset += s_len + if s_len % 2 == 0 { 0 } else { 1 };
            debug_assert_eq!(string_data_offset % 2, 0);
        }
        // !!!!!!!!!!!!!!!! End string offset table !!!!!!!!!!!!!!!! \\

        // ================ String data ================ \\
        for s in strings {
            let s_len = s.len();
            let mut str_buf = Vec::with_capacity(3 + s_len);
            let mut str_cur = Cursor::new(&mut str_buf);

            str_cur.write_u16::<LittleEndian>(s_len as u16)?;
            str_cur.write_all(s.as_bytes())?;
            if s_len % 2 != 0 {
                str_cur.write_all(&[0u8])?;
            }

            debug_assert_eq!(str_buf.len() % 2, 0);
            out.write_all(&str_buf)?;
        }
        offset = string_data_offset;
        debug_assert_eq!(offset % 2, 0);
        // !!!!!!!!!!!!!!!! End string data !!!!!!!!!!!!!!!! \\

        match offset % 8 {
            // Align bitmap offset table to 8 bytes.
            0 => (),
            2 => {
                out.write_all(&[0u8; 6])?;
                offset += 6;
            },
            4 => {
                out.write_all(&[0u8; 4])?;
                offset += 4;
            },
            6 => {
                out.write_all(&[0u8; 2])?;
                offset += 2;
            },
            _ => unreachable!(), // We already know we're aligned at 2 bytes.
        }
        debug_assert_eq!(offset % 8, 0);

        let bitmap_count = bitmaps.len() as u32;
        let bitmap_offset_table_offset = offset;
        let mut bitmap_data_offset = offset + 8 * bitmaps.len() as u64;

        // ================ Bitmap offset table ================ \\
        for bm in bitmaps.iter() {
            out.write_u64::<LittleEndian>(bitmap_data_offset)?;
            let bm_len = 4 + bm.raw_data().len() as u64;
            bitmap_data_offset +=
                bm_len + if bm_len % 8 == 0 { 0 } else { 8 - bm_len % 8 };
            debug_assert_eq!(bitmap_data_offset % 8, 0);
        }
        // !!!!!!!!!!!!!!!! End bitmap offset table !!!!!!!!!!!!!!!! \\

        // ================ Bitmap data ================ \\
        for bm in bitmaps {
            let bm_len = bm.raw_data().len();
            let mut bm_buf = Vec::with_capacity(11 + bm_len);
            let mut bm_cur = Cursor::new(&mut bm_buf);

            bm_cur.write_u32::<LittleEndian>(bm_len as u32)?;
            bm_cur.write_all(bm.raw_data())?;
            let bm_total_len = bm_len + 4;
            if bm_total_len % 8 != 0 {
                let pad_buf = [0u8; 7];
                bm_cur.write_all(&pad_buf[0..8 - bm_total_len % 8])?;
            }

            debug_assert_eq!(bm_buf.len() % 8, 0);
            out.write_all(&bm_buf)?;
        }
        offset = bitmap_data_offset;
        debug_assert_eq!(offset % 8, 0);
        // !!!!!!!!!!!!!!!! End bitmap data !!!!!!!!!!!!!!!! \\

        let audio_count = audios.len() as u32;
        let audio_offset_table_offset = offset;
        let mut audio_data_offset = offset + 8 * audios.len() as u64;

        // ================ Audio offset table ================ \\
        for a in audios.iter() {
            out.write_u64::<LittleEndian>(audio_data_offset)?;
            let a_len = 82 + a.data().len() as u64; // 82 bytes of WZ header.
            audio_data_offset +=
                a_len + if a_len % 8 == 0 { 0 } else { 8 - a_len % 8 };
            debug_assert_eq!(audio_data_offset % 8, 0);
        }
        // !!!!!!!!!!!!!!!! End audio offset table !!!!!!!!!!!!!!!! \\

        // ================ Audio data ================ \\
        for a in audios {
            let a_len = 82 + a.data().len() as usize;
            let padding = if a_len % 8 == 0 { 0 } else { 8 - a_len % 8 };
            let mut a_buf = Vec::with_capacity(a_len + padding);
            let mut a_cur = Cursor::new(&mut a_buf);

            a_cur.write_all(a.header())?;
            a_cur.write_all(a.data())?;
            if padding != 0 {
                let pad_buf = [0u8; 7];
                a_cur.write_all(&pad_buf[0..padding])?;
            }

            debug_assert_eq!(a_buf.len() % 8, 0);
            out.write_all(&a_buf)?;
        }
        //offset = audio_data_offset;
        debug_assert_eq!(audio_data_offset % 8, 0);
        // !!!!!!!!!!!!!!!! End audio data !!!!!!!!!!!!!!!! \\

        // ================ Rest of the header ================ \\
        out.seek(SeekFrom::Start(4))?;

        out.write_u32::<LittleEndian>(node_count)?;
        out.write_u64::<LittleEndian>(node_block_offset)?;
        out.write_u32::<LittleEndian>(string_count)?;
        out.write_u64::<LittleEndian>(string_offset_table_offset)?;
        out.write_u32::<LittleEndian>(bitmap_count)?;
        out.write_u64::<LittleEndian>(bitmap_offset_table_offset)?;
        out.write_u32::<LittleEndian>(audio_count)?;
        out.write_u64::<LittleEndian>(audio_offset_table_offset)?;
        // !!!!!!!!!!!!!!!! End rest of the header !!!!!!!!!!!!!!!! \\

        Ok(())
    }
}

impl FileDiff {
    #[inline]
    pub fn new() -> Self {
        Self {
            modifications: Map::default(),
            history:       Vec::new(),
        }
    }

    pub fn add_modification(&mut self, path: Vec<i32>, diff: ForwardNodeDiff) {
        let full_diff = match diff {
            ForwardNodeDiff::AddChild(child) => {
                if let Some(path_state) = self.modifications.get_mut(&path) {
                    path_state.children.push(child.clone());
                } else {
                    self.modifications.insert(
                        path.clone(),
                        ChangedNode::new().with_child(child.clone()),
                    );
                }

                NodeDiff::AddChild(child)
            },
            ForwardNodeDiff::Delete => {
                if let Some(path_state) = self.modifications.get_mut(&path) {
                    path_state.delete();
                } else {
                    self.modifications
                        .insert(path.clone(), ChangedNode::new().deleted());
                }

                NodeDiff::Delete
            },
            ForwardNodeDiff::NameChange(new_name) => {
                let old_name = if let Some(path_state) =
                    self.modifications.get_mut(&path)
                {
                    path_state.set_name(new_name.clone())
                } else {
                    self.modifications.insert(
                        path.clone(),
                        ChangedNode::new().with_name(new_name.clone()),
                    );

                    None
                };

                NodeDiff::NameChange(old_name, new_name)
            },
            ForwardNodeDiff::ValueChange(new_val) => {
                let old_val = if let Some(path_state) =
                    self.modifications.get_mut(&path)
                {
                    path_state.set_val(new_val.clone())
                } else {
                    self.modifications.insert(
                        path.clone(),
                        ChangedNode::new().with_val(new_val.clone()),
                    );

                    None
                };

                NodeDiff::ValueChange(old_val, new_val)
            },
        };

        self.history.push((path, full_diff));

        /*
        println!(
            "FileDiff::add_modification: {:?} | {:?}",
            self.modifications, self.history
        );
        */
    }

    #[inline]
    pub fn get_modified(&self, path: &[i32]) -> Option<&ChangedNode> {
        self.modifications.get(path)
    }
}

impl ChangedNode {
    #[inline]
    pub fn new() -> Self {
        Self {
            name:     None,
            val:      None,
            children: Vec::new(),
            deleted:  false,
        }
    }

    #[inline]
    pub fn name(&self) -> Option<&String> {
        self.name.as_ref()
    }

    #[inline]
    pub fn val(&self) -> Option<&NodeValue> {
        self.val.as_ref()
    }

    #[inline]
    pub fn children(&self) -> &Vec<NewNode> {
        &self.children
    }

    #[inline]
    pub fn with_name(self, name: String) -> Self {
        Self {
            name: Some(name),
            ..self
        }
    }

    #[inline]
    pub fn with_val(self, val: NodeValue) -> Self {
        Self {
            val: Some(val),
            ..self
        }
    }

    #[inline]
    pub fn with_child(self, child: NewNode) -> Self {
        Self {
            children: vec![child],
            ..self
        }
    }

    #[inline]
    pub fn deleted(self) -> Self {
        Self {
            deleted: true,
            ..self
        }
    }

    #[inline]
    pub fn delete(&mut self) {
        self.deleted = true;
    }

    #[inline]
    pub fn undelete(&mut self) {
        self.deleted = false;
    }

    #[inline]
    pub fn set_name(&mut self, name: String) -> Option<String> {
        if self.name.is_none() {
            self.name = Some(name);

            return None;
        }

        self.name.as_mut().map(move |n| mem::replace(n, name))
    }

    #[inline]
    pub fn set_val(&mut self, val: NodeValue) -> Option<NodeValue> {
        if self.val.is_none() {
            self.val = Some(val);

            return None;
        }

        self.val.as_mut().map(move |v| mem::replace(v, val))
    }
}

impl NodeValue {
    #[inline]
    pub fn get_type(&self) -> NodeType {
        match self {
            NodeValue::Empty => NodeType::Empty,
            NodeValue::Str(_) => NodeType::Str,
            NodeValue::Int(_) => NodeType::Int,
            NodeValue::Float(_) => NodeType::Float,
            NodeValue::Vector(_, _) => NodeType::Vector,
            NodeValue::Img(_) => NodeType::Img,
            NodeValue::Audio(_) => NodeType::Audio,
        }
    }
}

impl NodeType {
    #[inline]
    pub fn display_str(&self) -> &'static str {
        match self {
            NodeType::Empty => "",
            NodeType::Str => "<string>",
            NodeType::Int => "<integer>",
            NodeType::Float => "<float>",
            NodeType::Vector => "<vector>",
            NodeType::Img => "<image>",
            NodeType::Audio => "<audio>",
        }
    }
}

impl Into<u8> for NodeType {
    #[inline]
    fn into(self) -> u8 {
        self as u8
    }
}

impl From<u8> for NodeType {
    #[inline]
    fn from(tag: u8) -> Self {
        unsafe { ::std::mem::transmute(tag) }
    }
}

impl fmt::Display for NodeType {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.display_str())
    }
}

impl<'a> Image<'a> {
    pub fn compress_pixbuf(pb: &Pixbuf) -> Result<Self, Error> {
        if pb.get_colorspace() != Colorspace::Rgb {
            return Err(Error::ImgError(format!(
                "expected Pixbuf with a colorspace of Rgb, instead got {:?}",
                pb.get_colorspace()
            )));
        }
        if pb.get_bits_per_sample() != 8 {
            return Err(Error::ImgError(format!(
                "converting Pixbufs with bits-per-sample other than 8 is not \
                 implemented (this Pixbuf has {} bps)",
                pb.get_bits_per_sample()
            )));
        }

        let bytes = unsafe { pb.get_pixels() };

        let bgra_buf = if pb.get_has_alpha() {
            if bytes.len() % 4 != 0 {
                return Err(Error::ImgError(format!(
                    "pixbuf_bytes.len() == {}; {} % 4 != 0",
                    bytes.len(),
                    bytes.len(),
                )));
            }

            let mut bgra_buf = Vec::with_capacity(bytes.len());
            unsafe {
                bgra_buf.set_len(bgra_buf.capacity());
            }

            let mut i = 0;
            while i < bytes.len() {
                bgra_buf[i] = bytes[i + 2];
                bgra_buf[i + 1] = bytes[i + 1];
                bgra_buf[i + 2] = bytes[i];
                bgra_buf[i + 3] = bytes[i + 3];

                i += 4;
            }

            bgra_buf
        } else {
            if bytes.len() % 3 != 0 {
                return Err(Error::ImgError(format!(
                    "pixbuf_bytes.len() == {}; {} % 3 != 0",
                    bytes.len(),
                    bytes.len(),
                )));
            }

            let mut bgra_buf = Vec::with_capacity(bytes.len() / 3 * 4);
            unsafe {
                bgra_buf.set_len(bgra_buf.capacity());
            }

            let (mut i, mut j) = (0, 0);
            while i < bytes.len() {
                bgra_buf[j] = bytes[i + 2];
                bgra_buf[j + 1] = bytes[i + 1];
                bgra_buf[j + 2] = bytes[i];
                bgra_buf[j + 3] = 0xFF;

                i += 3;
                j += 4;
            }

            bgra_buf
        };

        let mut compressed = Vec::with_capacity(bgra_buf.len());

        compress_buf(&bgra_buf, &mut compressed)?;
        Ok(Image::Compressed(compressed))
    }

    /// LZ4-compressed BGRA8888 data.
    #[inline]
    pub fn raw_data(&self) -> &[u8] {
        match self {
            Image::NxBitmap(bm) => bm.raw_data(),
            Image::Compressed(c) => c,
        }
    }
}

pub fn new_node_onto_tree_store(
    new_node: &NewNode,
    store: &gtk::TreeStore,
    parent: Option<&gtk::TreeIter>,
    icons: &Icons,
) -> gtk::TreeIter {
    let node_name = &new_node.name;
    let tag: u8 = new_node.val.get_type().into();

    match new_node.val {
        NodeValue::Empty =>
            store.insert_with_values(parent, None, &[0, 4], &[node_name, &tag]),
        NodeValue::Int(ref i) => store.insert_with_values(
            parent,
            None,
            &[0, 1, 2, 4],
            &[node_name, &icons.int_type, i, &tag],
        ),
        NodeValue::Float(ref f) => store.insert_with_values(
            parent,
            None,
            &[0, 1, 2, 4],
            &[node_name, &icons.float_type, f, &tag],
        ),
        NodeValue::Str(ref s) => store.insert_with_values(
            parent,
            None,
            &[0, 1, 2, 4],
            &[node_name, &icons.str_type, s, &tag],
        ),
        NodeValue::Vector(ref x, ref y) => store.insert_with_values(
            parent,
            None,
            &[0, 1, 2, 4],
            &[
                node_name,
                &icons.vector_type,
                &format!("[{}, {}]", x, y),
                &tag,
            ],
        ),
        NodeValue::Img(ref i) => store.insert_with_values(
            parent,
            None,
            &[0, 1, 3, 4],
            &[node_name, &icons.img_type, i, &tag],
        ),
        NodeValue::Audio(_) =>
            unimplemented!("TODO: new_node_onto_tree_store Audio"),
    }
}

pub fn nx_onto_tree_store(
    node: &nx::Node,
    store: &gtk::TreeStore,
    parent: Option<&gtk::TreeIter>,
    icons: &Icons,
) -> gtk::TreeIter {
    let node_name = node.name();

    match node.dtype() {
        nx::Type::Empty => {
            let tag: u8 = NodeType::Empty.into();
            store.insert_with_values(
                parent,
                None,
                &[0, 4],
                &[&node_name, &tag],
            )
        },
        nx::Type::Integer => {
            let tag: u8 = NodeType::Int.into();
            store.insert_with_values(
                parent,
                None,
                &[0, 1, 2, 4],
                &[&node_name, &icons.int_type, &node.integer().unwrap(), &tag],
            )
        },
        nx::Type::Float => {
            let tag: u8 = NodeType::Float.into();
            store.insert_with_values(
                parent,
                None,
                &[0, 1, 2, 4],
                &[&node_name, &icons.float_type, &node.float().unwrap(), &tag],
            )
        },
        nx::Type::String => {
            let tag: u8 = NodeType::Str.into();
            store.insert_with_values(
                parent,
                None,
                &[0, 1, 2, 4],
                &[&node_name, &icons.str_type, &node.string().unwrap(), &tag],
            )
        },
        nx::Type::Vector => {
            let tag: u8 = NodeType::Vector.into();
            let (x, y) = node.vector().unwrap();
            store.insert_with_values(
                parent,
                None,
                &[0, 1, 2, 4],
                &[
                    &node_name,
                    &icons.vector_type,
                    &format!("[{}, {}]", x, y),
                    &tag,
                ],
            )
        },
        nx::Type::Bitmap => {
            let tag: u8 = NodeType::Img.into();
            store.insert_with_values(parent, None, &[0, 1, 3, 4], {
                let bitmap = node.bitmap().unwrap();
                let bitmap_len = bitmap.len() as usize;
                let mut vec = Vec::with_capacity(bitmap_len);
                unsafe {
                    vec.set_len(bitmap_len);
                }
                bitmap.data(&mut vec);

                // Convert from BGRA8888 to RGBA8888.
                vec.exact_chunks_mut(4).for_each(|bgra| bgra.swap(0, 2));

                let (width, height) =
                    (i32::from(bitmap.width()), i32::from(bitmap.height()));

                &[
                    &node_name,
                    &icons.img_type,
                    &Pixbuf::new_from_vec(
                        vec,
                        Colorspace::Rgb,
                        true,
                        8,
                        width,
                        height,
                        width * 4,
                    ),
                    &tag,
                ]
            })
        },
        nx::Type::Audio => {
            let tag: u8 = NodeType::Audio.into();
            store.insert_with_values(
                parent,
                None,
                &[0, 1, 2, 4],
                &[
                    &node_name,
                    &icons.audio_type,
                    &format!(
                        "[audio: {} bytes]",
                        node.audio().unwrap().data().len(),
                    ),
                    &tag,
                ],
            )
        },
    }
}

pub fn changed_nx_onto_tree_store<S: AsRef<str>>(
    node: &nx::Node,
    store: &gtk::TreeStore,
    parent: Option<&gtk::TreeIter>,
    icons: &Icons,
    replacement_name: Option<&S>,
    replacement_val: Option<&NodeValue>,
) -> gtk::TreeIter {
    let node_name =
        replacement_name.map_or_else(|| node.name(), |n| n.as_ref());

    match node.dtype() {
        nx::Type::Empty => {
            let tag: u8 = NodeType::Empty.into();
            store.insert_with_values(
                parent,
                None,
                &[0, 4],
                &[&node_name, &tag],
            )
        },
        nx::Type::Integer => {
            let tag: u8 = NodeType::Int.into();
            store.insert_with_values(
                parent,
                None,
                &[0, 1, 2, 4],
                &[
                    &node_name,
                    &icons.int_type,
                    &replacement_val.map_or_else(
                        || node.integer().unwrap(),
                        |r| match r {
                            NodeValue::Int(i) => *i,
                            _ => panic!("expected NodeValue::Int"),
                        },
                    ),
                    &tag,
                ],
            )
        },
        nx::Type::Float => {
            let tag: u8 = NodeType::Float.into();
            store.insert_with_values(
                parent,
                None,
                &[0, 1, 2, 4],
                &[
                    &node_name,
                    &icons.float_type,
                    &replacement_val.map_or_else(
                        || node.float().unwrap(),
                        |r| match r {
                            NodeValue::Float(f) => *f,
                            _ => panic!("expected NodeValue::Float"),
                        },
                    ),
                    &tag,
                ],
            )
        },
        nx::Type::String => {
            let tag: u8 = NodeType::Str.into();
            store.insert_with_values(
                parent,
                None,
                &[0, 1, 2, 4],
                &[
                    &node_name,
                    &icons.str_type,
                    &replacement_val.map_or_else(
                        || node.string().unwrap(),
                        |r| match r {
                            NodeValue::Str(s) => s,
                            _ => panic!("expected NodeValue::Str"),
                        },
                    ),
                    &tag,
                ],
            )
        },
        nx::Type::Vector => {
            let tag: u8 = NodeType::Vector.into();
            let (x, y) = replacement_val.map_or_else(
                || node.vector().unwrap(),
                |r| match r {
                    NodeValue::Vector(x, y) => (*x, *y),
                    _ => panic!("expected NodeValue::Vector"),
                },
            );
            store.insert_with_values(
                parent,
                None,
                &[0, 1, 2, 4],
                &[
                    &node_name,
                    &icons.vector_type,
                    &format!("[{}, {}]", x, y),
                    &tag,
                ],
            )
        },
        nx::Type::Bitmap => {
            let tag: u8 = NodeType::Img.into();

            if let Some(r) = replacement_val {
                match r {
                    NodeValue::Img(i) => store.insert_with_values(
                        parent,
                        None,
                        &[0, 1, 3, 4],
                        &[&node_name, &icons.img_type, i, &tag],
                    ),
                    _ => panic!("expected NodeValue::Img"),
                }
            } else {
                let bitmap = node.bitmap().unwrap();
                let bitmap_len = bitmap.len() as usize;
                let mut vec = Vec::with_capacity(bitmap_len);
                unsafe {
                    vec.set_len(bitmap_len);
                }
                bitmap.data(&mut vec);

                // Convert from BGRA8888 to RGBA8888.
                vec.exact_chunks_mut(4).for_each(|bgra| bgra.swap(0, 2));

                let (width, height) =
                    (i32::from(bitmap.width()), i32::from(bitmap.height()));

                store.insert_with_values(
                    parent,
                    None,
                    &[0, 1, 3, 4],
                    &[
                        &node_name,
                        &icons.img_type,
                        &Pixbuf::new_from_vec(
                            vec,
                            Colorspace::Rgb,
                            true,
                            8,
                            width,
                            height,
                            width * 4,
                        ),
                        &tag,
                    ],
                )
            }
        },
        nx::Type::Audio => {
            let tag: u8 = NodeType::Audio.into();
            store.insert_with_values(
                parent,
                None,
                &[0, 1, 2, 4],
                &[
                    &node_name,
                    &icons.audio_type,
                    &replacement_val.map_or_else(
                        || {
                            format!(
                                "[audio: {} bytes]",
                                node.audio().unwrap().data().len(),
                            )
                        },
                        |_| {
                            unimplemented!(
                                "TODO: changed_nx_onto_tree_store Audio"
                            )
                        },
                    ),
                    &tag,
                ],
            )
        },
    }
}

pub fn append_text_column(
    tree: &gtk::TreeView,
    col_ix: i32,
    window_width: u32,
    stamp: bool,
) {
    let column = gtk::TreeViewColumn::new();
    let cell = gtk::CellRendererText::new();

    cell.set_property_ellipsize(EllipsizeMode::None);
    cell.set_property_wrap_mode(WrapMode::Word);
    cell.set_property_wrap_width(get_wrap_width(window_width));
    if stamp {
        cell.set_property_family(Some("monospace"));
        cell.set_property_family_set(true);
        cell.set_property_size_points(8.0);
    }

    column.pack_start(&cell, true);
    column.add_attribute(&cell, "text", col_ix);
    tree.append_column(&column);
}

#[inline]
pub fn append_pixbuf_column(tree: &gtk::TreeView, col_ix: i32) {
    let column = gtk::TreeViewColumn::new();
    let cell = gtk::CellRendererPixbuf::new();

    // cell.set_property_... ;

    column.pack_start(&cell, true);
    column.add_attribute(&cell, "pixbuf", col_ix);
    tree.append_column(&column);
}

pub fn get_node_from_indices<'a>(
    root: nx::Node<'a>,
    indices: &[i32],
) -> Option<nx::Node<'a>> {
    if indices[0] != 0 {
        return None;
    }

    let mut node = root;
    for index in &indices[1..] {
        node = if let Some(n) = node.iter().nth(*index as usize) {
            n
        } else {
            return None;
        };
    }

    Some(node)
}

pub fn parse_vector(s: &str) -> Result<(i32, i32), Error> {
    let (mut seen_l, mut seen_comma, mut seen_r) = (false, false, false);
    let (mut digits_l, mut digits_r) =
        (String::with_capacity(11), String::with_capacity(11));
    let (mut seen_digits_l, mut seen_digits_r) = (false, false);

    for (i, ch) in s.chars().enumerate() {
        match ch {
            '[' => if seen_l || seen_comma || seen_r {
                return Err(Error::ParseVector(format!(
                    "unexpected '[' at character {} when parsing vector",
                    i
                )));
            } else {
                seen_l = true;
            },
            ',' => if !seen_l || seen_comma || seen_r {
                return Err(Error::ParseVector(format!(
                    "unexpected ',' at character {} when parsing vector",
                    i
                )));
            } else {
                seen_comma = true;
                if !seen_digits_l && !digits_l.is_empty() {
                    seen_digits_l = true;
                } else if !seen_digits_r && !digits_r.is_empty() {
                    seen_digits_r = true;
                }
            },
            ']' => if !seen_l || !seen_comma || seen_r {
                return Err(Error::ParseVector(format!(
                    "unexpected ']' at character {} when parsing vector",
                    i
                )));
            } else {
                seen_r = true;
                if !seen_digits_l && !digits_l.is_empty() {
                    seen_digits_l = true;
                } else if !seen_digits_r && !digits_r.is_empty() {
                    seen_digits_r = true;
                }
            },
            d if d.is_digit(10) || d == '-' => if !seen_l || seen_r {
                return Err(Error::ParseVector(format!(
                    "unexpected '{}' at character {} when parsing vector",
                    d, i
                )));
            } else if !seen_comma && !seen_digits_l {
                digits_l.push(d);
            } else if seen_comma && seen_digits_l && !seen_digits_r {
                digits_r.push(d);
            } else {
                return Err(Error::ParseVector(format!(
                    "unexpected '{}' at character {} when parsing vector",
                    d, i
                )));
            },
            ' ' | '\n' | '\r' => if !seen_digits_l && !digits_l.is_empty() {
                seen_digits_l = true;
            } else if !seen_digits_r && !digits_r.is_empty() {
                seen_digits_r = true;
            },
            c =>
                return Err(Error::ParseVector(format!(
                    "unexpected '{}' at character {} when parsing vector",
                    c, i
                ))),
        }
    }

    if !seen_r {
        Err(Error::ParseVector(
            "expected trailing ']' when parsing vector".to_owned(),
        ))
    } else {
        Ok((digits_l.parse()?, digits_r.parse()?))
    }
}
