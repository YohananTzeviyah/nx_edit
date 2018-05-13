use err::Error;
use fxhash::FxHashMap as Map;
use gdk;
use gdk_pixbuf::{Colorspace, Pixbuf};
use gtk::{
    self,
    prelude::*,
    CellLayoutExt,
    TreeStoreExtManual,
    TreeViewExt,
    WidgetExt,
};
use nx::{self, GenericNode};
use pango::{EllipsizeMode, WrapMode};
use std::{
    fmt,
    mem,
    sync::{Arc, Mutex, MutexGuard},
};
use ui::{
    get_wrap_width,
    run_msg_dialog,
    Content,
    NodeDisplay,
    NodeView,
    TreeView,
};

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
    pub modifications: Map<Vec<i32>, ChangedNode>,
    pub history:       Vec<(Vec<i32>, NodeDiff)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ChangedNode {
    pub name:     Option<String>,
    pub val:      Option<NodeValue>,
    pub siblings: Vec<NewNode>,
    pub children: Vec<NewNode>,
    pub deleted:  bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct NewNode {
    pub id:   usize,
    pub name: String,
    pub val:  NodeValue,
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeDiff {
    NameChange(Option<String>, String),
    ValueChange(Option<NodeValue>, NodeValue),
    AddSibling(NewNode),
    AddChild(NewNode),
    Delete,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ForwardNodeDiff {
    NameChange(String),
    ValueChange(NodeValue),
    AddSibling(NewNode),
    AddChild(NewNode),
    Delete,
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeValue {
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
    Empty,
    Str,
    Int,
    Float,
    Vector,
    Img,
    Audio,
}

impl AppState {
    pub fn new() -> Self {
        Self {
            open_files:   OpenFiles::new(),
            window_width: 0,
        }
    }
}

impl OpenFiles {
    pub fn new() -> Self {
        Self {
            files: Vec::with_capacity(2),
            icons: Arc::new(Icons::new(24)),
        }
    }

    pub fn new_file(
        &mut self,
        nf: nx::File,
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

        {
            let of = Arc::clone(&of);
            let icons = Arc::clone(&self.icons);
            tree_view.connect_test_expand_row(move |tv, titer, tpath| {
                let model_store: gtk::TreeStore = tv.get_model()
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

                let expanded_path = tpath.get_indices();
                let of = of.lock().unwrap();
                let expanded_node =
                    get_node_from_indices(of.nx_file().root(), &expanded_path)
                        .expect("bad row expansion");

                // Actual modifications happen here.
                for immed_child_node in expanded_node.iter() {
                    for sndry_child_node in immed_child_node.iter() {
                        nx_onto_tree_store(
                            &sndry_child_node,
                            &model_store,
                            Some(&immed_child_tree_iter),
                            &icons,
                        );
                    }

                    model_store.iter_next(&immed_child_tree_iter);
                }

                Inhibit(false)
            });
        }

        tree_view.connect_test_collapse_row(|tv, titer, _| {
            let model_store: gtk::TreeStore = tv.get_model()
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

        {
            let c = Arc::clone(&content);
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

                    let mut c = c.lock().unwrap();
                    if let Some(ref mut nv) = c.node_view {
                        nv.name_display
                            .get_buffer()
                            .unwrap()
                            .set_text(name.get().unwrap());

                        if let Some(text) = text_val.get::<&str>() {
                            let new_text_view =
                                nv.set_text(text, path.get_indices());

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
                            nv.set_img(
                                gtk::Image::new_from_pixbuf(&pixbuf),
                                path.get_indices(),
                            );
                        } else {
                            return;
                        }

                        nv.show();
                    }
                }
            });
        }

        let mut c = content.lock().unwrap();

        let node_view_struct = NodeView::new_empty(&c.main_box);

        // Monitor name text buffer for changes.
        {
            let of = Arc::clone(of);
            node_view_struct
                .name_display
                .get_buffer()
                .unwrap()
                .connect_changed(move |_| {
                    of.lock().unwrap().name_buffer_dirty = true;
                });
        }

        // Hook up NodeView buttons.
        {
            let content = Arc::clone(content);
            let of = Arc::clone(of);
            let w = window.clone();
            node_view_struct.buttons.record_button.connect_clicked(
                move |_| {
                    let c = content.lock().unwrap();
                    let (nv, tv) = if let (Some(nv), Some(tv)) =
                        (&c.node_view, &c.tree_view)
                    {
                        (nv, tv)
                    } else {
                        return;
                    };

                    let mut of = of.lock().unwrap();

                    let (name_edited, val_edited) =
                        (of.name_buffer_dirty, of.val_buffer_dirty);
                    if !name_edited && !val_edited {
                        return;
                    }

                    let model = tv.gtk_tree_view.get_model().unwrap();

                    let (path, ntype) = if let Some(ref curr_selection) =
                        of.curr_selection
                    {
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
                            nv.name_display.get_buffer().unwrap();
                        let new_name = name_buffer
                            .get_text(
                                &name_buffer.get_start_iter(),
                                &name_buffer.get_end_iter(),
                                true,
                            )
                            .expect("TextBufferExt::get_text failed");

                        if let Some(ref curr_selection) = of.curr_selection {
                            let store: gtk::TreeStore =
                                model.downcast()
                                    .expect(
                                        "failed to downcast gtk::TreeModel => \
                                         gtk::TreeStore"
                                    );

                            store.set(curr_selection, &[0], &[&new_name]);
                        } else {
                            return;
                        }

                        of.record_name(path, new_name);
                    } else if !name_edited && val_edited {
                        match nv.node_display {
                            NodeDisplay::Empty(_) =>
                                of.val_buffer_dirty = false,
                            NodeDisplay::Text(_, ref text_view) => {
                                let ntype: NodeType = ntype.into();

                                let text_buffer =
                                    text_view.get_buffer().unwrap();
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

                                if let Some(ref curr_selection) =
                                    of.curr_selection
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
                            nv.name_display.get_buffer().unwrap();
                        let new_name = name_buffer
                            .get_text(
                                &name_buffer.get_start_iter(),
                                &name_buffer.get_end_iter(),
                                true,
                            )
                            .expect("TextBufferExt::get_text failed");

                        match nv.node_display {
                            NodeDisplay::Empty(_) =>
                                of.val_buffer_dirty = false,
                            NodeDisplay::Text(_, ref text_view) => {
                                let ntype: NodeType = ntype.into();

                                let text_buffer =
                                    text_view.get_buffer().unwrap();
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

                                if let Some(ref curr_selection) =
                                    of.curr_selection
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
                },
            );
        }
        {
            let content = Arc::clone(content);
            node_view_struct.buttons.insert_button.connect_clicked(
                move |button| {
                    let c = content.lock().unwrap();
                    let nv = if let Some(ref nv) = c.node_view {
                        nv
                    } else {
                        return;
                    };

                    nv.buttons.insert_menu.menu.show_all();
                    nv.buttons.insert_menu.menu.popup_at_widget(
                        button,
                        gdk::Gravity::NorthWest,
                        gdk::Gravity::SouthWest,
                        None,
                    );
                },
            );
        }

        node_view_struct.show();
        c.node_view = Some(node_view_struct);

        let tree_view_struct = TreeView::new(&c.main_box, tree_view);
        tree_view_struct.scroll_win.show_all();
        c.tree_view = Some(tree_view_struct);
    }

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

    fn index(&self, index: NodeType) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl OpenFile {
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
                    ForwardNodeDiff::ValueChange(
                        NodeValue::Int(trimmed.parse()?),
                    ),
                );
                if trimmed != string {
                    return Ok(Some(trimmed.to_owned()));
                }
            },
            NodeType::Float => {
                let trimmed = string.trim();
                self.diff.add_modification(
                    path,
                    ForwardNodeDiff::ValueChange(
                        NodeValue::Float(trimmed.parse()?),
                    ),
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
}

impl FileDiff {
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
            ForwardNodeDiff::AddSibling(sibling) => {
                if let Some(path_state) = self.modifications.get_mut(&path) {
                    path_state.siblings.push(sibling.clone());
                } else {
                    self.modifications.insert(
                        path.clone(),
                        ChangedNode::new().with_sibling(sibling.clone()),
                    );
                }

                NodeDiff::AddSibling(sibling)
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

        println!(
            "FileDiff::add_modification: {:?} | {:?}",
            self.modifications, self.history
        );
    }
}

impl ChangedNode {
    pub fn new() -> Self {
        Self {
            name:     None,
            val:      None,
            children: Vec::new(),
            siblings: Vec::new(),
            deleted:  false,
        }
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
    pub fn with_sibling(self, sibling: NewNode) -> Self {
        Self {
            siblings: vec![sibling],
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
        self.name.as_mut().map(move |n| mem::replace(n, name))
    }

    #[inline]
    pub fn set_val(&mut self, val: NodeValue) -> Option<NodeValue> {
        self.val.as_mut().map(move |v| mem::replace(v, val))
    }
}

impl NodeType {
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
