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
use std::sync::{Arc, Mutex, MutexGuard};
use ui::{Content, NodeDisplay, NodeView, TreeView};

pub struct AppState {
    pub open_files: OpenFiles,
}

pub struct OpenFiles {
    files: Vec<Arc<Mutex<nx::File>>>,
}

impl AppState {
    pub fn new() -> Self {
        Self {
            open_files: OpenFiles::new(),
        }
    }
}

impl OpenFiles {
    pub fn new() -> Self {
        Self {
            files: Vec::with_capacity(2),
        }
    }

    pub fn new_file(&mut self, nf: nx::File, content: &Arc<Mutex<Content>>) {
        self.files.push(Arc::new(Mutex::new(nf)));
        let nf = &self.files[self.files.len() - 1];

        let nf_unwrapped = nf.lock().unwrap();
        let root = nf_unwrapped.root();
        let tree_store = gtk::TreeStore::new(&[
            gtk::Type::String,
            gtk::Type::String,
            Pixbuf::static_type(),
        ]);

        let root_iter = nx_onto_tree_store(&root, &tree_store, None);

        for n in root.iter() {
            nx_onto_tree_store(&n, &tree_store, Some(&root_iter));
        }

        let tree_view = gtk::TreeView::new_with_model(&tree_store);
        tree_view.set_halign(gtk::Align::Center);
        tree_view.set_valign(gtk::Align::Start);
        tree_view.set_property_expand(true);
        tree_view.set_headers_visible(true);
        tree_view.set_enable_tree_lines(true);
        tree_view.set_vscroll_policy(gtk::ScrollablePolicy::Natural);

        append_text_column(&tree_view, 0);
        append_text_column(&tree_view, 1);
        append_pixbuf_column(&tree_view, 2);

        {
            let f = Arc::clone(&nf);
            tree_view.connect_test_expand_row(move |tv, titer, tpath| {
                let model_store: gtk::TreeStore = tv.get_model()
                    .clone()
                    .expect("gtk::TreeView expected to have a gtk::TreeModel")
                    .downcast()
                    .expect(
                        "Failed to downcast gtk::TreeModel => gtk::TreeStore",
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
                let f = f.lock().unwrap();
                let expanded_node =
                    get_node_from_indices(f.root(), &expanded_path)
                        .expect("Bad row expansion");

                // Actual modifications happen here.
                for immed_child_node in expanded_node.iter() {
                    for sndry_child_node in immed_child_node.iter() {
                        nx_onto_tree_store(
                            &sndry_child_node,
                            &model_store,
                            Some(&immed_child_tree_iter),
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
                .expect("Failed to downcast gtk::TreeModel => gtk::TreeStore");

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
            tree_view.connect_cursor_changed(move |tv| {
                println!(
                    "cursor_changed: {:?}",
                    tv.get_cursor()
                        .0
                        .map(|cur| cur.get_indices())
                        .unwrap_or(Vec::new())
                );

                let path = if let (Some(p), _) = tv.get_cursor() {
                    p
                } else {
                    return;
                };
                let model =
                    tv.get_model().expect("No model for gtk::TreeView");
                if let Some(iter) = model.get_iter(&path) {
                    let (val0, val1, val2) = (
                        model.get_value(&iter, 0),
                        model.get_value(&iter, 1),
                        model.get_value(&iter, 2),
                    );
                    //println!("({:?}, {:?}, {:?})", val0, val1, val2);

                    let mut c = c.lock().unwrap();
                    if let Some(ref mut nv) = c.node_view {
                        if let Some(text) = val1.get::<&str>() {
                            nv.set_node_display(NodeDisplay::Label(
                                gtk::Label::new(text),
                            ));
                        } else if let Some(pixbuf) = val2.get::<Pixbuf>() {
                            nv.set_node_display(NodeDisplay::Image(
                                gtk::Image::new_from_pixbuf(&pixbuf),
                            ));
                        } else {
                            return;
                        }
                        nv.show();
                    }
                }
            });
        }
        //tree_view.connect_row_activated(|_, tpath, _| {
        //    println!("row_activated: {:?}", tpath.get_indices());
        //});

        let mut c = content.lock().unwrap();

        //
        let node_view_struct = NodeView::new(&c.main_box, None);
        node_view_struct.show();
        c.node_view = Some(node_view_struct);

        //
        let tree_view_struct = TreeView::new(&c.main_box, tree_view);
        tree_view_struct.scroll_win.show_all();
        c.tree_view = Some(tree_view_struct);
    }

    pub fn get_file(&self, index: usize) -> Option<MutexGuard<nx::File>> {
        self.files.get(index).map(|nf| nf.lock().unwrap())
    }
}

pub fn nx_onto_tree_store(
    node: &nx::Node,
    store: &gtk::TreeStore,
    parent: Option<&gtk::TreeIter>,
) -> gtk::TreeIter {
    let node_name = node.name();

    match node.dtype() {
        nx::Type::Empty =>
            store.insert_with_values(parent, None, &[0], &[&node_name]),
        nx::Type::Integer => store.insert_with_values(
            parent,
            None,
            &[0, 1],
            &[&node_name, &node.integer().unwrap()],
        ),
        nx::Type::Float => store.insert_with_values(
            parent,
            None,
            &[0, 1],
            &[&node_name, &node.float().unwrap()],
        ),
        nx::Type::String => store.insert_with_values(
            parent,
            None,
            &[0, 1],
            &[&node_name, &node.string().unwrap()],
        ),
        nx::Type::Vector => {
            let (x, y) = node.vector().unwrap();
            store.insert_with_values(
                parent,
                None,
                &[0, 1],
                &[&node_name, &format!("⟨{}, {}⟩", x, y)],
            )
        },
        nx::Type::Bitmap => store.insert_with_values(parent, None, &[0, 2], {
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
                &Pixbuf::new_from_vec(
                    vec,
                    Colorspace::Rgb,
                    true,
                    8,
                    width,
                    height,
                    width * 4,
                ),
            ]
        }),
        nx::Type::Audio => store.insert_with_values(
            parent,
            None,
            &[0, 1],
            &[
                &node_name,
                &format!(
                    "[Audio: {} bytes]",
                    node.audio().unwrap().data().len()
                ),
            ],
        ),
    }
}

pub fn append_text_column(tree: &gtk::TreeView, col_ix: i32) {
    let column = gtk::TreeViewColumn::new();
    let cell = gtk::CellRendererText::new();

    cell.set_property_ellipsize(EllipsizeMode::None);
    cell.set_property_wrap_mode(WrapMode::Word);
    cell.set_property_wrap_width(256);

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
