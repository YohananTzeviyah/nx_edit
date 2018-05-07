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
use std::sync::{Arc, Mutex, MutexGuard};
use ui::{Content, TreeView};

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

    pub fn new_file(&mut self, nf: nx::File, content: &Mutex<Content>) {
        self.files.push(Arc::new(Mutex::new(nf)));
        let nf = &self.files[self.files.len() - 1];

        let nf_unwrapped = nf.lock().unwrap();
        let root = nf_unwrapped.root();
        let tree_store = match root.dtype() {
            nx::Type::Integer =>
                gtk::TreeStore::new(&[gtk::Type::String, gtk::Type::I64]),
            nx::Type::Float =>
                gtk::TreeStore::new(&[gtk::Type::String, gtk::Type::F64]),
            nx::Type::Vector => gtk::TreeStore::new(&[
                gtk::Type::String,
                gtk::Type::I32,
                gtk::Type::I32,
            ]),
            nx::Type::Bitmap | nx::Type::Audio => gtk::TreeStore::new(&[
                gtk::Type::String,
                gtk::Type::BaseObject,
            ]),
            _ => gtk::TreeStore::new(&[gtk::Type::String, gtk::Type::String]),
        };

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
                        println!("return Inhibit(true);");
                        return Inhibit(true);
                    };

                let expanded_path = tpath.get_indices();
                let f = f.lock().unwrap();
                let expanded_node =
                    get_node_from_indices(f.root(), &expanded_path)
                        .expect("Bad row expansion");

                // Unset model while modifying it, then reconnect after
                // modifications are made, to speed it up. TODO: Test if this
                // actually presents a speedup. UPDATE: Jk lol you can't do it
                // like this unless you like segfaults. :))))))) Gotta figure
                // out how to tell GTK that I want to hold an outstanding
                // reference to the model even after "unsetting" it.
                // tv.set_model::<_, Option<&gtk::TreeModel>>(None);

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

                // tv.set_model(&model_store);

                Inhibit(false)
            });
        }

        tree_view.connect_test_collapse_row(move |tv, titer, _| {
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

        let mut c = content.lock().unwrap();

        let tree_view_struct = TreeView::new(&c.main_box, tree_view);
        tree_view_struct.scroll_win.show_all();
        c.tree_view = Some(tree_view_struct);
    }

    pub fn get_file(&self, index: usize) -> Option<MutexGuard<nx::File>> {
        self.files
            .get(index)
            .map(|nf| nf.lock().unwrap())
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
            store.insert_with_values(parent, None, &[0, 1], &[&node_name, &""]),
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
                &[&node_name, &x, &y],
            )
        },
        nx::Type::Bitmap => store.insert_with_values(parent, None, &[0, 1], {
            let bitmap = node.bitmap().unwrap();
            let mut vec = Vec::with_capacity(bitmap.len() as usize);
            bitmap.data(&mut vec);

            // Convert from BGRA8888 to RGBA8888.
            vec.exact_chunks_mut(4)
                .for_each(|bgra| bgra.swap(0, 2));

            let (width, height) = (
                i32::from(bitmap.width()),
                i32::from(bitmap.height()),
            );
            let pixbuf = Pixbuf::new_from_vec(
                vec,
                Colorspace::Rgb,
                true,
                32,
                width,
                height,
                width * 4,
            );

            &[
                &node_name,
                &gtk::Image::new_from_pixbuf(&pixbuf),
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

    column.pack_start(&cell, true);
    column.add_attribute(&cell, "text", col_ix);
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
