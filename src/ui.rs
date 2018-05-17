use err::Error;
use gdk_pixbuf::Pixbuf;
use gio::FileExt;
use gtk::{self, prelude::*};
use nx;
use state::*;
use std::sync::{Arc, Mutex};

pub struct App {
    pub state:  Arc<Mutex<AppState>>,
    pub window: Window,
}

pub struct Window {
    pub gtk_window: gtk::ApplicationWindow,
    pub toolbar:    Toolbar,
    pub content:    Arc<Mutex<Content>>,
}

pub struct Content {
    pub main_box:  gtk::Box,
    pub node_view: Option<NodeView>,
    pub tree_view: Option<TreeView>,
}

pub struct Toolbar {
    pub container:      gtk::HeaderBar,
    pub open_button:    gtk::Button,
    pub save_as_button: gtk::Button,
    pub close_button:   gtk::Button,
    pub about_button:   gtk::Button,
}

pub enum NodeDisplay {
    Empty(gtk::Label),
    Text(gtk::ScrolledWindow, gtk::TextView),
    Image(gtk::Image),
    Audio(u8), // TODO
}

pub struct NodeView {
    pub own_box:      gtk::Box,
    pub name_display: gtk::TextView,
    pub node_display: NodeDisplay,
    pub assoc_path:   Option<Vec<i32>>,
    pub buttons:      NodeViewButtons,
}

pub struct NodeViewButtons {
    pub own_box:       gtk::Box,
    pub record_button: gtk::Button,
    pub insert_button: gtk::Button,
    pub insert_menu:   InsertMenu,
    pub delete_button: gtk::Button,
    pub undo_button:   gtk::Button,
    pub redo_button:   gtk::Button,
}

pub struct TreeView {
    pub scroll_win:    gtk::ScrolledWindow,
    pub gtk_tree_view: gtk::TreeView,
}

pub struct InsertMenu {
    pub menu:        gtk::Menu,
    pub before_item: gtk::MenuItem,
    pub before_menu: TypeMenu,
    pub after_item:  gtk::MenuItem,
    pub after_menu:  TypeMenu,
    pub child_item:  gtk::MenuItem,
    pub child_menu:  TypeMenu,
}

pub struct TypeMenu {
    pub menu:        gtk::Menu,
    pub empty_item:  gtk::MenuItem,
    pub str_item:    gtk::MenuItem,
    pub int_item:    gtk::MenuItem,
    pub float_item:  gtk::MenuItem,
    pub vector_item: gtk::MenuItem,
    pub img_item:    gtk::MenuItem,
    pub audio_item:  gtk::MenuItem,
}

impl App {
    pub fn new(application: &gtk::Application) -> Result<Self, Error> {
        let state = Arc::new(Mutex::new(AppState::new()));
        let window = Window::new(application, &state)?;

        Ok(Self { state, window })
    }
}

impl Window {
    pub fn new(
        application: &gtk::Application,
        state: &Arc<Mutex<AppState>>,
    ) -> Result<Self, Error> {
        let gtk_window = gtk::ApplicationWindow::new(application);

        gtk::Window::set_default_icon(&Pixbuf::new_from_file(
            "img/nx_edit.svg",
        )?);
        gtk_window.set_title("nx_edit");
        gtk_window.set_position(gtk::WindowPosition::Center);
        gtk_window.set_default_size(1024, 576);

        {
            let w = gtk_window.clone();
            gtk_window.connect_delete_event(move |_, _| {
                w.destroy();
                Inhibit(false)
            });
        }

        let toolbar = Toolbar::new();
        gtk_window.set_titlebar(&toolbar.container);

        let content = Arc::new(Mutex::new(Content::new(&gtk_window)));

        // Hook up toolbar button actions here.
        {
            let s = Arc::clone(&state);
            let w = gtk_window.clone();
            let c = Arc::clone(&content);
            toolbar.open_button.connect_clicked(move |_| {
                let mut state = s.lock().unwrap();
                let window_width = state.window_width;
                // TODO: Figure out some kind of error handling, maybe
                // involving storing errors in `AppState`.
                let nx_file = if let Some(nf) = open_file(&w).unwrap() {
                    nf
                } else {
                    return;
                };
                state.open_files.new_file(nx_file, &c, &w, window_width);

                println!(
                    "{}",
                    state
                        .open_files
                        .get_file(0)
                        .unwrap()
                        .nx_file()
                        .node_count()
                );

                /*
                // Test
                let shit = state.open_files.get_file(0).unwrap();
                let ni = ::nx_utils::NxDepthIter::new(shit.nx_file().root());
                for (i, n) in ni.enumerate() {
                    println!("{}", n.name());
                    if i > 300 {
                        break;
                    }
                }
                */
            });
        }
        {
            let s = Arc::clone(state);
            let w = gtk_window.clone();
            toolbar.save_as_button.connect_clicked(move |_| {
                if let Err(e) = s
                    .lock()
                    .unwrap()
                    .open_files
                    .get_file(0)
                    .unwrap()
                    .write_to_file(&w)
                {
                    eprintln!("{}", e);
                }
            });
        }
        {
            let w = gtk_window.clone();
            toolbar.about_button.connect_clicked(move |_| {
                if let Err(e) = run_about_dialog(&w) {
                    eprintln!("{}", e);
                }
            });
        }

        // Hooking up window resize events here.
        {
            let c = Arc::clone(&content);
            let s = Arc::clone(&state);
            gtk_window.connect_configure_event(move |_, event| {
                let mut s = s.lock().unwrap();

                let (new_width, _) = event.get_size();
                if s.window_width == new_width {
                    return false;
                }
                s.window_width = new_width;

                let wrap_width = get_wrap_width(new_width);

                if let Some(tv) = c.lock().unwrap().tree_view.as_ref() {
                    tv.gtk_tree_view.get_columns().iter().for_each(|col| {
                        col.get_cells()
                            .iter()
                            .filter_map(|cell| cell.clone().downcast().ok())
                            .for_each(|cell: gtk::CellRendererText| {
                                cell.set_property_wrap_width(wrap_width);
                            });
                    });
                }

                false
            });
        }

        Ok(Self {
            gtk_window,
            toolbar,
            content,
        })
    }
}

impl Content {
    pub fn new(window: &gtk::ApplicationWindow) -> Self {
        let main_box = gtk::Box::new(gtk::Orientation::Horizontal, 0);
        main_box.set_homogeneous(true);

        window.add(&main_box);

        Self {
            main_box,
            node_view: None,
            tree_view: None,
        }
    }
}

impl Toolbar {
    pub fn new() -> Self {
        let container = gtk::HeaderBar::new();
        container.set_title("nx_edit");
        container.set_show_close_button(true);

        // Add buttons to toolbar.
        let open_button = gtk::Button::new_with_label("open file");
        if let Some(c) = open_button.get_style_context() {
            c.add_class("suggested-action");
        }
        container.pack_start(&open_button);

        let save_as_button = gtk::Button::new_with_label("save as");
        container.pack_start(&save_as_button);

        let close_button = gtk::Button::new_with_label("close file");
        if let Some(c) = close_button.get_style_context() {
            c.add_class("destructive-action");
        }
        container.pack_start(&close_button);

        let about_button = gtk::Button::new_with_label("about");
        container.pack_end(&about_button);

        Self {
            container,
            open_button,
            save_as_button,
            close_button,
            about_button,
        }
    }
}

impl NodeView {
    pub fn new_empty(main_box: &gtk::Box) -> Self {
        let own_box = gtk::Box::new(gtk::Orientation::Vertical, 0);
        own_box.set_homogeneous(false);

        let name_display =
            gtk::TextView::new_with_buffer(&gtk::TextBuffer::new(None));
        config_text_view(&name_display);
        own_box.pack_start(&name_display, false, false, 4);

        let node_display = {
            let blank_label = gtk::Label::new("");
            own_box.pack_start(&blank_label, true, true, 0);
            NodeDisplay::Empty(blank_label)
        };

        let buttons = NodeViewButtons::new();
        own_box.pack_end(&buttons.own_box, false, false, 0);

        main_box.pack_start(&own_box, true, true, 0);

        Self {
            own_box,
            name_display,
            node_display,
            assoc_path: None,
            buttons,
        }
    }

    pub fn set_empty(&mut self) {
        self.destroy();

        let blank_label = gtk::Label::new("");
        self.own_box.pack_start(&blank_label, true, true, 0);

        self.node_display = NodeDisplay::Empty(blank_label);
        self.assoc_path = None;
    }

    pub fn set_text(&mut self, text: &str, path: Vec<i32>) -> &gtk::TextView {
        self.destroy();

        let scroll_win = gtk::ScrolledWindow::new(None, None);

        let text_view = {
            let buf = gtk::TextBuffer::new(None);
            buf.set_text(text);
            gtk::TextView::new_with_buffer(&buf)
        };
        config_text_view(&text_view);
        scroll_win.add(&text_view);
        self.own_box.pack_start(&scroll_win, true, true, 0);

        self.node_display = NodeDisplay::Text(scroll_win, text_view);
        self.assoc_path = Some(path);

        match self.node_display {
            NodeDisplay::Text(_, ref tv) => tv,
            _ => unreachable!(),
        }
    }

    pub fn set_img(&mut self, img: gtk::Image, path: Vec<i32>) {
        self.destroy();

        self.own_box.pack_start(&img, true, true, 0);

        self.node_display = NodeDisplay::Image(img);
        self.assoc_path = Some(path);
    }

    pub fn show(&self) {
        self.own_box.show_all();
    }

    fn destroy(&mut self) {
        match self.node_display {
            NodeDisplay::Empty(ref n) => n.destroy(),
            NodeDisplay::Text(ref sw, _) => sw.destroy(),
            NodeDisplay::Image(ref i) => i.destroy(),
            NodeDisplay::Audio(_) =>
                unimplemented!("TODO: NodeDisplay::Audio"),
        }
    }
}

impl NodeViewButtons {
    pub fn new() -> Self {
        let own_box = gtk::Box::new(gtk::Orientation::Horizontal, 0);
        own_box.set_homogeneous(true);

        let record_button = gtk::Button::new_with_label("record");
        own_box.pack_start(&record_button, true, true, 0);

        let insert_button = gtk::Button::new_with_label("insert node");
        own_box.pack_start(&insert_button, true, true, 0);

        let insert_menu = InsertMenu::new();

        let delete_button = gtk::Button::new_with_label("delete node");
        own_box.pack_start(&delete_button, true, true, 0);

        let undo_button = gtk::Button::new_with_label("undo");
        own_box.pack_start(&undo_button, true, true, 0);

        let redo_button = gtk::Button::new_with_label("redo");
        own_box.pack_start(&redo_button, true, true, 0);

        Self {
            own_box,
            record_button,
            insert_button,
            insert_menu,
            delete_button,
            undo_button,
            redo_button,
        }
    }
}

impl TreeView {
    pub fn new(main_box: &gtk::Box, gtk_tree_view: gtk::TreeView) -> Self {
        let scroll_win = gtk::ScrolledWindow::new(None, None);

        scroll_win.add(&gtk_tree_view);

        main_box.pack_end(&scroll_win, true, true, 0);

        Self {
            scroll_win,
            gtk_tree_view,
        }
    }
}

impl InsertMenu {
    pub fn new() -> Self {
        let menu = gtk::Menu::new();

        let before_item = gtk::MenuItem::new_with_label("before");
        let before_menu = TypeMenu::new();
        before_item.set_submenu(&before_menu.menu);
        menu.append(&before_item);

        let after_item = gtk::MenuItem::new_with_label("after");
        let after_menu = TypeMenu::new();
        after_item.set_submenu(&after_menu.menu);
        menu.append(&after_item);

        let child_item = gtk::MenuItem::new_with_label("child");
        let child_menu = TypeMenu::new();
        child_item.set_submenu(&child_menu.menu);
        menu.append(&child_item);

        Self {
            menu,
            before_item,
            before_menu,
            after_item,
            after_menu,
            child_item,
            child_menu,
        }
    }
}

impl TypeMenu {
    pub fn new() -> Self {
        let menu = gtk::Menu::new();

        let empty_item = gtk::MenuItem::new_with_label("empty");
        menu.append(&empty_item);

        let str_item = gtk::MenuItem::new_with_label("string");
        menu.append(&str_item);

        let int_item = gtk::MenuItem::new_with_label("integer");
        menu.append(&int_item);

        let float_item = gtk::MenuItem::new_with_label("float");
        menu.append(&float_item);

        let vector_item = gtk::MenuItem::new_with_label("vector");
        menu.append(&vector_item);

        let img_item = gtk::MenuItem::new_with_label("image");
        menu.append(&img_item);

        let audio_item = gtk::MenuItem::new_with_label("audio");
        menu.append(&audio_item);

        Self {
            menu,
            empty_item,
            str_item,
            int_item,
            float_item,
            vector_item,
            img_item,
            audio_item,
        }
    }
}

fn open_file(
    window: &gtk::ApplicationWindow,
) -> Result<Option<nx::File>, Error> {
    let file_dialog = gtk::FileChooserDialog::with_buttons(
        Some("select an *.nx file to view/edit"),
        Some(window),
        gtk::FileChooserAction::Open,
        &[
            ("open", gtk::ResponseType::Accept),
            ("cancel", gtk::ResponseType::Cancel),
        ],
    );
    let file_filter = gtk::FileFilter::new();
    FileFilterExt::set_name(&file_filter, "NX files");
    file_filter.add_pattern("*.nx");
    file_dialog.add_filter(&file_filter);

    let dialog_res: gtk::ResponseType = file_dialog.run().into();
    let res = match dialog_res {
        gtk::ResponseType::Accept =>
            if let Some(file) = file_dialog.get_file() {
                let path = &file.get_path().ok_or_else(|| {
                    Error::Gio("gio::File has no path".to_owned())
                })?;

                if path.extension().and_then(|os| os.to_str()) == Some("nx") {
                    Ok(unsafe { nx::File::open(path).map(Some)? })
                } else {
                    eprintln!(r#"filename doesn't match "*.nx""#);
                    run_msg_dialog(
                        &file_dialog,
                        "wrong file type",
                        "wrong file type (must be *.nx).",
                        gtk::MessageType::Error,
                    );

                    Ok(None)
                }
            } else {
                Ok(None)
            },
        gtk::ResponseType::DeleteEvent => return Ok(None),
        _ => Ok(None),
    };

    file_dialog.destroy();

    res
}

pub fn run_msg_dialog<W: gtk::IsA<gtk::Window>>(
    parent: &W,
    title: &str,
    msg: &str,
    msg_type: gtk::MessageType,
) {
    let md = gtk::MessageDialog::new(
        Some(parent),
        gtk::DialogFlags::from_bits(0b11).unwrap(),
        msg_type,
        gtk::ButtonsType::Close,
        msg,
    );
    md.set_title(title);

    md.run();
    md.destroy();
}

pub fn run_about_dialog<
    'a,
    P: gtk::IsA<gtk::Window> + 'a,
    Q: Into<Option<&'a P>>,
>(
    parent: Q,
) -> Result<(), Error> {
    let ad = gtk::AboutDialog::new();

    ad.set_transient_for(parent);
    ad.set_copyright(
        "(ɔ) copyleft 2018-2019, IntransigentMS v2 Team. all rites reversed.",
    );
    ad.set_license_type(gtk::License::Agpl30);
    ad.set_logo(&Pixbuf::new_from_file_at_size("img/nx_edit.svg", 128, 128)?);
    ad.set_program_name("nx_edit");
    ad.set_website("https://bitbucket.org/NoetherEmmy/nx_edit");
    ad.set_website_label("source");
    ad.set_version(env!("CARGO_PKG_VERSION"));

    ad.run();
    ad.destroy();

    Ok(())
}

pub fn config_text_view(t: &gtk::TextView) {
    t.set_wrap_mode(gtk::WrapMode::Word);
    t.set_accepts_tab(false); // You have to use '\t' anyways.
    t.set_cursor_visible(true);
    t.set_editable(true);
    t.set_input_purpose(gtk::InputPurpose::FreeForm);
    t.set_justification(gtk::Justification::Left);
    t.set_monospace(true);
}

pub fn get_wrap_width(window_width: u32) -> i32 {
    (if window_width < 1300 {
        if window_width < 900 {
            (window_width / 5)
        } else {
            (window_width / 4)
        }
    } else if window_width < 1900 {
        (window_width / 3)
    } else if window_width < 3000 {
        (window_width * 3 / 8)
    } else {
        (window_width / 2)
    } as i32)
}
