use err::Error;
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
    pub content:    Arc<Mutex<Content>>,
}

pub struct Content {
    pub main_box:  gtk::Box,
    pub toolbar:   Toolbar,
    pub tree_view: Option<TreeView>,
}

pub struct Toolbar {
    pub gtk_toolbar:  gtk::Toolbar,
    pub open_button:  gtk::ToolButton,
    pub about_button: gtk::ToolButton,
}

pub struct TreeView {
    pub scroll_win:    gtk::ScrolledWindow,
    pub gtk_tree_view: gtk::TreeView,
}

impl App {
    pub fn new(application: &gtk::Application) -> Self {
        let state = Arc::new(Mutex::new(AppState::new()));
        let window = Window::new(application, &state);

        Self { state, window }
    }
}

impl Window {
    pub fn new(
        application: &gtk::Application,
        state: &Arc<Mutex<AppState>>,
    ) -> Self {
        let gtk_window = gtk::ApplicationWindow::new(application);

        gtk::Window::set_default_icon_name("iconname"); // TODO
        gtk_window.set_title("nx_edit");
        gtk_window.set_position(gtk::WindowPosition::Center);
        gtk_window.set_default_size(800, 600);

        let w = gtk_window.clone();
        gtk_window.connect_delete_event(move |_, _| {
            w.destroy();
            Inhibit(false)
        });

        let content = Content::new(&gtk_window, state);

        Self {
            gtk_window,
            content,
        }
    }
}

impl Content {
    pub fn new(
        window: &gtk::ApplicationWindow,
        state: &Arc<Mutex<AppState>>,
    ) -> Arc<Mutex<Self>> {
        let main_box = gtk::Box::new(gtk::Orientation::Vertical, 0);

        //
        let toolbar = Toolbar::new();
        main_box.pack_start(&toolbar.gtk_toolbar, true, false, 0);

        window.add(&main_box);

        let ret = Arc::new(Mutex::new(Self {
            main_box,
            toolbar,
            tree_view: None,
        }));

        {
            let s = Arc::clone(&state);
            let w = window.clone();
            let c = Arc::clone(&ret);
            ret.lock()
                .unwrap()
                .toolbar
                .open_button
                .connect_clicked(move |_| {
                    let mut state = s.lock().unwrap();
                    // TODO: Figure out some kind of error handling, maybe
                    // involving storing errors in `AppState`.
                    state
                        .open_files
                        .new_file(open_file(&w).unwrap().unwrap(), &c);

                    println!(
                        "{}",
                        state
                            .open_files
                            .get_file(0)
                            .unwrap()
                            .node_count()
                    );
                });
        }

        ret
    }
}

impl Toolbar {
    pub fn new() -> Self {
        let gtk_toolbar = gtk::Toolbar::new();
        gtk_toolbar.set_vexpand(false);
        gtk_toolbar.set_vexpand_set(true);

        // Add buttons to toolbar.
        let open_button =
            gtk::ToolButton::new(&gtk::Label::new("open file"), "open file");
        gtk_toolbar.insert(&open_button, 0);

        let about_button = gtk::ToolButton::new(&gtk::Label::new("about"), //&gtk::Image::new_from_file("img/about.svg")
                                                "about");
        gtk_toolbar.insert(&about_button, 1);
        // toolbar.insert(...

        Self {
            gtk_toolbar,
            open_button,
            about_button,
        }
    }
}

impl TreeView {
    pub fn new(main_box: &gtk::Box, gtk_tree_view: gtk::TreeView) -> Self {
        let scroll_win = gtk::ScrolledWindow::new(None, None);
        scroll_win.set_property_expand(true);

        scroll_win.add(&gtk_tree_view);

        main_box.pack_start(&scroll_win, true, true, 8);

        Self {
            scroll_win,
            gtk_tree_view,
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
    file_filter.add_pattern("*.nx");
    file_dialog.add_filter(&file_filter);

    let dialog_res: gtk::ResponseType = file_dialog.run().into();
    let res = match dialog_res {
        gtk::ResponseType::Accept =>
            if let Some(file) = file_dialog.get_file() {
                let path = &file.get_path()
                    .ok_or(Error::Gio("`gio::File` has no path"))?;

                if path.extension().and_then(|os| os.to_str()) == Some("nx") {
                    Ok(unsafe { nx::File::open(path).map(Some)? })
                } else {
                    eprintln!("Filename doesn't match \"*.nx\"");
                    run_err_msg_dialog(
                        &file_dialog,
                        "wrong file type (must be *.nx).",
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

pub fn run_err_msg_dialog<W: IsA<gtk::Window>>(parent: &W, msg: &str) {
    let md = gtk::MessageDialog::new(
        Some(parent),
        gtk::DialogFlags::from_bits(0b11).unwrap(),
        gtk::MessageType::Error,
        gtk::ButtonsType::Close,
        msg,
    );

    md.run();
    md.destroy();
}
