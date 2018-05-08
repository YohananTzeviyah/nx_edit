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
    pub container:    gtk::HeaderBar,
    pub open_button:  gtk::Button,
    pub close_button: gtk::Button,
    pub about_button: gtk::Button,
}

pub enum NodeDisplay {
    Nothing(gtk::Label),
    Text(gtk::ScrolledWindow, gtk::TextView),
    Image(gtk::Image),
    Audio(u8), // TODO
}

pub struct NodeView {
    pub main_box:     gtk::Box,
    pub node_display: NodeDisplay,
}

pub struct TreeView {
    pub scroll_win:    gtk::ScrolledWindow,
    pub gtk_tree_view: gtk::TreeView,
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
        gtk_window.set_default_size(800, 600);

        {
            let w = gtk_window.clone();
            gtk_window.connect_delete_event(move |_, _| {
                w.destroy();
                Inhibit(false)
            });
        }

        let toolbar = Toolbar::new();
        gtk_window.set_titlebar(&toolbar.container);

        let content = Content::new(&gtk_window);

        // Hook up toolbar button actions here.
        {
            let s = Arc::clone(&state);
            let w = gtk_window.clone();
            let c = Arc::clone(&content);
            toolbar.open_button.connect_clicked(move |_| {
                let mut state = s.lock().unwrap();
                // TODO: Figure out some kind of error handling, maybe
                // involving storing errors in `AppState`.
                let nx_file = if let Some(nf) = open_file(&w).unwrap() {
                    nf
                } else {
                    return;
                };
                state.open_files.new_file(nx_file, &c);

                println!(
                    "{}",
                    state.open_files.get_file(0).unwrap().node_count()
                );
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
            gtk_window.connect_configure_event(move |_, event| {
                let (new_width, _) = event.get_size();

                if let Some(tv) = c.lock().unwrap().tree_view.as_ref() {
                    tv.gtk_tree_view.get_columns().iter().for_each(|col| {
                        col.get_cells()
                            .iter()
                            .filter_map(|cell| cell.clone().downcast().ok())
                            .for_each(|cell: gtk::CellRendererText| {
                                cell.set_property_wrap_width(
                                    (new_width / 3 - 64) as i32,
                                );
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
    pub fn new(window: &gtk::ApplicationWindow) -> Arc<Mutex<Self>> {
        let main_box = gtk::Box::new(gtk::Orientation::Horizontal, 0);
        main_box.set_homogeneous(true);

        window.add(&main_box);

        Arc::new(Mutex::new(Self {
            main_box,
            node_view: None,
            tree_view: None,
        }))
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
            close_button,
            about_button,
        }
    }
}

impl NodeView {
    pub fn new_empty(main_box: &gtk::Box) -> Self {
        let node_display = {
            let blank_label = gtk::Label::new("");
            noexpand(&blank_label);
            main_box.pack_start(&blank_label, true, true, 0);
            NodeDisplay::Nothing(blank_label)
        };

        Self {
            main_box: main_box.clone(),
            node_display,
        }
    }

    pub fn set_text(&mut self, text: &str) {
        self.destroy();

        let scroll_win = gtk::ScrolledWindow::new(None, None);
        noexpand(&scroll_win);

        let text_view = {
            let buf = gtk::TextBuffer::new(None);
            buf.set_text(text);
            gtk::TextView::new_with_buffer(&buf)
        };
        config_text_view(&text_view);
        scroll_win.add(&text_view);
        self.main_box.pack_start(&scroll_win, true, true, 0);

        self.node_display = NodeDisplay::Text(scroll_win, text_view);
    }

    pub fn set_img(&mut self, img: gtk::Image) {
        self.destroy();

        noexpand(&img);
        self.main_box.pack_start(&img, true, true, 0);

        self.node_display = NodeDisplay::Image(img);
    }

    pub fn show(&self) {
        match self.node_display {
            NodeDisplay::Nothing(ref n) => n.show_all(),
            NodeDisplay::Text(ref sw, _) => sw.show_all(),
            NodeDisplay::Image(ref i) => i.show_all(),
            NodeDisplay::Audio(_) =>
                unimplemented!("TODO: NodeDisplay::Audio"),
        }
    }

    fn destroy(&mut self) {
        match self.node_display {
            NodeDisplay::Nothing(ref n) => n.destroy(),
            NodeDisplay::Text(ref sw, _) => sw.destroy(),
            NodeDisplay::Image(ref i) => i.destroy(),
            NodeDisplay::Audio(_) =>
                unimplemented!("TODO: NodeDisplay::Audio"),
        }
    }
}

impl TreeView {
    pub fn new(main_box: &gtk::Box, gtk_tree_view: gtk::TreeView) -> Self {
        let scroll_win = gtk::ScrolledWindow::new(None, None);
        expand(&scroll_win);

        scroll_win.add(&gtk_tree_view);

        main_box.pack_end(&scroll_win, true, true, 0);

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
                let path = &file.get_path().ok_or_else(|| {
                    Error::Gio("gio::File has no path".to_owned())
                })?;

                if path.extension().and_then(|os| os.to_str()) == Some("nx") {
                    Ok(unsafe { nx::File::open(path).map(Some)? })
                } else {
                    eprintln!("Filename doesn't match \"*.nx\"");
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

pub fn run_msg_dialog<W: IsA<gtk::Window>>(
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

pub fn noexpand<W: gtk::WidgetExt>(w: &W) {
    w.set_property_expand(false);
    w.set_hexpand(false);
    w.set_hexpand_set(true);
    w.set_vexpand(false);
    w.set_vexpand_set(true);
}

pub fn expand<W: gtk::WidgetExt>(w: &W) {
    w.set_property_expand(true);
    w.set_hexpand(true);
    w.set_hexpand_set(true);
    w.set_vexpand(true);
    w.set_vexpand_set(true);
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
