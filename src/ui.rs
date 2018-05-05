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
    pub content:    Content,
}

pub struct Content {
    pub main_box: gtk::Box,
    pub toolbar:  gtk::Toolbar,
    pub label:    gtk::Label,
}


impl App {
    pub fn new(application: &gtk::Application) -> Self {
        let state = Arc::new(Mutex::new(AppState::new()));
        let window = Window::new(application, &state);

        Self { state, window }
    }
}

impl Window {
    pub fn new(application: &gtk::Application,
               state:       &Arc<Mutex<AppState>>) -> Self {
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

        Self { gtk_window, content }
    }
}

impl Content {
    pub fn new(window: &gtk::ApplicationWindow,
               state:  &Arc<Mutex<AppState>>) -> Self {
        let main_box = gtk::Box::new(gtk::Orientation::Vertical, 0);

        //
        let toolbar = gtk::Toolbar::new();

        // Add buttons to toolbar.
        let open_button = gtk::ToolButton::new(&gtk::Label::new("open file"),
                                               "open file");
        {
            let s = Arc::clone(&state);
            let w = window.clone();
            open_button.connect_clicked(move |_| {
                let mut state = s.lock().unwrap();
                // TODO: Figure out some kind of error handling, maybe
                // involving storing errors in `AppState`.
                state.open_files.new_file(open_file(&w).unwrap().unwrap());

                println!("{}",
                         state.open_files.get_file(0).unwrap().node_count());
            });
        }
        toolbar.insert(&open_button, 0);

        let about_button = gtk::ToolButton::new(&gtk::Label::new("about"), //&gtk::Image::new_from_file("img/about.svg")
                                                "about");
        toolbar.insert(&about_button, 1);
        // toolbar.insert(...

        main_box.pack_start(&toolbar, true, false, 0);

        //
        let label = gtk::Label::new("LOL!");
        label.set_halign(gtk::Align::Center);
        main_box.pack_end(&label, true, true, 8);

        window.add(&main_box);

        Self { main_box, toolbar, label }
    }
}

fn open_file(window: &gtk::ApplicationWindow)
    -> Result<Option<nx::File>, Error>
{
    let file_dialog = gtk::FileChooserDialog::with_buttons(
        Some("select an *.nx file to view/edit"),
        Some(window),
        gtk::FileChooserAction::Open,
        &[("open",   gtk::ResponseType::Accept),
          ("cancel", gtk::ResponseType::Cancel)]
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

                if path.extension().and_then(|os| os.to_str()) != Some("nx") {
                    eprintln!("Filename doesn't match \"*.nx\"");
                    run_msg_dialog(&file_dialog,
                                   "wrong file type (must be *.nx).");

                    return Ok(None);
                }

                Ok(unsafe { nx::File::open(path).map(|nf| Some(nf))? })
            } else {
                Ok(None)
            },
                                       // No need to destroy the file dialog.
        gtk::ResponseType::DeleteEvent => return Ok(None),
        _ => Ok(None),
    };

    file_dialog.destroy();

    res
}

pub fn run_msg_dialog<W: IsA<gtk::Window>>(parent: &W, msg: &str) {
    let md = gtk::MessageDialog::new(
        Some(parent),
        gtk::DialogFlags::from_bits(0b11).unwrap(),
        gtk::MessageType::Error,
        gtk::ButtonsType::Close,
        msg
    );

    md.run();
    md.destroy();
}
