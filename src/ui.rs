use err::Error;
use gio::FileExt;
use gtk::{self, prelude::*};
use nx;
use state::*;
use std::sync::{Arc, Mutex};


pub struct App {
    pub state:   Arc<Mutex<AppState>>,
    pub content: Content,
    pub toolbar: gtk::Toolbar,
}

pub struct Content {
    pub window: gtk::ApplicationWindow,
}


impl App {
    pub fn new(application: &gtk::Application) -> Self {
        let state = Arc::new(Mutex::new(AppState::new()));
        let content = Content::new(application);
        let toolbar = gtk::Toolbar::new();

        // Add buttons to toolbar.
        let open_button = gtk::ToolButton::new(&gtk::Label::new("open file"),
                                               "open file");
        {
            let s = Arc::clone(&state);
            let w = content.window.clone();
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

        content.window.add(&toolbar);

        Self { state, content, toolbar }
    }
}

impl Content {
    pub fn new(application: &gtk::Application) -> Self {
        let window = gtk::ApplicationWindow::new(application);

        gtk::Window::set_default_icon_name("iconname"); // TODO
        window.set_title("nx_edit");
        window.set_position(gtk::WindowPosition::Center);

        let w = window.clone();
        window.connect_delete_event(move |_, _| {
            w.destroy();
            Inhibit(false)
        });

        Self { window }
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

                    let d = gtk::Dialog::new_with_buttons(
                        Some("wrong file type"),
                        Some(&file_dialog),
                        gtk::DialogFlags::from_bits(0b11).unwrap(),
                        &[("close", 0)]
                    );
                    d.run();
                    d.destroy();

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
