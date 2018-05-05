mod err;
mod state;
mod ui;

extern crate gdk_pixbuf;
extern crate gio;
extern crate glib;
extern crate gtk;
extern crate nx;

use gio::prelude::*;
use gtk::prelude::*;
use std::env::args;
use ui::App;


fn started(application: &gtk::Application) {
    let app = App::new(application);
    app.content.window.show_all();
}

fn main() {
    let application =
        gtk::Application::new("net.ims_v2.nx_edit",
                              gio::ApplicationFlags::empty())
                         .expect("Could not initialize GTK+ application.");

    application.connect_startup(move |app| started(app));
    application.connect_activate(|_| {});

    application.run(&args().collect::<Vec<_>>());
}
