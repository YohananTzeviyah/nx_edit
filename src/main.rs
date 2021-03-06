#![allow(unknown_lints)]
#![warn(clippy)]
#![feature(exact_chunks)]
#![feature(nll)]

mod audio;
mod compression;
mod err;
mod nx_utils;
mod state;
mod ui;

extern crate byteorder;
extern crate fxhash;
extern crate gdk;
extern crate gdk_pixbuf;
extern crate gio;
extern crate glib;
extern crate gtk;
extern crate lz4;
extern crate nx;
extern crate pango;
extern crate simplemad;
extern crate vorbis;

use err::Error;
use gio::prelude::*;
use gtk::prelude::*;
use std::env::args;
use ui::App;

#[inline]
fn started(application: &gtk::Application) -> Result<(), Error> {
    let app = App::new(application)?;
    app.window.gtk_window.show_all();

    Ok(())
}

fn main() {
    let application = gtk::Application::new(
        "net.ims_v2.nx_edit",
        gio::ApplicationFlags::empty(),
    ).expect("could not initialize GTK+ application.");

    application.connect_startup(move |app| {
        if let Err(e) = started(app) {
            eprintln!("{}", e);
        }
    });
    application.connect_activate(|_| {});

    application.run(&args().collect::<Vec<_>>());
}
