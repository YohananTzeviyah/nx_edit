use gdk_pixbuf;
use nx;
use std::{self, fmt};

#[derive(Debug)]
pub enum Error {
    Gio(String),
    Pixbuf(gdk_pixbuf::Error),
    Nx(nx::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Gio(s) => {
                f.write_str("[gio error] ")?;
                f.write_str(s)
            },
            Error::Pixbuf(e) => {
                f.write_str("[gdk_pixbuf error] ")?;
                write!(f, "{}", e)
            },
            Error::Nx(ne) => write!(f, "[nx error] {}", ne),
        }
    }
}

impl std::error::Error for Error {}

impl From<nx::Error> for Error {
    fn from(nxerr: nx::Error) -> Self {
        Error::Nx(nxerr)
    }
}

impl From<gdk_pixbuf::Error> for Error {
    fn from(pixbuferr: gdk_pixbuf::Error) -> Self {
        Error::Pixbuf(pixbuferr)
    }
}
