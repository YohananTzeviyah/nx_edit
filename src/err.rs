use nx;
use std::{self, fmt};


#[derive(Debug)]
pub enum Error<'a> {
    Gio(&'a str),
    Nx(nx::Error),
}


impl<'a> fmt::Display for Error<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Gio(s) => {
                f.write_str("[gio error] ")?;
                f.write_str(s)
            },
            Error::Nx(ne) => write!(f, "[nx error] {}", ne),
        }
    }
}

impl<'a> std::error::Error for Error<'a> {}

impl<'a> From<nx::Error> for Error<'a> {
    fn from(nxerr: nx::Error) -> Self {
        Error::Nx(nxerr)
    }
}
