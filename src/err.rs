use gdk_pixbuf;
use nx;
use std::{self, fmt, num};

#[derive(Debug)]
pub enum Error {
    Gio(String),
    Pixbuf(gdk_pixbuf::Error),
    Nx(nx::Error),
    ParseInt(num::ParseIntError),
    ParseFloat(num::ParseFloatError),
    ParseVector(String),
    LogicError(String),
    FileChooser(String),
    Io(std::io::Error),
    Path(String),
    IntoInner(String),
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
            Error::ParseInt(pie) => write!(f, "[integer parse error] {}", pie),
            Error::ParseFloat(pfe) => write!(f, "[float parse error] {}", pfe),
            Error::ParseVector(pve) => {
                f.write_str("[vector parse error] ")?;
                f.write_str(pve)
            },
            Error::LogicError(le) => {
                f.write_str("[logic error] ")?;
                f.write_str(le)
            },
            Error::FileChooser(fc) => {
                f.write_str("[file chooser error] ")?;
                f.write_str(fc)
            },
            Error::Io(io) => write!(f, "[io error] {}", io),
            Error::Path(p) => {
                f.write_str("[filepath error] ")?;
                f.write_str(p)
            },
            Error::IntoInner(ii) => {
                f.write_str("[intoinner error] ")?;
                f.write_str(ii)
            },
        }
    }
}

impl std::error::Error for Error {}

impl From<nx::Error> for Error {
    #[inline]
    fn from(nxerr: nx::Error) -> Self {
        Error::Nx(nxerr)
    }
}

impl From<gdk_pixbuf::Error> for Error {
    #[inline]
    fn from(pixbuferr: gdk_pixbuf::Error) -> Self {
        Error::Pixbuf(pixbuferr)
    }
}

impl From<num::ParseIntError> for Error {
    #[inline]
    fn from(parseerr: num::ParseIntError) -> Self {
        Error::ParseInt(parseerr)
    }
}

impl From<num::ParseFloatError> for Error {
    #[inline]
    fn from(parseerr: num::ParseFloatError) -> Self {
        Error::ParseFloat(parseerr)
    }
}

impl From<std::io::Error> for Error {
    #[inline]
    fn from(ioerr: std::io::Error) -> Self {
        Error::Io(ioerr)
    }
}

impl<W> From<std::io::IntoInnerError<W>> for Error {
    #[inline]
    fn from(iierr: std::io::IntoInnerError<W>) -> Self {
        Error::IntoInner(iierr.error().to_string())
    }
}
