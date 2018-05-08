# nx_edit

a graphical user interface for viewing and editing
[NX files](https://nxformat.github.io/), written in Rust.

## requirements

- GTK+ 3.22+ runtime libraries (should include glib, cairo, pango, etc. runtime
  libraries)

## additional requirements for building from source

- the latest nightly version of `rustc`, which can be obtained from
  [rustup](https://www.rust-lang.org/en-US/install.html)
- GTK+ 3.22+ development files*
- GLib development files*
- Cairo development files*
- Pango development files*
- GDK-PixBuf development files*

\*see [here](http://gtk-rs.org/docs/requirements.html).

## building

simply use the provided makefile:

- `make native` : builds a very efficient "release" executable for your machine
  (not portable)
- `make debug` : quickly builds a "debug" executable with debug info
- `make release` : builds an efficient "release" executable that is portable to
  other machines with the same "target triple" (e.g. x86_64-unknown-linux-gnu)
