.PHONY: clippy debug release native release_windows native_windows

debug:
	cargo clippy && cargo build

release:
	cargo build --release && strip ./target/release/nx_edit

native:
	cargo rustc --release -- -C target-cpu=native && strip ./target/release/nx_edit

release_windows:
	cargo.exe build --release && strip ./target/release/nx_edit.exe

native_windows:
	cargo.exe rustc --release -- -C target-cpu=native && strip ./target/release/nx_edit.exe
