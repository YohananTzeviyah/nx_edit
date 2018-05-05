use nx;


pub struct AppState {
    pub open_files: OpenFiles,
}

pub struct OpenFiles {
    files: Vec<nx::File>,
}


impl AppState {
    pub fn new() -> Self {
        Self { open_files: OpenFiles::new() }
    }
}

impl OpenFiles {
    pub fn new() -> Self {
        Self { files: Vec::with_capacity(2) }
    }

    pub fn new_file(&mut self, nf: nx::File) {
        self.files.push(nf);
    }

    pub fn get_file(&self, index: usize) -> Option<&nx::File> {
        self.files.get(index)
    }
}
