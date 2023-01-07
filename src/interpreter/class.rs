/// A Lox class.
#[derive(Debug, Clone)]
pub struct Class {
    name: String,
}

impl Class {
    /// Create a new class, specifying its name.
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

impl ToString for Class {
    fn to_string(&self) -> String {
        self.name.clone()
    }
}
