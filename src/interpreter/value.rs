/// A value being handled by the interpreter.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Boolean(bool),
    String(String),
    Number(f64),
}

impl Value {
    /// Check whether a value is truthy or not.
    pub fn is_truthy(&self) -> bool {
        if self == &Value::Nil {
            false
        } else if let Value::Boolean(b) = self {
            *b
        } else {
            true
        }
    }
}
