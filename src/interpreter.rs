/// A value being handled by the interpreter.
#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    String(String),
    Number(f64),
}
