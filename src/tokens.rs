/// The type of a token. May also contain its litteral value.
#[derive(Clone, PartialEq, Debug)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier(String),
    String(String),
    Number(f64),

    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

/// Full information about a token.
#[derive(Clone, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
}

impl Token {
    /// Check whether a token corresponds to a litteral value.
    pub fn is_litteral(&self) -> bool {
        match self.token_type {
            TokenType::True
            | TokenType::False
            | TokenType::Nil
            | TokenType::String(_)
            | TokenType::Number(_) => true,
            _ => false,
        }
    }
}
