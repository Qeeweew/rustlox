#[derive (Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    // Single-character tokens.
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
  
    // One or two character tokens.
    Bang, 
    BangEqual,
    Equal, 
    EqualEqual,
    Greater, 
    GreaterEqual,
    Less, 
    LessEqual,
  
    // Literals.
    Identifier,
    String, 
    Number,
  
    // Keywords.
    AND, 
    CLASS, 
    ELSE, 
    FALSE, 
    FUN, 
    FOR, 
    IF, 
    NIL, 
    OR,
    PRINT, 
    RETURN, 
    SUPER, 
    THIS, 
    TRUE, 
    VAR, 
    WHILE,
  
    // extra
    EOF,
}

#[derive (Clone, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: Option<Box<String>>,
}

impl Token {
    pub fn new(token_type: TokenType) -> Self {
        Self { 
            token_type, 
            literal: None,
        }
    }

    pub fn new_literal(token_type: TokenType, s: Box<String>) -> Self {
        Self { 
            token_type, 
            literal: Some(s),
        }
    }

}
