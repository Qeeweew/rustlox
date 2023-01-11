#[derive (Debug, PartialEq, Clone)]
pub enum Token {
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
    Identifier(String), 
    String(String), 
    Number(f64),
  
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
  
    EOF
}

#[test]
fn test() {
}

