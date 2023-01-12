use core::{str::{self}};
use std::collections::HashMap;

use super::token::*;

use nom::{
    IResult, 
    combinator::{map, recognize}, 
    character::{
        complete::{one_of, digit1, }, 
        is_alphabetic, is_alphanumeric
    }, 
    branch::alt, 
    bytes::{complete::{tag, take, take_while1, take_while}}, 
    sequence::{delimited, preceded, pair}, 
    multi::{many0, many_till}, 
    AsBytes
};
fn easy_token(input: &[u8]) -> IResult<&[u8], Token> {
    use TokenType::*;
    alt((
        map(tag("!="), |_| Token::new(BangEqual)),
        map(tag("=="), |_| Token::new(EqualEqual)),
        map(tag(">="), |_| Token::new(GreaterEqual)),
        map(tag("<="), |_| Token::new(LessEqual)),
        map(one_of("(){},.-+;/*!<=>"), |c|
            match c {
                '(' => Token::new(LeftParen),
                ')' => Token::new(RightParen),
                '{' => Token::new(LeftBrace),
                '}' => Token::new(RightBrace),
                ',' => Token::new(Comma),
                '.' => Token::new(Dot),
                '-' => Token::new(Minus),
                '+' => Token::new(Plus),
                ';' => Token::new(Semicolon),
                '/' => Token::new(Slash),
                '*' => Token::new(Star),
                '!' => Token::new(Bang),
                '=' => Token::new(Equal),
                '>' => Token::new(Greater),
                '<' => Token::new(Less),
                _ => unreachable!(),
            }),
    ))(input)
}

fn number_token(input: &[u8]) -> IResult<&[u8], Token> {
    // [0-9]+.? [0-9]+
    map(recognize(alt((preceded(pair(digit1, tag(".")), digit1), digit1))),
        |s: &[u8]| {
            let s = convert_u8_string(s);
            Token::new_literal(TokenType::Number, s)
        }
    )(input)
}
// ascii 
fn convert_u8_string(s: &[u8]) -> Box<String> { Box::new(str::from_utf8(s).unwrap().to_owned()) }

fn string_token(input: &[u8]) -> IResult<&[u8], Token> {
    //"[^"]+"
    map(delimited(tag("\""), take_while(|c| c != b'\"'), tag("\"")), |s| 
        Token::new_literal(TokenType::String, convert_u8_string(s))
    )(input)
}

lazy_static! {
    static ref KEYWORDS_MAP : HashMap<String, TokenType> = {
        let mut map = HashMap::new();
        map.insert("and".to_owned(), TokenType::AND);
        map.insert("class".to_owned(), TokenType::CLASS);
        map.insert("else".to_owned(), TokenType::ELSE);
        map.insert("false".to_owned(), TokenType::FALSE);
        map.insert("fun".to_owned(), TokenType::FUN);
        map.insert("for".to_owned(), TokenType::FOR);
        map.insert("if".to_owned(), TokenType::IF); 
        map.insert("nil".to_owned(), TokenType::NIL);
        map.insert("or".to_owned(), TokenType::OR);
        map.insert("print".to_owned(), TokenType::PRINT);
        map.insert("return".to_owned(), TokenType::RETURN);
        map.insert("super".to_owned(), TokenType::SUPER);
        map.insert("this".to_owned(), TokenType::THIS);
        map.insert("true".to_owned(), TokenType::TRUE);
        map.insert("var".to_owned(), TokenType::VAR);
        map.insert("while".to_owned(), TokenType::WHILE);
        map
    };
}

fn identifier(input: &[u8]) -> IResult<&[u8], Token> {
    map(
        recognize(
            preceded(take_while1(|c| is_alphabetic(c) || c == b'_'), take_while(|c| is_alphanumeric(c) || c == b'_'))),
        |s: &[u8]| {
            let s = String::from_utf8(s.into()).unwrap();
            if let Some(&token) = KEYWORDS_MAP.get(&s) {
                Token::new(token)
            } else {
                Token::new_literal(TokenType::Identifier, Box::new(s))
            }
        }
    )(input)
}

// '\n' deals in the loop
fn skip_white_space(input: &[u8]) -> IResult<&[u8], ()> {
    map(one_of(" \t\r\n"), |_| ())(input)
}

fn skip_line_comment(input: &[u8]) -> IResult<&[u8], ()> {
    map(preceded(tag("//"), take_while(|c| c != b'\n')), |_| ())(input)
}
fn block_comment(input: &[u8]) -> IResult<&[u8], ()> {
    let (next_input, (_, s)) = many_till(take(1usize), alt((tag("*/"), tag("/*"))))(input)?;
    match s.as_bytes() {
        b"*/" => Ok((next_input,())),
        b"/*" => preceded(block_comment, block_comment) (next_input),
        _ => unreachable!(),
    }
}
fn skip_block_comment(input: &[u8]) -> IResult<&[u8],()> {
    preceded(tag("/*"), block_comment)(input)
}

fn get_a_token(input: &[u8]) -> IResult<&[u8], Token> {
    alt((identifier, string_token, number_token, easy_token,))(input)
}

fn get_tokens(input: &[u8]) -> IResult<&[u8], Vec<Token>> {
    let mut vec = vec![];
    let (i, _) = many0(alt((skip_white_space, skip_line_comment, skip_block_comment, map(get_a_token, |s| -> () {
        vec.push(s);
    }))))(input)?;
    Ok((i, vec))
}

pub struct Scanner {
    source: String,
}
impl Scanner {
    pub fn scan(&mut self) -> Result<Vec<Token>, String>{
        let (i, mut res) = get_tokens(self.source.as_bytes()).unwrap();
        if i.len() != 0 {
            return Err(format!("lexer error at line: {}", convert_u8_string(i)));
        }
        res.push(Token::new(TokenType::EOF));
        Ok(res)
    }
    pub fn new(source: String) -> Self {
        Scanner { source}
    }
}

#[test]
fn test() {

    let mut sc = Scanner {
        source: r"andy formless fo _ _123 _abc ab123
        abcdefghijklmnopqrstuvwxyzAB
        CDEFGHIJKLMNOPQRSTUVWXYZ1234567890_
        ".to_owned(),
    };
    let res = sc.scan().unwrap();

    println!("{:?}", res);
}

