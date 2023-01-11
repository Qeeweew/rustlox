use core::str::{self};
use std::collections::HashMap;

use super::token::*;
use nom::{IResult, combinator::{map, recognize, opt}, character::{complete::{one_of, alpha1, digit1, alphanumeric0}}, branch::alt, bytes::{complete::{tag, is_not, take}}, sequence::{delimited, preceded}, multi::{many0, many_till}, AsBytes};
fn easy_token(input: &[u8]) -> IResult<&[u8], Token> {
    alt((
        map(tag("!="), |_| Token::BangEqual),
        map(tag("=="), |_| Token::EqualEqual),
        map(tag(">="), |_| Token::GreaterEqual),
        map(tag("<="), |_| Token::LessEqual),
        map(one_of("(){},.-+;/*!<=>"), |c|
            match c {
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                '{' => Token::LeftBrace,
                '}' => Token::RightBrace,
                ',' => Token::Comma,
                '.' => Token::Dot,
                '-' => Token::Minus,
                '+' => Token::Plus,
                ';' => Token::Semicolon,
                '/' => Token::Slash,
                '*' => Token::Star,
                '!' => Token::Bang,
                '=' => Token::Equal,
                '>' => Token::Greater,
                '<' => Token::Less,
                _ => unreachable!(),
            }),
    ))(input)
}

fn number_token(input: &[u8]) -> IResult<&[u8], Token> {
    // [0-9]+.? [0-9]+
    map(recognize(alt((preceded(preceded(digit1, tag(".")), digit1), digit1))),
        |s: &[u8]| {
            let s = convert_u8_string(s);
            Token::Number(s.parse::<f64>().unwrap())
        }
    )(input)
}
// ascii 
fn convert_u8_string(s: &[u8]) -> String { str::from_utf8(s).unwrap().to_owned() }

fn string_token(input: &[u8]) -> IResult<&[u8], Token> {
    //"[^"]+"
    map(delimited(tag("\""), opt(is_not("\"")), tag("\"")), |s| 
        Token::String(
            if let Some(s) = s {convert_u8_string(s)} else { String::new() }
        )
    )(input)
}

lazy_static! {
    static ref KEYWORDS_MAP : HashMap<String, Token> = {
        let mut map = HashMap::new();
        map.insert("and".to_owned(), Token::AND);
        map.insert("class".to_owned(), Token::CLASS);
        map.insert("else".to_owned(), Token::ELSE);
        map.insert("false".to_owned(), Token::FALSE);
        map.insert("fun".to_owned(), Token::FUN);
        map.insert("for".to_owned(), Token::FOR);
        map.insert("if".to_owned(), Token::IF); 
        map.insert("nil".to_owned(), Token::NIL);
        map.insert("or".to_owned(), Token::OR);
        map.insert("print".to_owned(), Token::PRINT);
        map.insert("return".to_owned(), Token::RETURN);
        map.insert("super".to_owned(), Token::SUPER);
        map.insert("this".to_owned(), Token::THIS);
        map.insert("true".to_owned(), Token::TRUE);
        map.insert("var".to_owned(), Token::VAR);
        map.insert("while".to_owned(), Token::WHILE);
        map
    };
}

fn identifier(input: &[u8]) -> IResult<&[u8], Token> {
    map(
        recognize(
            preceded(alt((tag("_"), alpha1)), alt((tag("_"), alphanumeric0)))),
        |s: &[u8]| {
            let s = String::from_utf8(s.into()).unwrap();
            if let Some(token) = KEYWORDS_MAP.get(&s) {
                token.clone()
            } else {
                Token::Identifier(s)
            }
        }
    )(input)
}

fn skip_white_space(input: &[u8]) -> IResult<&[u8], ()> {
    map(one_of(" \t\r\n"), |_| ())(input)
}

fn skip_line_comment(input: &[u8]) -> IResult<&[u8], ()> {
    map(preceded(tag("//"), is_not("\n")), |_| ())(input)
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
    many0(preceded(
        many0(alt((skip_white_space, skip_line_comment, skip_block_comment))), 
        get_a_token)
    )(input)
}


pub struct Scanner {
    source: String,
    result: Vec<Token>,
}
impl Scanner {
    pub fn scan(&mut self) -> IResult<&[u8], Vec<Token>> {
        let s = self.source.as_bytes();
        let (remain, vec) = get_tokens(s)?;
        self.result = vec;
        self.result.push(Token::EOF);
        Ok((remain, self.result.clone()))
    }
    pub fn new(source: String) -> Self {
        Scanner { source, result: vec![] }
    }
}

#[test]
fn test() {

    let mut sc = Scanner {
        source: r"andy formless fo _ _123 _abc ab123
        abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_
        ".to_owned(),
        result: vec![],
    };
    let _ = sc.scan();

    println!("{:?}", sc.result);
}

