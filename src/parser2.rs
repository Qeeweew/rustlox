use core::{str::{self}};
use std::collections::HashMap;

use crate::ast::{BinaryOp, UnaryOp, Expr};

use super::ast::*;

use nom::{
    IResult, 
    combinator::{map, recognize, verify}, 
    character::{
        complete::{one_of, digit1, }, 
        is_alphanumeric
    }, 
    branch::alt, 
    bytes::{complete::{tag, take, take_while, take_while_m_n}}, 
    sequence::{delimited, preceded, pair}, 
    multi::{many0, many_till}, 
    AsBytes
};

fn equality_op(input: &[u8]) -> IResult<&[u8], BinaryOp> {
    preceded(skip_all, alt((
        map(tag("!="), |_| BinaryOp::NEQ),
        map(tag("=="), |_| BinaryOp::EQ)
    )))(input)
}
fn comparsion_op(input: &[u8]) -> IResult<&[u8], BinaryOp> {
    preceded(skip_all, alt((
        map(tag(">"), |_| BinaryOp::GT),
        map(tag("<"), |_| BinaryOp::LT),
        map(tag(">="), |_| BinaryOp::GE),
        map(tag("<="), |_| BinaryOp::LE),
    )))(input)
}

fn term_op(input: &[u8]) -> IResult<&[u8], BinaryOp> {
    preceded(skip_all, alt((
        map(tag("-"), |_| BinaryOp::SUB),
        map(tag("+"), |_| BinaryOp::ADD),
    )))(input)
}
fn factor_op(input: &[u8]) -> IResult<&[u8], BinaryOp> {
    preceded(skip_all, alt((
        map(tag("/"), |_| BinaryOp::DIV),
        map(tag("*"), |_| BinaryOp::MUL),
    )))(input)
}
fn unary_op(input: &[u8]) -> IResult<&[u8], UnaryOp> {
    preceded(skip_all, alt((
        map(tag("!"), |_| UnaryOp::Not),
        map(tag("-"), |_| UnaryOp::Neg),
    )))(input)
}

fn primary(input: &[u8]) -> IResult<&[u8], Box<Expr>> {
    use Object::*;
    preceded(skip_all, alt((
        (map(parse_number, |i| Box::new(Expr::Literal(Number(i))))),
        (map(parse_string, |s| Box::new(Expr::Literal(String(s))))),
        (alt((
            map(parse_keywords("true".as_bytes()), |_| Box::new(Expr::Literal(Bool(true)))),
            map(parse_keywords("false".as_bytes()), |_| Box::new(Expr::Literal(Bool(false))))))),
        (map(parse_keywords("nil".as_bytes()), |_| Box::new(Expr::Literal(Nil)))),
        delimited(tag("("), expression, tag(")"))
    )))(input)
}

pub fn expression(input: &[u8]) -> IResult<&[u8], Box<Expr>> {
    delimited(skip_all, equality, skip_all)(input)
}

type ExprResult<'a> = IResult<&'a[u8], Box<Expr>>;
fn chainl<'a>(
    higher: impl Fn(&'a [u8]) -> ExprResult<'a> + Copy,
    get_op: impl Fn(&'a [u8]) -> IResult<&'a [u8], BinaryOp> + Copy
)
    -> impl Fn(&'a [u8]) -> ExprResult<'a>
{
    move |input| 
    preceded(skip_all, map(
        pair(higher, many0(pair(get_op, higher))),
        |(mut expr, v)| -> Box<Expr> {
            for (op, right) in v {
                expr = Box::new(Expr::Binary(op, expr, right));
            }
            expr
        }
    ))(input)
}

fn equality(input: &[u8]) -> ExprResult { chainl(comparison, equality_op)(input) }
fn comparison(input: &[u8]) -> ExprResult { chainl(term, comparsion_op)(input) }
fn term(input: &[u8]) -> ExprResult { chainl(factor, term_op)(input) }
fn factor(input: &[u8]) -> ExprResult { chainl(unary, factor_op)(input) }

fn unary(input: &[u8]) -> IResult<&[u8], Box<Expr>> {
    preceded(skip_all, alt((
        map(pair(unary_op, unary), |(op, expr)| Box::new(Expr::Unary(op, expr))),
        primary
    )))(input)
}

fn parse_number(input: &[u8]) -> IResult<&[u8], f64> {
    // [0-9]+.? [0-9]+
    map(recognize(alt((preceded(pair(digit1, tag(".")), digit1), digit1))),
        |s: &[u8]| {
            let s = convert_u8_string(s);
            s.parse::<f64>().unwrap()
        }
    )(input)
}
// ascii 
fn convert_u8_string(s: &[u8]) -> String { str::from_utf8(s).unwrap().to_owned() }

fn parse_string(input: &[u8]) -> IResult<&[u8], String> {
    //"[^"]+"
    map(delimited(tag("\""), take_while(|c| c != b'\"'), tag("\"")), |s| 
        convert_u8_string(s)
    )(input)
}

fn parse_keywords<'a>(word: &'a [u8]) -> 
    impl Fn(&'a [u8]) -> IResult<&'a[u8], ()> 
{
    move |input| 
        map(
            verify(
                take_while_m_n(word.len(), word.len() + 1, |c| is_alphanumeric(c) || c == b'_'), 
                |s: &[u8]| s == word), 
            |_| ()
        )(input)
}



// lazy_static! {
//     static ref KEYWORDS_MAP : HashMap<String, TokenType> = {
//         let mut map = HashMap::new();
//         map.insert("and".to_owned(), TokenType::AND);
//         map.insert("class".to_owned(), TokenType::CLASS);
//         map.insert("else".to_owned(), TokenType::ELSE);
//         map.insert("false".to_owned(), TokenType::FALSE);
//         map.insert("fun".to_owned(), TokenType::FUN);
//         map.insert("for".to_owned(), TokenType::FOR);
//         map.insert("if".to_owned(), TokenType::IF); 
//         map.insert("nil".to_owned(), TokenType::NIL);
//         map.insert("or".to_owned(), TokenType::OR);
//         map.insert("print".to_owned(), TokenType::PRINT);
//         map.insert("return".to_owned(), TokenType::RETURN);
//         map.insert("super".to_owned(), TokenType::SUPER);
//         map.insert("this".to_owned(), TokenType::THIS);
//         map.insert("true".to_owned(), TokenType::TRUE);
//         map.insert("var".to_owned(), TokenType::VAR);
//         map.insert("while".to_owned(), TokenType::WHILE);
//         map
//     };
// }

// fn identifier(input: &[u8]) -> IResult<&[u8], Token> {
//     map(
//         recognize(
//             preceded(take_while1(|c| is_alphabetic(c) || c == b'_'), take_while(|c| is_alphanumeric(c) || c == b'_'))),
//         |s: &[u8]| {
//             let s = String::from_utf8(s.into()).unwrap();
//             if let Some(&token) = KEYWORDS_MAP.get(&s) {
//                 Token::new(token)
//             } else {
//                 Token::new_literal(TokenType::Identifier, Box::new(s))
//             }
//         }
//     )(input)
// }

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
fn skip_all(input: &[u8]) -> IResult<&[u8],()> {
    map(many0(alt((skip_white_space, skip_line_comment, skip_block_comment))), |_| ()) (input)
}
