use core::{str::{self}};
use std::collections::{HashSet};

use crate::ast::{BinaryOp, UnaryOp, Expr};

use super::ast::*;

use nom::{
    IResult, 
    combinator::{map, recognize, verify, opt, map_res, cut}, 
    character::{
        complete::{one_of, digit1, }, 
        is_alphanumeric, is_alphabetic
    }, 
    branch::alt, 
    bytes::{complete::{take, take_while, take_while_m_n, take_while1, tag}}, 
    sequence::{delimited, preceded, pair, terminated}, 
    multi::{many0, many_till}, 
    AsBytes, error::{context, ErrorKind, VerboseError, FromExternalError}, error_position
};

macro_rules! skip {
    ($f: ident) => { delimited(skip_all, $f, skip_all) };
    ($f: expr) => { delimited(skip_all, $f, skip_all) };
}

fn equality_op(input: &[u8]) -> IResult<&[u8], BinaryOp> {
    alt((
        map(tag("!="), |_| BinaryOp::NEQ),
        map(tag("=="), |_| BinaryOp::EQ)
    ))(input)
}
fn comparsion_op(input: &[u8]) -> IResult<&[u8], BinaryOp> {
    alt((
        map(tag(">"), |_| BinaryOp::GT),
        map(tag("<"), |_| BinaryOp::LT),
        map(tag(">="), |_| BinaryOp::GE),
        map(tag("<="), |_| BinaryOp::LE),
    ))(input)
}

fn term_op(input: &[u8]) -> IResult<&[u8], BinaryOp> {
    alt((
        map(tag("-"), |_| BinaryOp::SUB),
        map(tag("+"), |_| BinaryOp::ADD),
    ))(input)
}
fn factor_op(input: &[u8]) -> IResult<&[u8], BinaryOp> {
    alt((
        map(tag("/"), |_| BinaryOp::DIV),
        map(tag("*"), |_| BinaryOp::MUL),
    ))(input)
}
fn unary_op(input: &[u8]) -> IResult<&[u8], UnaryOp> {
    alt((
        map(tag("!"), |_| UnaryOp::Not),
        map(tag("-"), |_| UnaryOp::Neg),
    ))(input)
}

fn primary(input: &[u8]) -> IResult<&[u8], Box<Expr>> {
    use Object::*;
    skip!(alt((
        (map(parse_number, |i| Box::new(Expr::Literal(Number(i))))),
        (map(parse_string, |s| Box::new(Expr::Literal(String(Box::new(s)))))),
        (map(identifier_or_keywords, |s| Box::new(
            if s == "true" {
                Expr::Literal(Bool(true))
            } else if s == "false" {
                Expr::Literal(Bool(false))
            } else if s == "nil" {
                Expr::Literal(Nil)
            } else {
                Expr::Varible(Box::new(s))
            }
        ))),
        delimited(tag("("), expression, tag(")"))
    )))(input)
}


type ExprResult<'a> = IResult<&'a[u8], Box<Expr>>;
type StmtResult<'a> = IResult<&'a[u8], Box<Stmt>>;
macro_rules! chainl {
    ($lower: ident, $higher: ident, $get_op: ident) => {
        fn $lower(input: &[u8]) -> ExprResult {
            skip!(map(
                pair($higher, many0(pair($get_op, $higher))),
                |(mut expr, v)| -> Box<Expr> {
                    for (op, right) in v {
                        expr = Box::new(Expr::Binary(op, expr, right));
                    }
                    expr
                }
            ))(input)
        }
    };
}
pub enum MyCustomError {
    ParserError(String),
}

pub fn expression(input: &[u8]) -> ExprResult { skip!(assinment)(input) }
pub fn assinment(input: &[u8]) -> ExprResult {
    let (next_input, (expr, eq)) = pair(equality, opt(preceded(tag("="), cut(assinment))))(input)?;
    if let Some(e) = eq {
        if let Expr::Varible(s) = *expr {
            Ok((next_input, Box::new(Expr::Assign(s, e))))
        } else {
            Err(nom::Err::Failure(nom::error::Error{input: next_input, code: ErrorKind::Satisfy}))
        }
    } else {
        Ok((next_input, expr))
    }
}

chainl!(equality, comparison, equality_op);
chainl!(comparison, term, comparsion_op);
chainl!(term, factor, term_op);
chainl!(factor, unary, factor_op);

fn unary(input: &[u8]) -> IResult<&[u8], Box<Expr>> {
    skip!(alt((
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
    map(preceded(tag("\""), cut(terminated(take_while(|c| c != b'\"'), tag("\"")))), |s| 
        convert_u8_string(s)
    )(input)
}

fn parse_keywords<'a>(word: &'a [u8]) -> 
    impl Fn(&'a [u8]) -> IResult<&'a[u8], ()> 
{
    move |input| 
        map(verify(
            take_while_m_n(word.len(), word.len() + 1, |c| is_alphanumeric(c) || c == b'_'), 
            |s: &[u8]| s == word), |_| ()
        )(input)
}

fn print_stmt(input: &[u8]) -> StmtResult {
    map(
        delimited(parse_keywords("print".as_bytes()), expression, tag(";")),
        |expr| Box::new(Stmt::PrintStmt(expr))
    )(input)
}
fn expr_stmt(input: &[u8]) -> StmtResult {
    map(
        terminated(expression, tag(";")),
        |expr| Box::new(Stmt::ExprStmt(expr))
    )(input)
}
fn var_decl(input: &[u8]) -> StmtResult {
    map(terminated(
            pair(
                preceded(tag("var"), preceded(skip_all, identifier)),
                skip!(opt(preceded(tag("="), expression)))),
            tag(";")),
        |(a,b)| Box::new(Stmt::Var(Box::new(a),if let Some(e) = b { e } else { Box::new(Expr::Literal(Object::Nil)) }))
    )(input)
}

fn statement(input: &[u8]) -> StmtResult {
    alt((print_stmt, expr_stmt))(input)
}

fn declaration(input: &[u8]) -> StmtResult {
    skip!(
        alt((var_decl,statement))
    )(input)
}

pub fn program(input: &[u8]) -> IResult<&[u8], Vec<Box<Stmt>>> {
    skip!(many0(declaration))(input)
}

lazy_static! {
    static ref KEYWORDS_SET : HashSet<String> = {
        let mut map = HashSet::new();
        map.insert("and".to_owned());
        map.insert("class".to_owned());
        map.insert("else".to_owned());
        map.insert("false".to_owned());
        map.insert("fun".to_owned());
        map.insert("for".to_owned());
        map.insert("if".to_owned());
        map.insert("nil".to_owned());
        map.insert("or".to_owned());
        map.insert("print".to_owned());
        map.insert("return".to_owned());
        map.insert("super".to_owned());
        map.insert("this".to_owned());
        map.insert("true".to_owned());
        map.insert("var".to_owned());
        map.insert("while".to_owned());
        map
    };
}

fn identifier_or_keywords(input: &[u8]) -> IResult<&[u8], String> {
    map(recognize(
            preceded(take_while1(|c| is_alphabetic(c) || c == b'_'), take_while(|c| is_alphanumeric(c) || c == b'_'))),
        convert_u8_string
    )(input)
}

fn identifier(input: &[u8]) -> IResult<&[u8], String> {
    verify(identifier_or_keywords, |s:&String| !KEYWORDS_SET.contains(s))(input)
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
fn skip_all(input: &[u8]) -> IResult<&[u8],()> {
    map(many0(alt((skip_white_space, skip_line_comment, skip_block_comment))), |_| ()) (input)
}
