use core::{str::{self}};
use std::{collections::{HashSet}};

use super::ast::*;
use super::object::*;

use alloc::rc::Rc;
use nom::{
    IResult, 
    combinator::{map, recognize, verify, opt, map_res, cut, value}, 
    character::{
        complete::{one_of, digit1, }, 
        is_alphanumeric, is_alphabetic
    }, 
    branch::alt, 
    bytes::{complete::{take, take_while, take_while_m_n, take_while1, tag}}, 
    sequence::{delimited, preceded, pair, terminated, tuple}, 
    multi::{many0, many_till, separated_list1, separated_list0}, 
    AsBytes, error::{context, ErrorKind, VerboseError, FromExternalError}, error_position
};

macro_rules! skip {
    ($f: ident) => { delimited(skip_all, $f, skip_all) };
    ($f: expr) => { delimited(skip_all, $f, skip_all) };
}

fn equality_op(input: &[u8]) -> IResult<&[u8], BinaryOp> {
    alt((
        value(BinaryOp::NEQ, tag("!=")),
        value(BinaryOp::EQ, tag("=="))
    ))(input)
}
fn comparsion_op(input: &[u8]) -> IResult<&[u8], BinaryOp> {
    alt((
        value(BinaryOp::GE, tag(">=")),
        value(BinaryOp::LE, tag("<=")),
        value(BinaryOp::GT, tag(">")),
        value(BinaryOp::LT, tag("<")),
    ))(input)
}

fn term_op(input: &[u8]) -> IResult<&[u8], BinaryOp> {
    alt((
        value(BinaryOp::SUB, tag("-")),
        value(BinaryOp::ADD, tag("+")),
    ))(input)
}
fn factor_op(input: &[u8]) -> IResult<&[u8], BinaryOp> {
    alt((
        value(BinaryOp::DIV, tag("/")),
        value(BinaryOp::MUL, tag("*")),
    ))(input)
}
fn unary_op(input: &[u8]) -> IResult<&[u8], UnaryOp> {
    alt((
        value(UnaryOp::Not, tag("!")),
        value(UnaryOp::Neg, tag("-"))
    ))(input)
}
fn or_op(input: &[u8]) -> IResult<&[u8], BinaryOp> {
    value(BinaryOp::OR, parse_keywords("or".as_bytes()))(input)
}
fn and_op(input: &[u8]) -> IResult<&[u8], BinaryOp> {
    value(BinaryOp::AND, parse_keywords("and".as_bytes()))(input)
}


type ExprResult<'a> = IResult<&'a[u8], Expr>;
type StmtResult<'a> = IResult<&'a[u8], Stmt>;

fn primary(input: &[u8]) -> ExprResult {
    use Object::*;
    skip!(alt((
        map(parse_number, |i| Expr::Literal(Number(i))),
        map(parse_string, |s| Expr::Literal(String(Box::new(s)))),
        map(identifier_or_keywords, |s| 
            if s == "true" {
                Expr::Literal(Bool(true))
            } else if s == "false" {
                Expr::Literal(Bool(false))
            } else if s == "nil" {
                Expr::Literal(Nil)
            } else {
                Expr::Varible(s)
            }
        ),
        delimited(tag("("), expression, tag(")"))
    )))(input)
}


macro_rules! chainl {
    ($lower: ident, $higher: ident, $get_op: ident) => {
        fn $lower(input: &[u8]) -> ExprResult {
            (map(
                pair($higher, many0(pair(delimited(skip_all, $get_op, skip_all), $higher))),
                |(mut expr, v)| -> Expr {
                    for (op, right) in v {
                        expr = Expr::Binary(op, Box::new(expr), Box::new(right));
                    }
                    expr
                }
            ))(input)
        }
    };
}

pub fn expression(input: &[u8]) -> ExprResult { skip!(assinment)(input) }
pub fn assinment(input: &[u8]) -> ExprResult {
    let (next_input, (expr, eq)) = pair(logic_or, opt(preceded(tag("="), cut(assinment))))(input)?;
    if let Some(e) = eq {
        if let Expr::Varible(s) = expr {
            Ok((next_input, Expr::Assign(s, Box::new(e))))
        } else {
            Err(nom::Err::Failure(nom::error::Error{input: next_input, code: ErrorKind::Satisfy}))
        }
    } else {
        Ok((next_input, expr))
    }
}

chainl!(logic_or, logic_and, or_op);
chainl!(logic_and, equality, and_op);
chainl!(equality, comparison, equality_op);
chainl!(comparison, term, comparsion_op);
chainl!(term, factor, term_op);
chainl!(factor, unary, factor_op);

fn unary(input: &[u8]) -> ExprResult {
    alt((
        map(pair(unary_op, unary), |(op, expr)| Expr::Unary(op, Box::new(expr))),
        call
    ))(input)
}

fn call(input: &[u8]) -> ExprResult {
    map(
        pair(primary, many0(delimited(delimited(skip_all, tag("("), skip_all), separated_list0(tag(","), expression), cut(tag(")"))))),
        |(mut expr, arguments_list)| -> Expr {
            for arguments in arguments_list {
                expr = Expr::Call(Box::new(expr), arguments)
            }
            expr
        }
    )(input)
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
        |expr| Stmt::Print(expr)
    )(input)
}
fn expr_stmt(input: &[u8]) -> StmtResult {
    map(
        terminated(expression, tag(";")),
        |expr| Stmt::Expression(expr)
    )(input)
}
fn var_decl(input: &[u8]) -> StmtResult {
    map(terminated(
            pair(
                preceded(tag("var"), preceded(skip_all, identifier)),
                skip!(opt(preceded(tag("="), expression)))),
            tag(";")),
        |(a,b)| Stmt::Var(a,if let Some(e) = b { e } else { Expr::Literal(Object::Nil) })
    )(input)
}

fn block_stmt(input: &[u8]) -> StmtResult {
    map(
        delimited(tag("{"), many0(declaration), cut(tag("}"))),
        |v| Stmt::Block(v)
    )(input)
}

fn if_stmt(input: &[u8]) -> StmtResult {
    map(
        tuple((
            preceded(
                preceded(tag("if"), skip_all),
                cut(delimited(tag("("), expression, tag(")"))),
            ),
            cut(delimited(skip_all, statement, skip_all)),
            opt(preceded(tag("else"), cut(preceded(skip_all, statement))))
        )),
        |(cond, then_branch, else_branch)|
            Stmt::If(cond, Box::new(then_branch), else_branch.map(Box::new))
    )(input)
}

fn while_stmt(input: &[u8]) -> StmtResult {
    map(
        preceded(
            preceded(tag("while"), skip_all),
            pair(
                cut(delimited(tag("("), expression, tag(")"))),
                preceded(skip_all, statement)
            )
        ),
        |(expr, stmt)| Stmt::While(expr, Box::new(stmt))
    )(input)
}

fn for_stmt(input: &[u8]) -> StmtResult {
    map(preceded(
        pair(tag("for"), skip_all),
        cut(tuple((
            preceded(
                pair(tag("("), skip_all), 
                alt((
                    map(var_decl, Some),
                    map(tag(";"), |_| None),
                    map(expr_stmt, Some),
                ))
            ),
            delimited(skip_all, opt(expression), tag(";")),
            delimited(skip_all, opt(expression), tag(")")),
            preceded(skip_all, statement)
        )))),
        |(stmt1, expr1, expr2, stmt2)| -> Stmt {
            let mut while_block = vec![stmt2];
            if let Some(expr) = expr2 {
                while_block.push(Stmt::Expression(expr));
            }
            let cond = if let Some(expr1) = expr1 { expr1 } else { Expr::Literal(Object::Bool(true))};
            let body = Stmt::While(cond,Box::new(Stmt::Block(while_block)));
            if let Some(stmt) = stmt1 {
                Stmt::Block(vec![stmt, body])
            } else {
                body
            }
        }
    )(input)
}

fn return_stmt(input: &[u8]) -> StmtResult {
    map(
        delimited(tag("return"), opt(expression), pair(skip_all, tag(";"))),
        |expr| Stmt::Return(if let Some(expr) = expr { expr } else {Expr::Literal(Object::Nil)})
    )(input)
}

fn statement(input: &[u8]) -> StmtResult {
    alt((print_stmt, block_stmt, if_stmt, while_stmt, for_stmt, return_stmt, expr_stmt))(input)
}

fn fun_decl(input: &[u8]) -> StmtResult {
    preceded(pair(tag("fun"), skip_all), function)(input)
}

fn function(input: &[u8]) -> StmtResult {
    map(tuple((
        terminated(identifier, pair(skip_all, tag("("))),
        separated_list0(tag(","), delimited(skip_all, identifier, skip_all)),
        preceded(delimited(skip_all, tag(")"),skip_all), delimited(tag("{"), many0(declaration), cut(tag("}"))))
    )),
        |(name, params, body)| Stmt::Function { name, params, body: Rc::new(body)}
    )(input)
}

fn declaration(input: &[u8]) -> StmtResult {
    skip!(
        alt((var_decl, fun_decl, statement))
    )(input)
}

pub fn program(input: &[u8]) -> IResult<&[u8], Vec<Stmt>> {
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

fn skip_white_space(input: &[u8]) -> IResult<&[u8], ()> {
    value((), one_of(" \t\r\n"))(input)
}

fn skip_line_comment(input: &[u8]) -> IResult<&[u8], ()> {
    value((), preceded(tag("//"), take_while(|c| c != b'\n')))(input)
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
