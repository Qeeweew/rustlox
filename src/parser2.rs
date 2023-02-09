use core::{str::{self}};
use std::{collections::{HashSet}};

use super::ast::*;
use super::object::*;

use nom::{
    IResult, 
    combinator::{map, recognize, verify, opt, cut, value}, 
    character::{
        complete::{one_of, digit1, }, 
        is_alphanumeric, is_alphabetic
    }, 
    branch::alt, 
    bytes::{complete::{take, take_while, take_while_m_n, take_while1, tag}}, 
    sequence::{delimited, preceded, pair, terminated, tuple}, 
    multi::{many0, many_till, separated_list0}, 
    AsBytes, 
    error::{ErrorKind}
};

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
    alt((
        map(parse_number, |i| Expr::Literal(Number(i))),
        map(parse_string, |s| Expr::Literal(String(Box::new(s)))),
        map(pair(terminated(tag("super"), tuple((sp, tag("."), sp))), identifier), |(s0, s1)| 
            Expr::Super(Identifier::new(convert_u8_string(s0)), s1)
        ),
        map(identifier_or_keywords, |s| 
            if s == "true" {
                Expr::Literal(Bool(true))
            } else if s == "false" {
                Expr::Literal(Bool(false))
            } else if s == "nil" {
                Expr::Literal(Nil)
            } else if s == "this"{
                Expr::This(Identifier::new(s))
            } else {
                Expr::Varible(Identifier::new(s))
            }
        ),
        delimited(pair(tag("("), sp), expression, pair(sp, tag(")"))
    )))(input)
}


macro_rules! chainl {
    ($lower: ident, $higher: ident, $get_op: ident) => {
        fn $lower(input: &[u8]) -> ExprResult {
            (map(
                pair($higher, many0(pair(delimited(sp, $get_op, sp), $higher))),
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

pub fn expression(input: &[u8]) -> ExprResult { delimited(sp, assignment, sp)(input) }
pub fn assignment(input: &[u8]) -> ExprResult {
    let (next_input, (expr, eq)) = pair(terminated(logic_or, sp), opt(preceded(pair(tag("="), sp), cut(assignment))))(input)?;
    if let Some(value) = eq {
        match expr {
            Expr::Varible(s) => Ok((next_input, Expr::Assign(s, Box::new(value)))),
            Expr::Get(e, s) => Ok((next_input, Expr::Set(e, s, Box::new(value)))),
            _ => Err(nom::Err::Failure(nom::error::Error{input: next_input, code: ErrorKind::Satisfy}))
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
        map(pair(terminated(unary_op, sp), cut(unary)), |(op, expr)| Expr::Unary(op, Box::new(expr))),
        call
    ))(input)
}

fn call(input: &[u8]) -> ExprResult {
    let (mut next_input, mut expr) = terminated(primary, sp)(input)?;
    while let (tmp, Some(char)) = opt(terminated(alt((tag("("), tag("."))), sp))(next_input)? {
        if char == b"(" {
            let (tmp, arguments) = cut(terminated(separated_list0(tag(","), expression), pair(sp, tag(")"))))(tmp)?;
            expr = Expr::Call(Box::new(expr), arguments);
            (next_input, _) = sp(tmp)?;
        } else {
            let (tmp, ident) = cut(identifier)(tmp)?;
            expr = Expr::Get(Box::new(expr), *ident.name);
            (next_input, _) = sp(tmp)?;
        }
    }
    Ok((next_input, expr))
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
pub fn convert_u8_string(s: &[u8]) -> String { str::from_utf8(s).unwrap().to_owned() }

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
                preceded(tag("var"), delimited(sp, identifier, sp)),
                opt(preceded(tag("="), expression))),
            pair(sp, tag(";"))),
        |(a,b)| Stmt::Var(a, b),
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
                preceded(tag("if"), sp),
                cut(delimited(tag("("), expression, tag(")"))),
            ),
            cut(delimited(sp, statement, sp)),
            opt(preceded(tag("else"), cut(preceded(sp, statement))))
        )),
        |(cond, then_branch, else_branch)|
            Stmt::If(cond, Box::new(then_branch), else_branch.map(Box::new))
    )(input)
}

fn while_stmt(input: &[u8]) -> StmtResult {
    map(
        preceded(
            preceded(tag("while"), sp),
            pair(
                cut(delimited(tag("("), expression, tag(")"))),
                preceded(sp, statement)
            )
        ),
        |(expr, stmt)| Stmt::While(expr, Box::new(stmt))
    )(input)
}

fn for_stmt(input: &[u8]) -> StmtResult {
    map(preceded(
        pair(tag("for"), sp),
        cut(tuple((
            preceded(
                pair(tag("("), sp), 
                alt((
                    map(var_decl, Some),
                    map(tag(";"), |_| None),
                    map(expr_stmt, Some),
                ))
            ),
            delimited(sp, opt(expression), tag(";")),
            delimited(sp, opt(expression), tag(")")),
            preceded(sp, statement)
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
        delimited(tag("return"), opt(expression), pair(sp, tag(";"))),
        |expr| Stmt::Return(if let Some(expr) = expr { expr } else {Expr::Literal(Object::Nil)})
    )(input)
}

fn statement(input: &[u8]) -> StmtResult {
    alt((print_stmt, block_stmt, if_stmt, while_stmt, for_stmt, return_stmt, expr_stmt))(input)
}

fn fun_decl(input: &[u8]) -> StmtResult {
    map(preceded(pair(tag("fun"), sp), function), Stmt::Func)(input)
}

fn function(input: &[u8]) -> IResult<&[u8], FunctionBody>{
    map(tuple((
        terminated(identifier, pair(sp, tag("("))),
        separated_list0(tag(","), delimited(sp, identifier, sp)),
        preceded(delimited(sp, tag(")"),sp), delimited(tag("{"), many0(declaration), cut(tag("}"))))
    )),
        |(ident, params, body)| 
        FunctionBody { ident, params, body}
    )(input)
}

fn class_decl(input: &[u8]) -> StmtResult {
    map(
        terminated(tuple((
                delimited(pair(tag("class"), sp), identifier, sp), 
                opt(preceded(pair(tag("<"),sp), identifier)),
                preceded(pair(sp, tag("{")), many0(preceded(sp, function))),
            )),
            pair(sp, tag("}"))
        ), 
        |(ident, super_class, funcs)| Stmt::Class(ident, super_class, funcs)
    )(input)
}

fn declaration(input: &[u8]) -> StmtResult {
    terminated(
        alt((var_decl, fun_decl, class_decl, statement)),sp
    )(input)
}

pub fn program(input: &[u8]) -> IResult<&[u8], Vec<Stmt>> {
    preceded(sp, many0(declaration))(input)
}


fn identifier_or_keywords(input: &[u8]) -> IResult<&[u8], String> {
    map(recognize(
            preceded(take_while1(|c| is_alphabetic(c) || c == b'_'), take_while(|c| is_alphanumeric(c) || c == b'_'))),
        convert_u8_string
    )(input)
}

fn identifier(input: &[u8]) -> IResult<&[u8], Identifier> {
    map(verify(identifier_or_keywords, |s:&String| !KEYWORDS_SET.contains(s)), Identifier::new)(input)
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
fn sp(input: &[u8]) -> IResult<&[u8],()> {
    map(many0(alt((skip_white_space, skip_line_comment, skip_block_comment))), |_| ()) (input)
}
