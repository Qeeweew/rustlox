use super::ast::*;
use super::object::*;

use pest::Parser;
#[derive(Parser)]
#[grammar = "loxxol.pest"]
struct LoxParser;
use pest::error::Error;

pub fn parse(s: &str) -> Result<Vec<Stmt>, Error<Rule>> {
    use pest::iterators::Pair;
    use pest::iterators::Pairs;
    fn parse_expression(pair: Pair<Rule>) -> Expr {
        let mut inner = pair.into_inner();
        let first = inner.next().unwrap();
        match first.as_rule() {
            Rule::call => {
                let left_value = parse_call(first);
                let right_value = Box::new(parse_expression(inner.next().unwrap()));
                match left_value {
                    Expr::Varible(s) => Expr::Assign(s, right_value),
                    Expr::Get(e, s) => Expr::Set(e, s, right_value),
                    _ => panic!("invaild left value")
                }
            },
            Rule::binary => {
                inner = first.into_inner();
                parse_binary(&mut inner, 0)
            },
            _ => unreachable!()
        }
    }
    
    fn get_precedence(op: BinaryOp) -> u8 {
        use BinaryOp::*;
        match op {
            OR | AND => 1,
            NEQ | EQ => 2,
            LT | LE | GT | GE => 3,
            ADD | SUB => 4,
            MUL | DIV => 5,
        }
    }
    fn get_binary_op(op: &str) -> BinaryOp {
        use BinaryOp::*;
        match op {
            "or" => OR,
            "and" => AND,
            "!=" => NEQ,
            "==" => EQ,
            ">=" => GE,
            ">" => GT,
            "<=" => LE,
            "<" => LT,
            "-" => SUB,
            "+" => ADD,
            "/" => DIV,
            "*" => MUL,
            _ => unreachable!()
        }
    }

    fn parse_binary(pairs: &mut Pairs<Rule>, min_precedence: u8) -> Expr {
        let mut lhs = parse_unary(pairs.next().unwrap());
        while let Some(op) = pairs.peek() {
            let op = get_binary_op(op.as_str());
            let cur_precedence = get_precedence(op);
            if cur_precedence <= min_precedence {
                break;
            }
            pairs.next();
            let rhs = parse_binary(pairs, cur_precedence);
            lhs = Expr::Binary(op, Box::new(lhs), Box::new(rhs))
        }
        lhs
    }
    
    fn parse_unary(pair: Pair<Rule>) -> Expr {
        let mut inner = pair.into_inner();
        let first = inner.next().unwrap();
        match first.as_rule() {
            Rule::unary_op => {
                let value = Box::new(parse_expression(inner.next().unwrap()));
                Expr::Unary(
                    if first.as_str() == "-" { UnaryOp::Neg } else { UnaryOp::Not },
                    value
                )
            },
            Rule::call => parse_call(first),
            _ => unreachable!()
        }
    }
    
    fn parse_call(pair: Pair<Rule>) -> Expr {
        let mut inner = pair.into_inner();
        let mut expr = parse_primary(inner.next().unwrap());
        while let Some(x) = inner.next() {
            match x.as_rule() {
                Rule::arguments => expr = Expr::Call(Box::new(expr), 
                    x.into_inner().map(|x| parse_expression(x)).collect()
                ),
                Rule::identifier => expr = Expr::Get(Box::new(expr), x.as_str().to_owned()),
                _ => unreachable!(),
            }
        }
        expr
    }
    
    fn parse_primary(pair: Pair<Rule>) -> Expr {
        let mut inner = pair.into_inner();
        let first = inner.next().unwrap();
        match first.as_rule() {
            Rule::primary_keywords => {
                match first.as_str() {
                    "true" => Expr::Literal(Object::Bool(true)),
                    "false" => Expr::Literal(Object::Bool(false)),
                    "nil" => Expr::Literal(Object::Nil),
                    "this" => Expr::This(Identifier::new("this".to_owned())),
                    _ => unreachable!()
                }
            }
            Rule::number => Expr::Literal(Object::Number(first.as_str().parse().unwrap())),
            Rule::string => Expr::Literal(Object::String(Box::new(first.as_str().to_owned()))),
            Rule::super_dot => Expr::Super(Identifier::new("super".to_owned()), parse_identifier(inner.next().unwrap())),
            Rule::assignment => parse_expression(first),
            Rule::identifier => Expr::Varible(parse_identifier(first)),
            _ => unreachable!()
        }
    }
    
    fn parse_identifier(pair: Pair<Rule>) -> Identifier {
        Identifier::new(pair.as_str().to_owned())
    }
    
    fn parse_function(pair: Pair<Rule>) -> FunctionBody {
        let mut inner = pair.into_inner();
        let ident = parse_identifier(inner.next().unwrap());
        let params = inner.next().unwrap().into_inner().map(parse_identifier).collect();
        let body = inner.next().unwrap().into_inner().map(parse_statement).collect();
        FunctionBody { ident, params, body}
    }

    fn parse_statement(pair: Pair<Rule>) -> Stmt {
        let rule = pair.as_rule();
        let mut inner = pair.into_inner();
        match rule {
            Rule::forStmt => {
                let stmt1 = inner.next().unwrap().into_inner().next().map(parse_statement);
                let expr1 = inner.next().unwrap().into_inner().next().map(parse_expression);
                let expr2 = inner.next().unwrap().into_inner().next().map(parse_expression);
                let stmt2 = parse_statement(inner.next().unwrap());
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
            },
            Rule::ifStmt => {
                let cond = parse_expression(inner.next().unwrap());
                let stmt_true = Box::new(parse_statement(inner.next().unwrap()));
                let stmt_false = inner.next().map(|x| Box::new(parse_statement(x)));
                Stmt::If(cond, stmt_true, stmt_false)
            },
            Rule::printStmt => {
                Stmt::Print(parse_expression(inner.next().unwrap()))
            },
            Rule::returnStmt => {
                Stmt::Return(parse_expression(inner.next().unwrap()))
            }
            Rule::whileStmt => {
                let cond = parse_expression(inner.next().unwrap());
                let stmt = parse_statement(inner.next().unwrap());
                Stmt::While(cond, Box::new(stmt))
            },
            Rule::block => {
                Stmt::Block(inner.map(parse_statement).collect())
            },
            Rule::expression => {
                Stmt::Expression(parse_expression(inner.next().unwrap()))
            },
            Rule::classDecl => {
                let name = parse_identifier(inner.next().unwrap());
                let super_class = inner.next().unwrap().into_inner().next().map(parse_identifier);
                let body = inner.map(parse_function).collect();
                Stmt::Class(name, super_class, body)
            }
            Rule::funDecl => {
                Stmt::Func(parse_function(inner.next().unwrap()))
            }
            Rule::varDecl => {
                let name = parse_identifier(inner.next().unwrap());
                let expr = inner.next().map(parse_expression);
                Stmt::Var(name, expr)
            }
            Rule::exprStmt => {
                Stmt::Expression(parse_expression(inner.next().unwrap()))
            }
            _ => unreachable!()
        }
    }
    
    fn parse_program(pairs: Pairs<Rule>) -> Vec<Stmt> {
        let mut res = vec![];
        for x in pairs {
            if x.as_rule() != Rule::EOI {
                res.push(parse_statement(x));
            }
        }
        res
    }
    let program = LoxParser::parse(Rule::program, s)?;
    Ok(parse_program(program))
}