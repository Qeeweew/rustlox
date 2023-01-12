use crate::token::{Token, TokenType};

trait Visitor<T> {
    fn visit(&mut self, e: &Expr) -> T;
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Unary(UnaryOp, Box<Expr>), // (op, expr)
    Binary(BinaryOp, Box<Expr>, Box<Expr>), // (op, left, right)
    Grouping(Box<Expr>),
}
#[derive(Debug)]
pub enum Literal {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}
#[derive(Debug)]
pub enum UnaryOp {
    Not,
    Neg,
}
impl UnaryOp {
    pub fn from_token(t: &Token) -> Self {
        if t.token_type == TokenType::Bang {
            Self::Not
        } else if t.token_type == TokenType::Minus {
            Self::Neg
        } else {
            panic!()
        }
    }

}
#[derive(Debug)]
pub enum BinaryOp {
    EQ, NEQ, LT, LE, GT, GE, ADD, SUB, MUL, DIV
}

impl BinaryOp {
    pub fn from_token(t: &Token) -> Self {
        match t.token_type {
            TokenType::Minus => Self::SUB,
            TokenType::Plus => Self::ADD,
            TokenType::Slash => Self::DIV,
            TokenType::Star => Self::MUL,
            TokenType::BangEqual => Self::NEQ,
            TokenType::EqualEqual => Self::EQ,
            TokenType::Greater => Self::GT, 
            TokenType::GreaterEqual => Self::GE,
            TokenType::Less => Self::LT,
            TokenType::LessEqual => Self::LE,
            _ => panic!("impossible")
        }
    }
}

struct PrintExpr {}
impl Visitor<String> for PrintExpr {
    fn visit(&mut self, e: &Expr) -> String {
        match e {
            Expr::Literal(n) => match n {
                Literal::Number(x) => x.to_string(),
                Literal::String(s) => s.clone(),
                Literal::Bool(b) => b.to_string(),
                Literal::Nil => "nil".into(),
            },
            Expr::Unary(op, expr) => {
                format!("({:?} {})", op, self.visit(expr))
            },
            Expr::Binary(op, left, right) => {
                format!("({} {:?} {})", self.visit(left), op, self.visit(right))
            },
            Expr::Grouping(expr) => {
                format!("(group {})", self.visit(expr))
            }
        }
    }
}
mod test {
    use super::*;

    #[test]
    fn test_expr_print() {
        let expr = Expr::Binary(BinaryOp::MUL, 
            Box::new(
                Expr::Unary(UnaryOp::Neg, 
                    Box::new(Expr::Literal(Literal::Number(123.0))))
            ), 
            Box::new(Expr::Grouping(
                Box::new(Expr::Literal(Literal::Number(45.67))
            ))
        ));
        let mut printer = PrintExpr{};
        println!("{}", printer.visit(&expr));
    }

}