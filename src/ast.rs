use crate::token::{Token, TokenType};

pub trait Visitor<T> {
    fn visit_expr(&mut self, e: &Expr) -> T;
}

#[derive(Debug)]
pub enum Expr {
    Literal(Object),
    Unary(UnaryOp, Box<Expr>), // (op, expr)
    Binary(BinaryOp, Box<Expr>, Box<Expr>), // (op, left, right)
    // Grouping(Box<Expr>),
}
#[derive(Debug, Clone, PartialEq)]
pub enum Object {
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
#[derive(Debug, PartialEq, Eq)]
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
    fn visit_expr(&mut self, e: &Expr) -> String {
        match e {
            Expr::Literal(n) => match n {
                Object::Number(x) => x.to_string(),
                Object::String(s) => s.clone(),
                Object::Bool(b) => b.to_string(),
                Object::Nil => "nil".into(),
            },
            Expr::Unary(op, expr) => {
                format!("({:?} {})", op, self.visit_expr(expr))
            },
            Expr::Binary(op, left, right) => {
                format!("({} {:?} {})", self.visit_expr(left), op, self.visit_expr(right))
            },
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
                    Box::new(Expr::Literal(Object::Number(123.0))))
            ), 
            Box::new(
                Expr::Literal(Object::Number(45.67)
            )
        ));
        let mut printer = PrintExpr{};
        println!("{}", printer.visit_expr(&expr));
    }

}