use nom::sequence::pair;

use crate::ast::*;

pub struct Interpreter {

}
#[derive(Debug)]
pub enum InterpreterError {
    ExprError(String),
    
}

fn is_truthy(o :&Object) -> bool {
    match o {
        Object::Number(_) => true,
        Object::String(_) => true,
        Object::Bool(b) => *b,
        Object::Nil => false,
    }
}

impl Visitor<Result<Object, InterpreterError>> for Interpreter {
    fn visit_expr(&mut self, e: &Expr) -> Result<Object, InterpreterError>{
        use InterpreterError::*;
        use Object::*;
        match e {
            Expr::Literal(o) => {
                Ok(o.clone())
            },
            Expr::Unary(op, expr) => {
                let val = self.visit_expr(&expr)?;
                match op {
                    UnaryOp::Not => {
                        return Ok(Bool(!is_truthy(&val)));
                    }
                    UnaryOp::Neg => {
                        if let Number(x) = val {
                            Ok(Number(-x))
                        } else {
                            Err(ExprError(format!("expected number type at {:?}", expr)))
                        }
                    },
                }
            }
            Expr::Binary(op,left, right) => {
                let left = self.visit_expr(&left)?;
                let right = self.visit_expr(&right)?;
                match op {
                    BinaryOp::EQ => {
                        Ok(Bool(left == right))
                    },
                    BinaryOp::NEQ => {
                        Ok(Bool(left != right))
                    },
                    BinaryOp::ADD => {
                        match (left, right) {
                            (Number(left), Number(right)) => Ok(Number(left + right)),
                            (String(left), String(right)) => Ok(String(left + &right)),
                            _ => Err(ExprError(format!("mismatched type at {:?}", e)))
                        }
                    },
                    BinaryOp::LT | BinaryOp::LE | BinaryOp::GT | BinaryOp::GE => {
                        if let (Number(left), Number(right)) = (left, right) {
                            Ok(Bool(
                                if *op == BinaryOp::LT {
                                    left < right
                                } else if *op == BinaryOp::LE {
                                    left <= right
                                } else if *op == BinaryOp::GT {
                                    left > right
                                } else {
                                    left >= right
                                }
                            ))
                        } else {
                            Err(ExprError(format!("mismatched type at {:?}", e)))
                        }
                    }
                    BinaryOp::SUB | BinaryOp::MUL | BinaryOp::DIV => {
                        if let (Number(left), Number(right)) = (left, right) {
                            Ok(Number(
                                if *op == BinaryOp::SUB {
                                    left - right
                                } else if *op == BinaryOp::MUL {
                                    left * right
                                } else {
                                    left / right
                                }
                            ))
                        } else {
                            Err(ExprError(format!("mismatched type at {:?}", e)))
                        }
                    }
                }
            }
        }
    }
}