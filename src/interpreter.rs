use core::mem;
use core::{cell::RefCell, fmt::Debug};
use std::collections::HashMap;
use alloc::fmt::format;
use alloc::rc::Rc;

use super::ast::*;
use super::object::*;

pub struct Environment {
    values: HashMap<String, Object>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(parent: Option<Rc<RefCell<Environment>>>) -> Self {
        Self { values: HashMap::new(), parent}
    }
    pub fn define(&mut self, name: &String, o :Object) {
        self.values.insert(name.clone(), o);
    }

    pub fn get(&self, name: &String) -> Result<Object, RuntimeError> {
        if let Some(x) = self.values.get(name) {
            Ok(x.clone())
        } else if let Some(pa) = &self.parent {
            pa.borrow().get(name)
        } else {
            Err(RuntimeError::ExprError(format!("can not find { } in the environment", name)))
        }
    }

    pub fn assign(&mut self, name: &String, o :Object) -> Result<(), RuntimeError>{
        if let Some(x) = self.values.get_mut(name) {
            *x = o;
            Ok(())
        } else if let Some(pa) = &self.parent {
            pa.borrow_mut().assign(name, o)
        } else {
            Err(RuntimeError::ExprError("Undefined variable".into()))
        }
    }
}


pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
    globals: Rc<RefCell<Environment>>,
    return_object: Option<Object>,
}

#[derive(Debug)]
pub enum RuntimeError {
    ExprError(String),
}

impl Visitor<Result<Object, RuntimeError>, Result<(), RuntimeError>> for Interpreter {
    fn visit_expr(&mut self, e: &Expr) -> Result<Object, RuntimeError>{
        use RuntimeError::*;
        use Object::*;
        match e {
            Expr::Literal(o) => { Ok(o.clone()) },
            Expr::Unary(op, expr) => {
                let val = self.visit_expr(&expr)?;
                match op {
                    UnaryOp::Not => Ok(Bool(!val.is_truthy())),
                    UnaryOp::Neg => {
                        if let Number(x) = val {
                            Ok(Number(-x))
                        } else {
                            Err(ExprError(format!("expected number type at {}", expr)))
                        }
                    },
                }
            }
            Expr::Binary(op,left, right) => {
                let left = self.visit_expr(&left)?;
                let right = self.visit_expr(&right)?;
                match op {
                    BinaryOp::EQ => Ok(Bool(left == right)),
                    BinaryOp::NEQ => Ok(Bool(left != right)),
                    BinaryOp::ADD => {
                        match (left, right) {
                            (Number(left), Number(right)) => Ok(Number(left + right)),
                            (String(left), String(right)) => Ok(String(Box::new(*left.clone() + &right))),
                            _ => Err(ExprError(format!("mismatched type at {}", e)))
                        }
                    },
                    BinaryOp::LT | BinaryOp::LE | BinaryOp::GT | BinaryOp::GE => {
                        if let (Number(left), Number(right)) = (left, right) {
                            Ok(Bool(
                                if *op == BinaryOp::LT { left < right } else 
                                if *op == BinaryOp::LE { left <= right} else 
                                if *op == BinaryOp::GT { left > right } else 
                                { left >= right }
                            ))
                        } else {
                            Err(ExprError(format!("mismatched type at {}", e)))
                        }
                    }
                    BinaryOp::SUB | BinaryOp::MUL | BinaryOp::DIV => {
                        if let (Number(left), Number(right)) = (left, right) {
                            Ok(Number(
                                if *op == BinaryOp::SUB { left - right } else 
                                if *op == BinaryOp::MUL { left * right } else 
                                { left / right }
                            ))
                        } else {
                            Err(ExprError(format!("mismatched type at {}", e)))
                        }
                    }
                    // short cut
                    BinaryOp::OR => Ok(if left.is_truthy() { left.clone() } else { right.clone() }),
                    BinaryOp::AND => Ok(if !left.is_truthy() { left.clone() } else { right.clone() })
                }
            }
            Expr::Varible(v) => {
                self.env.borrow().get(v)
            }
            Expr::Assign(s, e) => {
                let val = self.visit_expr(e)?;
                self.env.borrow_mut().assign(s, val.clone())?;
                Ok(val.clone())
            }
            Expr::Call(expr, arguments) => {
                let callee = self.visit_expr(&expr)?;
                let mut args = vec![];
                for arg in arguments {
                    args.push(self.visit_expr(&arg)?);
                }
                let f = callee.to_callable()?;
                if f.arity() != args.len() {
                    return Err(RuntimeError::ExprError(format!("wrong number of arguments")));
                }
                f.call(self, &args)?;
                if let Some(o) = mem::replace(&mut self.return_object, None) {
                    Ok(o)
                } else {
                    Ok(Object::Nil)
                }
            }
        }
    }

    fn visit_stmt(&mut self, s: &Stmt) -> Result<(), RuntimeError> {
        match s {
            Stmt::Expression(e) => {
                self.visit_expr(e)?;
                Ok(())
            }
            Stmt::Print(e) => {
                let x = self.visit_expr(e)?;
                println!("{}", x);
                Ok(())
            },
            Stmt::Var(name ,e) => {
                let val = self.visit_expr(e)?;
                self.env.borrow_mut().define(&name, val);
                Ok(())
            }
            Stmt::Block(v) => {
                self.execute_block(v, Environment::new(Some(self.env.clone())))
            }
            Stmt::If(cond, then_branch, else_branch) => {
                let cond = self.visit_expr(&cond)?;
                if cond.is_truthy() {
                    self.visit_stmt(&then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    self.visit_stmt(&else_branch)?;
                }
                Ok(())
            }
            Stmt::While(cond, stmt) => {
                loop {
                    let cond = self.visit_expr(&cond)?;
                    if cond.is_truthy() {
                        self.visit_stmt(&stmt)?;
                    } else {
                        break;
                    }
                    if self.return_object.is_some() {
                        break;
                    }
                }
                Ok(())
            }
            Stmt::Function { name, params, body } => {
                self.env.borrow_mut().define(name, Object::Function(
                    Box::new(LoxFunction::new(name.clone(), params.clone(), body.clone(), self.env.clone()))
                ));
                Ok(())
            }
            Stmt::Return(expr) => {
                self.return_object = Some(self.visit_expr(expr)?);
                return Ok(());
            }
        }
    }
}

impl Interpreter {
    pub fn new() -> Self {
        let env = Rc::new(RefCell::new(Environment::new(None)));
        let globals = env.clone();
        Interpreter { 
            env,
            globals,
            return_object: None,
        }
    }
    pub fn get_global(&self) -> Rc<RefCell<Environment>> {
        self.globals.clone()
    }
    pub fn interpret(&mut self, statements: &Vec<Stmt>) -> Result<(), RuntimeError> {
        for statement in statements {
            self.visit_stmt(statement)?;
        }
        Ok(())
    }

    pub fn execute_block(&mut self, v: &Vec<Stmt>, env: Environment) -> Result<(), RuntimeError> {
        let previous = self.env.clone();
        self.env = Rc::new(RefCell::new(env));
        for stmt in v {
            self.visit_stmt(stmt)?;
            if self.return_object.is_some() {
                break;
            }
        }
        self.env = previous;
        Ok(())
    }
}