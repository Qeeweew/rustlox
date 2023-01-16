use core::{cell::RefCell, fmt::Debug};
use std::collections::HashMap;
use alloc::rc::Rc;

use super::ast::*;
use super::object::*;
use super::resolver::*;

#[derive(Debug)]
pub enum InterpreterError {
    RuntimeError(String),
    ResolverError(String),
}
use InterpreterError::*;

pub struct Environment {
    values: Vec<Object>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new_global(builtin_functions: &Vec<RustFunction>) -> Self {
        let mut values = vec![];
        for f in builtin_functions {
            values.push(Object::BuitlinFunc(Box::new(f.clone())));
        }
        Environment { values, parent: None }
    }
    pub fn new(parent: Option<Rc<RefCell<Environment>>>) -> Self {
        Self { values: Vec::new(), parent}
    }
    pub fn define_this(&mut self, instace: Rc<RefCell<LoxInstance>>) {
        self.values.push(Object::Instance(instace))
    }
    pub fn define(&mut self, ident: &Identifier, o: Object) {
        assert!(ident.id != INF);
        assert!(ident.env_depth == 0);
        while self.values.len() <= ident.id {
            self.values.push(Object::Nil);
        }
        self.values[ident.id] = o;
    }
    pub fn assign(&mut self, ident: &Identifier, o: Object) {
        self.set(ident.env_depth, ident.id, o);
    }

    pub fn get_(&self, dep: usize, i: usize) -> Object {
        if dep == 0 {
            self.values[i].clone()
        } else {
            if let Some(parent) = &self.parent {
                parent.clone().borrow_mut().get_(dep - 1, i)
            } else {
                panic!("???")
            }
        }
    }
    pub fn get(&self, ident: &Identifier) -> Object {
        self.get_(ident.env_depth, ident.id)
    }

    pub fn set(&mut self, dep: usize, i: usize, o :Object) {
        if dep == 0 {
            self.values[i] = o;
        } else {
            if let Some(parent) = &self.parent {
                parent.borrow_mut().set(dep - 1, i, o)
            } else {
                panic!("???")
            }
        }
    }
}


pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
    globals: Rc<RefCell<Environment>>,
    pub return_object: Option<Object>,
    builtin_functions: Vec<RustFunction>,
}

impl Interpreter {
    fn visit_expr(&mut self, e: &Expr) -> Result<Object, InterpreterError> {
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
                            Err(RuntimeError(format!("expected number type at {}", expr)))
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
                            _ => Err(RuntimeError(format!("mismatched type at {}", e)))
                        }
                    },
                    BinaryOp::LT | BinaryOp::LE | BinaryOp::GT | BinaryOp::GE => {
                        let (left, right) = (left.to_f64()?, right.to_f64()?);
                        Ok(Bool(
                            if *op == BinaryOp::LT { left < right } else 
                            if *op == BinaryOp::LE { left <= right} else 
                            if *op == BinaryOp::GT { left > right } else 
                            { left >= right }
                        ))
                    }
                    BinaryOp::SUB | BinaryOp::MUL | BinaryOp::DIV => {
                        let (left, right) = (left.to_f64()?, right.to_f64()?);
                        Ok(Number(
                            if *op == BinaryOp::SUB { left - right } else 
                            if *op == BinaryOp::MUL { left * right } else 
                            { left / right }
                        ))
                    }
                    // short cut
                    BinaryOp::OR => Ok(if left.is_truthy() { left.clone() } else { right.clone() }),
                    BinaryOp::AND => Ok(if !left.is_truthy() { left.clone() } else { right.clone() })
                }
            }
            Expr::Varible(v) => {
                Ok(self.env.borrow().get(v))
            }
            Expr::Assign(s, e) => {
                let val = self.visit_expr(e)?;
                self.env.borrow_mut().set(s.env_depth, s.id, val.clone());
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
                    return Err(RuntimeError(format!("wrong number of arguments")));
                }
                f.call(self, &args)
            }
            Expr::Get(expr, name) => {
                let object = self.visit_expr(expr)?;
                let instance = object.to_instance()?;
                LoxInstance::get(instance, name)
            }
            Expr::Set(expr, name, value) => {
                let object = self.visit_expr(expr)?;
                let instance = object.to_instance()?;
                let value = self.visit_expr(value)?;
                let mut ref_instance = instance.borrow_mut();
                ref_instance.set(&name, value.clone());
                Ok(value)
            }
            Expr::This(v) => Ok(self.env.borrow().get(v)),
        }
    }

    fn visit_stmt(&mut self, s: &Stmt) -> Result<(), InterpreterError> {
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
                let val = if let Some(e) = e { self.visit_expr(e)? } else { Object::Nil };
                self.env.borrow_mut().define(&name, val);
                Ok(())
            }
            Stmt::Block(v) => self.execute_block(v, Rc::new(RefCell::new(Environment::new(Some(self.env.clone()))))),
            
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
            Stmt::Func(f) => {
                self.env.borrow_mut().define(&f.ident, Object::Closure(
                    Box::new(LoxFunction::new(f.clone(), self.env.clone(), false)) // clone once
                ));
                Ok(())
            }
            Stmt::Return(expr) => {
                self.return_object = Some(self.visit_expr(expr)?);
                Ok(())
            }
            Stmt::Class(ident, body) => {
                self.env.borrow_mut().define(&ident, Object::Nil);
                let methods = HashMap::from_iter(
                    body.iter().map(|f| 
                        (f.ident.name.as_ref().clone(), LoxFunction::new(f.clone(), self.env.clone(), f.ident.name.as_ref() == "init"))
                    )
                );
                let lox_class = LoxClass::new(ident.clone(), methods);
                self.env.borrow_mut().assign(&ident, Object::ClassCons(
                    Rc::new(lox_class)
                ));
                Ok(())
            }
        }
    }
    pub fn new(builtin_functions: Vec<RustFunction>) -> Self {
        let env = Rc::new(RefCell::new(Environment::new_global(&builtin_functions)));
        let globals = env.clone();
        Interpreter { 
            env,
            globals,
            return_object: None,
            builtin_functions,
        }
    }
    pub fn interpret(&mut self, mut statements: Vec<Stmt>) -> Result<(), InterpreterError> {
        let mut resolver = Resolver::new();
        for (i, f) in self.builtin_functions.iter().enumerate() {
            resolver.set_builtin(i, f.name.clone())
        }
        resolver.resolve(&mut statements)?;
        for statement in statements {
            self.visit_stmt(&statement)?;
        }
        Ok(())
    }

    pub fn execute_block(&mut self, v: &Vec<Stmt>, env: Rc<RefCell<Environment>>) -> Result<(), InterpreterError> {
        let previous = self.env.clone();
        self.env = env;
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

