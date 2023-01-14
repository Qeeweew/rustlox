use core::{fmt::{Display}, cell::RefCell};
use alloc::rc::Rc;

use crate::ast::Stmt;

use super::interpreter::*;

pub trait LoxCallable {
    fn call(&self, interpreter: &mut Interpreter, arguments: &Vec<Object>) -> Result<Object, RuntimeError>;
    fn arity(&self) -> usize;
}

type AlianFunctionType = fn(&Vec<Object>) -> Result<Object, RuntimeError>;

#[derive(Clone)]
pub struct RustFunction {
    name: String,
    func: AlianFunctionType,
    arity: usize,
}

#[derive(Clone)]
pub struct LoxFunction {
    name: String,
    params: Vec<String>,
    body: Rc<Vec<Stmt>>,
    clourse: Rc<RefCell<Environment>>,
}

impl LoxFunction {
    pub fn new(name: String, params: Vec<String>, body: Rc<Vec<Stmt>>, clourse: Rc<RefCell<Environment>> ) -> Self {
        LoxFunction { name, params, body, clourse}
    }
}
#[derive(Clone)]
pub enum Object {
    Number(f64),
    String(Box<String>),
    Bool(bool),
    Function(Box<LoxFunction>),
    BuiltinFunction(Box<RustFunction>),
    Nil,
}

impl LoxCallable for LoxFunction {
    fn call(&self, interpreter: &mut Interpreter, arguments: &Vec<Object>) -> Result<Object, RuntimeError> {
        let p_a = self.params.iter().zip(arguments);
        let mut environment = Environment::new(Some(self.clourse.clone()));
        for (name, value) in p_a {
            environment.define(name, value.clone());
        }
        interpreter.execute_block(&self.body, environment)?;
        return Ok(Object::Nil);
    }

    fn arity(&self) -> usize {
        return self.params.len();
    }
}

impl LoxCallable for RustFunction {
    fn call(&self, interpreter: &mut Interpreter, arguments: &Vec<Object>) -> Result<Object, RuntimeError> {
        (self.func)(arguments)
    }

    fn arity(&self) -> usize {
        return self.arity;
    }
}


impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Number(x) => write!(f, "{}", x),
            Object::String(s) => write!(f, "{}", s),
            Object::Bool(b) => write!(f, "{}", b),
            Object::Nil => write!(f,"Nil"),
            Object::Function(_) => todo!(),
            Object::BuiltinFunction(_) => todo!(),
        }
    }
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Bool(b) => *b,
            Object::Nil => false,
            _ => true,
        }
    }
    pub fn to_callable(&self) -> Result<Box<dyn LoxCallable>, RuntimeError> {
        match self {
            Object::Function(f) => Ok(f.clone()),
            Object::BuiltinFunction(f) => Ok(f.clone()),
            _ => Err(RuntimeError::ExprError(format!("{} not a callable object", self))) 
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Nil, Self::Nil) => true,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

