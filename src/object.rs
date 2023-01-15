use core::{fmt::{Display}, cell::RefCell};
use alloc::rc::Rc;

use crate::ast::{Stmt, Identifier};

use super::interpreter::*;
use InterpreterError::*;

pub trait LoxCallable {
    fn call(&self, interpreter: &mut Interpreter, arguments: &Vec<Object>) -> Result<Object, InterpreterError>;
    fn arity(&self) -> usize;
}

type AlianFunctionType = fn(&Vec<Object>) -> Result<Object, InterpreterError>;

#[derive(Clone)]
pub struct RustFunction {
    name: String,
    func: AlianFunctionType,
    arity: usize,
}

pub struct LoxFunction {
    name: Identifier,
    params: Vec<Identifier>,
    body: Vec<Stmt>,
    closure: Rc<RefCell<Environment>>,
}

impl LoxFunction {
    pub fn new(name: Identifier, params: Vec<Identifier>, body: Vec<Stmt>, closure: Rc<RefCell<Environment>> ) -> Self {
        LoxFunction { name, params, body, closure}
    }
}
#[derive(Clone)]
pub enum Object {
    Number(f64),
    String(Box<String>),
    Bool(bool),
    Function(Rc<LoxFunction>),
    BuiltinFunction(Rc<RustFunction>),
    Nil,
}

impl LoxCallable for LoxFunction {
    fn call(&self, interpreter: &mut Interpreter, arguments: &Vec<Object>) -> Result<Object, InterpreterError> {
        let p_a = self.params.iter().zip(arguments);
        let mut environment = Environment::new(Some(self.closure.clone()));
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
    fn call(&self, _: &mut Interpreter, arguments: &Vec<Object>) -> Result<Object, InterpreterError> {
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
            Object::Function(func) => write!(f,"closure: {}",func.name.name),
            Object::BuiltinFunction(func) => write!(f,"builtin function: {}", func.name),
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
    pub fn to_callable(&self) -> Result<Rc<dyn LoxCallable>, InterpreterError> {
        match self {
            Object::Function(f) => Ok(f.clone()),
            Object::BuiltinFunction(f) => Ok(f.clone()),
            _ => Err(RuntimeError(format!("{} not a callable object", self)))
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
            (Self::Function(f1), Self::Function(f2)) => f1.name.name == f2.name.name,
            (Self::BuiltinFunction(f1), Self::BuiltinFunction(f2)) => f1.name == f2.name,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

