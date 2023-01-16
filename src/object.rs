use core::{fmt::{Display}, cell::{RefCell},mem};
use std::{collections::HashMap, fs::OpenOptions};
use alloc::rc::Rc;

use crate::ast::{Identifier, FunctionBody};

use super::interpreter::*;
use InterpreterError::*;

pub trait LoxCallable: Display {
    fn call(&self, interpreter: &mut Interpreter, arguments: &Vec<Object>) -> Result<Object, InterpreterError>;
    fn arity(&self) -> usize;
}

#[derive (Clone)]
pub struct RustFunction {
    pub name: String,
    pub func: fn(&Vec<Object>) -> Result<Object, InterpreterError>,
    pub arity: usize,
}

#[derive(Clone)]
pub struct LoxFunction {
    func: Rc<FunctionBody>,
    closure: Rc<RefCell<Environment>>,
    is_initializer: bool,
}

pub struct LoxClass {
    name: Identifier,
    super_class: Option<Rc<LoxClass>>,
    methods: HashMap<String, LoxFunction>,
}

#[derive(Clone)]
pub struct LoxInstance {
    lox_class: Rc<LoxClass>,
    pub fields: HashMap<String, Object>,
}

#[derive(Clone)]
pub enum Object {
    Number(f64),
    String(Box<String>),
    Bool(bool),
    Closure(Box<LoxFunction>),
    BuitlinFunc(Box<RustFunction>),
    Class(Rc<LoxClass>),
    Instance(Rc<RefCell<LoxInstance>>),
    Nil,
}

impl LoxFunction {
    pub fn new(func: FunctionBody, closure: Rc<RefCell<Environment>>, is_initializer: bool) -> Self {
        LoxFunction { func: Rc::new(func), closure, is_initializer}
    }
    pub fn bind(&self, instance: Rc<RefCell<LoxInstance>>) -> Self {
        let mut new_env = Environment::new(Some(self.closure.clone()));
        new_env.define_this(instance);
        LoxFunction { func: self.func.clone(), closure: Rc::new(RefCell::new(new_env)), is_initializer: self.is_initializer}
    }
}
impl LoxCallable for LoxFunction {
    fn call(&self, interpreter: &mut Interpreter, arguments: &Vec<Object>) -> Result<Object, InterpreterError> {
        let p_a = self.func.params.iter().zip(arguments);
        let mut environment = Environment::new(Some(self.closure.clone()));
        for (name, value) in p_a {
            environment.define(name, value.clone());
        }
        let env = Rc::new(RefCell::new(environment));
        interpreter.execute_block(&self.func.body, env.clone())?;
        if let Some(o) = mem::replace(&mut interpreter.return_object, None) {
            Ok(o)
        } else {
            if self.is_initializer {
                Ok(env.borrow().get_(1, 0)) // "this"'s cooordinate is always (1, 0)
            } else {
                Ok(Object::Nil)
            }
        }
    }

    fn arity(&self) -> usize {
        return self.func.params.len();
    }
}
impl Display for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}", self.func.ident.name)
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
impl Display for RustFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}", self.name)
    }
}

impl LoxCallable for Rc<LoxClass> {
    fn call(&self, interpreter: &mut Interpreter, arguments: &Vec<Object>) -> Result<Object, InterpreterError> {
        let instance = Rc::new(RefCell::new(LoxInstance{lox_class: self.clone(), fields: HashMap::new()}));
        if let Some(initializer) = self.find_method(&"init".to_owned()) {
            initializer.bind(instance.clone()).call(interpreter, arguments)?;
        }
        Ok(Object::Instance(instance))
    }

    fn arity(&self) -> usize {
        if let Some(initializer) = self.find_method(&"init".to_owned()) {
            initializer.arity()
        } else {
            0
        }
    }
}

impl Display for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name.name)
    }
}

impl LoxClass {
    pub fn new(ident: Identifier,super_class: Option<Rc<LoxClass>>,methods: HashMap<String, LoxFunction>) -> Self {
        Self {
            name: ident, super_class, methods 
        }
    }

    pub fn find_method(&self, name: &String) -> Option<LoxFunction> {
        self.methods.get(name).map_or(
            self.super_class.as_ref().and_then(|c| c.find_method(name)), 
            |x| Some(x.clone())
        )
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Number(x) => write!(f, "{}", x),
            Object::String(s) => write!(f, "{}", s),
            Object::Bool(b) => write!(f, "{}", b),
            Object::Nil => write!(f,"Nil"),
            Object::Instance(o) => write!(f, "a instance of class: {}", o.borrow().lox_class),
            Object::Closure(fun) => write!(f, "closure: {}", fun),
            Object::BuitlinFunc(fun) => write!(f, "builtin function: {}",fun),
            Object::Class(fun) => write!(f, "Classtruct: {}", fun),
        }
    }
}

impl LoxInstance {
    pub fn get(instance: Rc<RefCell<LoxInstance>>, name: &String) -> Result<Object, InterpreterError> {
        let ref_instance = instance.borrow();
        if let Some(o) = ref_instance.fields.get(name) {
            return Ok(o.clone());
        }
        if let Some(method) = ref_instance.lox_class.find_method(name) {
            return Ok(Object::Closure(Box::new(method.bind(instance.clone()))));
        }
        Err(RuntimeError(format!("Undefined property {}.", name)))
    }
    pub fn set(instance: Rc<RefCell<LoxInstance>>, name: &String, value: Object) {
        let mut ref_instance = instance.borrow_mut();
        ref_instance.fields.insert(name.clone(), value);
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
    pub fn to_callable(self) -> Result<Box<dyn LoxCallable>, InterpreterError> {
        match self {
            Object::Closure(f) => Ok(f),
            Object::BuitlinFunc(f) => Ok(f),
            Object::Class(f) => Ok(Box::new(f)),
            _ => Err(RuntimeError(format!("{} not a callable object", self)))
        }
    }
    pub fn to_f64(self) -> Result<f64, InterpreterError> {
        match self {
            Object::Number(x) => Ok(x),
            _ => Err(RuntimeError(format!("{} is not a number", self))),
        }
    }
    pub fn to_instance(self) -> Result<Rc<RefCell<LoxInstance>>, InterpreterError> {
        match self {
            Object::Instance(o) => Ok(o),
            _ => Err(RuntimeError(format!("{} not a lox instance", self)))
        }
    }
    pub fn to_class(self) -> Result<Rc<LoxClass>, InterpreterError> {
        match self {
            Object::Class(c) => Ok(c),
            _ => Err(RuntimeError(format!("{} not a lox class", self)))
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

