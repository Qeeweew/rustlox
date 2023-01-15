use core::{fmt::{Display, write}, cell::RefCell};

use alloc::rc::Rc;

use super::object::*;

#[derive(Clone, Debug)]
pub struct Identifier {
    pub name: Box<String>, // won't visit it in interpreting pass move
    pub env_depth: usize,
    pub id: usize,
}
impl Identifier {
    pub fn new(name: String) -> Self {
        Self { name: Box::new(name), env_depth: usize::MAX, id: usize::MAX} // don't know env_depth and id at first
        
    }
}
#[derive(Clone)]
pub enum Expr {
    Literal(Object),
    Unary(UnaryOp, Box<Expr>), // (op, expr)
    Binary(BinaryOp, Box<Expr>, Box<Expr>), // (op, left, right)
    Varible(Identifier),
    Assign(Identifier, Box<Expr>), 
    Call(Box<Expr>, Vec<Expr>), // (callee, arguments)
    Get(Box<Expr>, String), // (object, property)
    Set(Box<Expr>, String, Box<Expr>),
    This(Identifier),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(o) => write!(f, "{}", o),
            Expr::Unary(op, e) => write!(f, "({:?} {})", op, e),
            Expr::Binary(op, l, r) => write!(f, "({} {:?} {})", l, op, r),
            Expr::Varible(v) => write!(f, "{}", v.name),
            Expr::Assign(v, e) => write!(f, "{} = {}", v.name, e),
            Expr::Call(func, a) => {
                write!(f, "{}(", func)?;
                for (i, e) in a.iter().enumerate() {
                    if i == a.len() - 1 {
                        write!(f, "{})", e)?
                    } else {
                        write!(f, "{}, ", e)?
                    }
                }
                write!(f, ")")
            }
            Expr::Get(e, s) => write!(f, "{}.{}", e, s),
            Expr::Set(o, s, v) => write!(f,"{}.{} = {}", o, s, v),
            Expr::This(_) => write!(f,"this"),
        }
    }
}

#[derive(Clone)]
pub struct Function {
    pub ident: Identifier,
    pub params: Vec<Identifier>,
    pub body: Vec<Stmt>,
}

#[derive(Clone)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    Var(Identifier, Option<Expr>),
    Block(Vec<Stmt>),
    Func(Function),
    Return(Expr),
    Class(Identifier, Vec<Function>),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expression(e) => { write!(f, "{};", e)}
            Stmt::Print(e) => { write!(f, "print {};", e)},
            Stmt::If(cond, e1, e2) => {
                if let Some(e2) = e2   {
                    write!(f, "if ({})\n{}\n else \n {}", cond, e1, e2)
                } else {
                    write!(f, "if ({})\n{}", cond, e1)
                }
            },
            Stmt::While(cond, stmt) => write!(f, "while ({})\n{}", cond, stmt),
            Stmt::Var(s, expr) => {
                if let Some(expr) = expr {
                    write!(f, "var {} = {};", s.name, expr)
                } else {
                    write!(f, "var {};", s.name)
                }
            }
            Stmt::Block(v) => {
                write!(f, "{{")?;
                for stmt in v {
                    write!(f, "\n{}", stmt)?;
                }
                write!(f,"\n}}")
            }
            Stmt::Func(func) => write!(f, "function {}\n", func.ident.name),
            Stmt::Return(_) => write!(f, "return statement"),
            Stmt::Class(ident,_) => write!(f, "class {}\n", ident.name),
        }
    }
}


#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinaryOp {
    EQ, NEQ, LT, LE, GT, GE, ADD, SUB, MUL, DIV, 
    OR, AND // short-circuit
}

mod test {
    use super::*;


}