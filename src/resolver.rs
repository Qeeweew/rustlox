use std::collections::HashMap;

use super::ast::*;
use super::interpreter::*;
use super::interpreter::InterpreterError::*;

#[derive(Clone, Copy)]
enum FunctionType {
    None,
    Function,
}
pub struct Resolver {
    scopes: Vec<HashMap<String, usize>>,
    current_function: FunctionType, // resolve return
}
type ResolveResult = Result<(), InterpreterError>;
pub const INF: usize = usize::MAX;

impl Resolver {
    pub fn new() -> Self {
        Self { scopes: vec![HashMap::new()], current_function: FunctionType::None }
    }
    pub fn resolve(&mut self, statements: &mut Vec<Stmt>) -> ResolveResult {
        for stmt in statements {
            self.resolve_stmt(stmt)?;
        }
        Ok(())
    }
    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }
    fn end_scope(&mut self) {
        let _ = self.scopes.pop();
    }
    fn declare(&mut self, ident: &Identifier) -> ResolveResult {
        let scope = self.scopes.last_mut().unwrap();
        if scope.contains_key(ident.name.as_ref()) {
            return Err(ResolverError(format!("Already a variable with this name in this scope.")));
        }
        scope.insert(*ident.name.clone(), INF);
        Ok(())
    }
    fn define(&mut self, ident: &mut Identifier) {
        let scope = self.scopes.last_mut().unwrap();
        ident.id = scope.len();
        ident.env_depth = 0;
        scope.insert(*ident.name.clone(), scope.len());
    }

    fn resolve_expr(&mut self, e: &mut Expr) -> ResolveResult {
        match e {
            Expr::Literal(_) => Ok(()),
            Expr::Unary(_, e) => self.resolve_expr(e),
            Expr::Binary(_, e1, e2) => { self.resolve_expr(e1)?; self.resolve_expr(e2) }
            Expr::Varible(ident) => {
                let scope = self.scopes.last().unwrap();
                if scope.get(ident.name.as_ref()) == Some(&INF) {
                    return Err(ResolverError(format!("Can't read local variable in its own initializer.")))
                }
                self.resolve_local(ident)?;
                println!("{:?}", ident);
                Ok(())
            }
            Expr::Assign(i, e) => {
                self.resolve_expr(e)?;
                self.resolve_local(i)
            }
            Expr::Call(callee, arguments) => {
                self.resolve_expr(callee)?;
                for argument in arguments {
                    self.resolve_expr(argument)?;
                }
                Ok(())
            }
        }
    }

    fn resolve_local(&mut self,ident: &mut Identifier) -> ResolveResult {
        let iter = (0..self.scopes.len()).zip(self.scopes.iter().rev());
        for (depth, scope) in iter {
            if let Some(index) = scope.get(ident.name.as_ref()) {
                ident.env_depth = depth;
                ident.id = *index;
                break;
            }
        }
        Ok(())
    }
    fn resolve_function(&mut self, params: &mut Vec<Identifier>, body: &mut Vec<Stmt>, function: FunctionType) -> ResolveResult {
        let enclosing_function = self.current_function;
        self.current_function = function;
        self.begin_scope();
        for param in params {
            self.declare(param)?;
            self.define(param);
        }
        self.resolve(body)?;
        self.end_scope();
        self.current_function = enclosing_function;
        Ok(())
    }

    fn resolve_stmt(&mut self, e: &mut Stmt) -> ResolveResult {
        match e {
            Stmt::Expression(e) => self.resolve_expr(e),
            Stmt::Print(e) => self.resolve_expr(e),
            Stmt::If(e,s_true, s_false) => {
                self.resolve_expr(e)?;
                self.resolve_stmt(s_true)?;
                if let Some(s_false) = s_false {
                    self.resolve_stmt(s_false)?;
                }
                Ok(())
            }
            Stmt::While(e, s) => {self.resolve_expr(e)?; self.resolve_stmt(s) },
            Stmt::Var(name, expr) => {
                self.declare(name)?;
                if let Some(initializer) = expr {
                    self.resolve_expr(initializer)?;
                }
                self.define(name);
                Ok(())
            }
            Stmt::Block(s) => {
                self.begin_scope();
                self.resolve(s)?;
                self.end_scope();
                Ok(())
            }
            Stmt::Function { name, params, body } => {
                self.declare(name)?;
                self.define(name);
                self.resolve_function(params, body, FunctionType::Function)?;
                Ok(())
            }
            Stmt::Return(e) => {
                if matches!(self.current_function, FunctionType::None) {
                    Err(ResolverError(format!("return at top level")))
                } else {
                    self.resolve_expr(e)
                }
            }
        }

    }
}
