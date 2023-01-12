trait Visitor<T> {
    fn visit(&mut self, e: &Expr) -> T;
}

pub enum Expr {
    Literal(Literal),
    Unary(UnaryOp, Box<Expr>), // (op, expr)
    Binary(BinaryOp, Box<Expr>, Box<Expr>), // (op, left, right)
    Grouping(Box<Expr>),
}
pub enum Literal {
    Number(f64),
    String(String),
    Bool(bool)
}
#[derive(Debug)]
pub enum UnaryOp {
    Not,
    Neg,
}
#[derive(Debug)]
pub enum BinaryOp {
    EQ, NEQ, LT, LE, GT, GE, ADD, SUB, MUL, DIV
}

struct PrintExpr {}
impl Visitor<String> for PrintExpr {
    fn visit(&mut self, e: &Expr) -> String {
        match e {
            Expr::Literal(n) => match n {
                Literal::Number(x) => x.to_string(),
                Literal::String(s) => s.clone(),
                Literal::Bool(b) => b.to_string(),
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