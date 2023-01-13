use super::token::*;
use super::ast::*;

pub struct Parser {
   tokens: Vec<Token>,
   current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }
    fn advance(&mut self) {
        if self.peek_type() != TokenType::EOF {
            self.current += 1;
        }
    }
    // fn previous_type(&self) -> TokenType { return self.tokens[self.current - 1].token_type; }
    fn peek_type(&self) -> TokenType { return self.tokens[self.current].token_type; }
    fn peek(&self) -> Token { return self.tokens[self.current].clone(); }
    fn match_type(&mut self, ts: Vec<TokenType>) -> Option<Token> {
        for t in ts {
            if self.peek_type() == t {
                let res = Some(self.peek());
                self.advance();
                return res;
            }
        }
        None
    }

    pub fn expression(&mut self) -> Box<Expr> {
       return self.equality();
    }

    fn equality(&mut self) -> Box<Expr> {
        let mut expr = self.comparison();
        use TokenType::*;
        while let Some(t) = self.match_type(vec![BangEqual, EqualEqual]) {
            let op = BinaryOp::from_token(&t);
            expr = Box::new(
                Expr::Binary(op, expr, self.comparison())
            );
        }
        expr
    }
    fn comparison(&mut self) -> Box<Expr> {
        let mut expr = self.term();
        use TokenType::*;
        while let Some(t) = self.match_type(vec![Greater, GreaterEqual, Less, LessEqual]) {
            let op = BinaryOp::from_token(&t);
            expr = Box::new(Expr::Binary(op, expr, self.term()));
        }
        expr
    }

    fn term(&mut self) -> Box<Expr> {
        let mut expr = self.factor();
        use TokenType::*;
        while let Some(t) = self.match_type(vec![Plus, Minus]) {
            let op = BinaryOp::from_token(&t);
            expr = Box::new(
                Expr::Binary(op, expr, self.factor())
            );
        }
        expr
    }

    fn factor(&mut self) -> Box<Expr> {
        let mut expr = self.unary();
        use TokenType::*;
        while let Some(t) = self.match_type(vec![Star, Slash]) {
            let op = BinaryOp::from_token(&t);
            expr = Box::new(
                Expr::Binary(op, expr, self.unary())
            );
        }
        expr
    }
    fn unary(&mut self) -> Box<Expr> {
        use TokenType::*;
        if let Some(t) = self.match_type(vec![Bang, Minus]) {
            let expr =  self.unary();
            let op = UnaryOp::from_token(&t);
            return Box::new(Expr::Unary(op, expr));
        }
        self.primary()
    }
    fn primary(&mut self) -> Box<Expr> {
        use Object::*;
        let t = self.peek();
        self.advance();
        if t.token_type == TokenType::LeftParen {
            let expr = self.expression();
            self.consume(TokenType::RightParen);
            return expr;
        }
        Box::new(match t.token_type {
            TokenType::FALSE  => Expr::Literal(Bool(false)),
            TokenType::TRUE   => Expr::Literal(Bool(true)),
            TokenType::NIL    => Expr::Literal(Nil),
            TokenType::Number => Expr::Literal(Number(t.literal.unwrap().parse::<f64>().unwrap())),
            TokenType::String => Expr::Literal(String(*t.literal.unwrap())),
            _ => panic!("unknown expression"),
        })
    }
    fn consume(&mut self, t: TokenType) {
        if self.peek_type() != t {
            panic!("Expect ')' after expression.");
        }
        self.advance();
    }
}