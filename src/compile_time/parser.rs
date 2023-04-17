use crate::{
    spanned::Spanned,
    token::{ExprRole, PrecedenceLevel, Token},
};

use super::{
    error::{CTError, CTErrorKind},
    ast::{
        expr::{Expr, Operator},
        Ast, AstNode,
    },
};
pub type SpanToken = Spanned<Token>;

#[derive(Debug)]
pub struct Parser<'a> {
    pub tokens: &'a [SpanToken],
    pub current: &'a SpanToken,
    pub previous: &'a SpanToken,
    pub ast: Ast,
    pub txt: &'a str,
    at: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [SpanToken], txt: &'a str) -> Self {
        Self {
            tokens,
            current: &tokens[0],
            previous: &tokens[0],
            ast: Ast::new(),
            txt,
            at: 0,
        }
    }

    pub fn parse(mut self) -> Ast {
        self.ast.text = Some(self.txt.to_string());
        let node = self.expr();
        self.ast.nodes.push(node);
        self.ast
    }

    fn expr(&mut self) -> AstNode {
        let mut ast = Ast::new();
        self.parse_prec(PrecedenceLevel::Assignment, &mut ast);
        ast.nodes.pop().unwrap_or_else(|| todo!("error handling"))
    }

    fn parse_prec(&mut self, level: PrecedenceLevel, ast: &mut Ast) {
        let level = level as u8;
        let prefix = self.current.get_data().prefix;
        self.call_from_expr_role(&prefix, ast);
        let mut current_data = self.current.get_data();
        while current_data.prec_level as u8 >= level {
            match self.call_from_expr_role(&current_data.midfix, ast) {
                Some(()) => (),
                None => return,
            };
            current_data = self.current.get_data()
        }
    }

    fn call_from_expr_role(&mut self, role: &ExprRole, ast: &mut Ast) -> Option<()> {
        match role {
            ExprRole::Binary => self.binary(ast),
            ExprRole::None => return None,
            ExprRole::Unary => self.unary(ast),
            ExprRole::Expr => todo!(),
            ExprRole::Object => todo!(),
            ExprRole::Number => self.number(ast),
            ExprRole::String => self.string(ast),
            ExprRole::Grouping => self.grouping(ast),
            ExprRole::Bool => todo!(),
            ExprRole::Float => todo!(),
        };
        Some(())
    }

    fn binary(&mut self, ast: &mut Ast) {
        let left = ast.nodes.pop().unwrap();
        let mut _poisoned = false;
        let op = match Operator::try_from_token(self.current) {
            Some(op) => op,
            None => {
                // Keep parsing the expression but
                // poison the tree node
                _poisoned = true;
                Operator::Plus
            },
        };  
        self.advance();
        self.parse_prec(op.prec_level(), ast);
        let right = match ast.nodes.pop() {
            Some(node) => node,
            None => {
                self.report_error(&self.make_error(CTErrorKind::Unexpected(self.current.cloned())));
                AstNode::Expr(Spanned::new(Expr::Int(0), 0, 0))
            }
        };
        let left = match left {
            AstNode::Expr(e) => e,
        };
        let right = match right {
            AstNode::Expr(e) => e,
        };
        let (start, end) = Spanned::get_start_and_len(&left, &right);
        ast.nodes.push(AstNode::Expr(Spanned::new(
            Expr::new_binary(
                Box::new(left),
                Box::new(right),
                op,
            ),
            start, end
        )));
    }

    fn unary(&mut self, ast: &mut Ast) {
        let token_start = self.current.start;
        self.consume_one_of(&[Token::Minus, Token::Bang]);
        self.parse_prec(PrecedenceLevel::Unary, ast);
        let expr = match ast.nodes.pop() {
            Some(node) => node,
            None => {
                self.report_error(&self.make_error(CTErrorKind::Unexpected(self.current.cloned())));
                AstNode::Expr(Spanned::new(Expr::Int(0), 0, 0))
            }
        };
        let expr = match expr {
            AstNode::Expr(e) => e,
        };
        let (start, size) = (token_start, 
            (self.current.start - token_start) + self.current.len);
        ast.nodes.push(AstNode::Expr(Spanned::new(Expr::Unary { 
            expr: Box::new(expr)
        }, start, size)));
        self.advance();
    }

    fn number(&mut self, ast: &mut Ast) {
        self.advance();
        let (start, end) = (self.previous.start, self.previous.len);
        ast.nodes.push(AstNode::Expr(Spanned::new(
            Expr::try_from_token(self.previous).unwrap(),
            start, end)
        ));
    }

    fn string(&mut self, ast: &mut Ast) {
        self.advance();
        let (start, end) = (self.previous.start, self.previous.len);
        ast.nodes.push(AstNode::Expr(Spanned::new(
            Expr::try_from_token(self.previous).unwrap(),
            start, end)
        ));
    }

    fn grouping(&mut self, ast: &mut Ast) {
        self.advance();
        self.parse_prec(PrecedenceLevel::Assignment, ast);
        self.advance(); // change to consume rparen
    }

    fn consume(&mut self, token: &'a Token) -> Option<&'a Token> {
        if &**self.current == token {
            self.advance();
            Some(token)
        } else { None }
    }

    fn consume_one_of(&mut self, tokens: &'a [Token]) -> Option<&'a Token> {
        tokens.iter().find(|t| self.consume(t).is_some())
    }

    fn advance(&mut self) -> Option<&'a Token> {
        self.at += 1;
        self.previous = self.current;
        self.current = self.tokens.get(self.at)?;
        Some(self.current)
    }

    fn make_error(&self, error: CTErrorKind) -> Spanned<CTError> {
        Spanned::new(
            CTError { kind: error },
            self.current.start,
            self.current.len,
        )
    }

    fn report_error(&self, error: &Spanned<CTError>) {
        super::error::report_error(error, self.txt);
    }
}
