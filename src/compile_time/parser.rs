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
            ExprRole::String => todo!(),
            ExprRole::Grouping => self.grouping(ast),
            ExprRole::Bool => todo!(),
            ExprRole::Float => todo!(),
        };
        Some(())
    }

    fn binary(&mut self, ast: &mut Ast) {
        let left = ast.nodes.pop().unwrap();
        let op = Operator::try_from_token(self.current).unwrap_or_else(|| todo!());
        self.advance();
        self.parse_prec(op.prec_level(), ast);
        let right = match ast.nodes.pop() {
            Some(node) => node,
            None => {
                self.report_error(self.make_error(CTErrorKind::Unexpected(self.current.cloned())));
                AstNode::Expr(Expr::Int(0))
            }
        };
        let left = match left {
            AstNode::Expr(e) => e,
        };
        let right = match right {
            AstNode::Expr(e) => e,
        };
        ast.nodes.push(AstNode::Expr(Expr::new_binary(
            Box::new(left),
            Box::new(right),
            op,
        )));
    }

    fn unary(&mut self, ast: &mut Ast) {
        self.advance();
        ast.nodes.push(AstNode::Expr(Expr::Unary {
            expr: Box::new(Expr::try_from_token(self.current).unwrap()),
        }));
        self.advance();
    }

    fn number(&mut self, ast: &mut Ast) {
        self.advance();
        ast.nodes
            .push(AstNode::Expr(Expr::try_from_token(self.previous).unwrap()));
    }

    fn grouping(&mut self, ast: &mut Ast) {
        self.advance();
        self.parse_prec(PrecedenceLevel::Assignment, ast);
        self.advance(); // change to consume rparen
    }

    // fn consume(&mut self, token: &Token) -> Option<&'a Token> {
    //     if self.current == token {
    //         self.advance();
    //         Some(token);
    //     }
    // }

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

    fn report_error(&self, error: Spanned<CTError>) {
        print!(" | [error] {}", *error);
        let mut at_char = error.start;
        let mut at_line = 0;
        let line = self.txt.lines().find(|line| {
            if line.len() <= at_char {
                at_char -= line.len();
                at_line += 1;
                false
            } else {
                true
            }
        });
        println!(" [line {at_line}]");
        let line = match line {
            Some(l) => l,
            None => {
                eprintln!(" | couldn't find the error in code!");
                return;
            }
        };
        print!(" | {line}\n | ");
        for _ in 0..at_char {
            print!(" ");
        }
        for _ in 0..self.current.len() {
            print!("^");
        }
        println!()
    }
}
