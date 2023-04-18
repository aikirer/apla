use crate::{
    spanned::Spanned,
    token::{ExprRole, PrecedenceLevel, Token},
};

use super::{
    error::{CTError, CTErrorKind},
    ast::{
        expr::{Expr, Operator},
        Ast, AstNode, stmt::Stmt,
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
    had_error: bool,
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
            had_error: false,
            at: 0,
        }
    }

    pub fn parse(mut self) -> Option<(Ast, bool)> {
        self.ast.text = Some(self.txt.to_string());
        let node = self.stmt()?;
        self.ast.nodes.push(node);
        Some((self.ast, self.had_error))
    }

    fn stmt(&mut self) -> Option<AstNode> {
        match &**self.current {
            Token::Mut | Token::Var => self.var_creation(),
            _ => Some(AstNode::Expr(self.expr()?)),
        }
    }

    fn var_creation(&mut self) -> Option<AstNode> {
        let mut mutable = false;
        let mut poisoned = false;
        // if the mut keyword is absent, its ok
        self.if_current_advance_and(&Token::Mut, || mutable = true);
        match self.consume(&Token::Var) {
            Ok(_) => (),
            Err(er) => {
                poisoned = true;
                self.report_error(&er);
            }
        }
        let (start, end) = (self.current.start, self.current.len);
        let name = self.consume_ident().unwrap();
        let name = Spanned::new(name.to_string(), start, end);
        let ty = if &Token::Colon == &**self.current {
            self.advance();
            let (start, end) = (self.current.start, self.current.len);
            let ty = self.consume_ident().unwrap();
            Spanned::new(ty.to_string(), start, end)
        } else { Spanned::new("".to_string(), 0, 0) };
        self.consume(&Token::Equals).unwrap();
        let expr = self.expr().unwrap();
        self.consume(&Token::Semicolon).unwrap();
        let mut span = Spanned::new(
            Stmt::VarCreation { 
                is_mut: mutable,
                name, 
                ty, 
                value: expr,
            }, 
            start, self.current.start - start + self.current.len
        );
        if poisoned { span.poison(); }
        Some(AstNode::Stmt(span))
    }

    fn _consume_string(&mut self) -> Result<&'a str, Spanned<CTError>> {
        if let Token::Str(s) = &**self.current {
            self.advance();
            Ok(s)
        } else {
            // this is so hacky
            Err(self.make_error(CTErrorKind::Expected(
                    Token::Identifier("a string".to_string())
            )))
        }
    }

    fn consume_ident(&mut self) -> Result<&'a str, Spanned<CTError>> {
        if let Token::Identifier(s) = &**self.current {
            self.advance();
            Ok(s)
        } else {
            // this is so hacky
            Err(self.make_error(CTErrorKind::Expected(
                    Token::Identifier("a string".to_string())
            )))
        }
    }

    fn expr(&mut self) -> Option<Spanned<Expr>> {
        let mut ast = Ast::new();
        self.parse_prec(PrecedenceLevel::Assignment, &mut ast);
        match ast.nodes.pop()? {
            AstNode::Expr(e) => Some(e),
            AstNode::Stmt(_) => panic!(),
        }
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
        let mut poisoned = false;
        let op = match Operator::try_from_token(self.current) {
            Some(op) => op,
            None => {
                // Keep parsing the expression but
                // poison the tree node
                poisoned = true;
                self.report_error(&self.make_error(CTErrorKind::ExpectedOperator));
                Operator::Plus
            },
        };  
        self.advance();
        self.parse_prec(op.prec_level(), ast);
        let right = match ast.nodes.pop() {
            Some(node) => node,
            None => {
                self.report_error(&self.make_error(CTErrorKind::Unexpected(self.current.cloned())));
                poisoned = true;
                AstNode::Expr(Spanned::new(Expr::Int(0), 0, 0))
            }
        };
        let left = match left {
            AstNode::Expr(e) => e,
            _ => panic!(),
        };
        let right = match right {
            AstNode::Expr(e) => e,
            _ => panic!(),
        };
        let (start, end) = Spanned::get_start_and_len(&left, &right);
        let mut span = Spanned::new(
            Expr::new_binary(
                Box::new(left),
                Box::new(right),
                op,
            ),
            start, end
        );
        if poisoned { span.poison(); }
        let node = AstNode::Expr(span);
        ast.nodes.push(node);
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
            _ => panic!(),
        };
        let (start, size) = (token_start, 
            (self.previous.start - token_start) + self.previous.len);
        ast.nodes.push(AstNode::Expr(Spanned::new(Expr::Unary { 
            expr: Box::new(expr)
        }, start, size)));
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

    fn consume(
        &mut self, token: &'a Token
    ) -> Result<&'a Token, Spanned<CTError>> 
    {
        if &**self.current == token {
            self.advance();
            Ok(token)
        } else { 
            Err(self.make_error(CTErrorKind::Expected(token.clone()))) 
        }
    }

    fn if_current_advance_and<T, F: FnOnce() -> T>(
        &mut self, token: &'a Token, clos: F
    ) -> Option<T>
    {
        if token == &**self.current {
            self.advance();
            Some(clos())
        } else { None }
    }

    fn consume_one_of(
        &mut self, tokens: &'a [Token]
    ) -> Option<&'a Token> 
    {
        tokens.iter().find(|t| self.consume(t).is_ok())
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

    fn report_error(&mut self, error: &Spanned<CTError>) {
        self.had_error = true;
        super::error::report_error(error, self.txt);
    }
}
