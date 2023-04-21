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

macro_rules! do_or_report_and {
    ($self: expr, $action: expr => $and: block) => {
        if let Err(er) = $action {
            $self.report_error(&er);
            $and;
        }
    };
    ($self: expr, $action: expr => $and: block on_true => $else: block) => {
        if let Err(er) = $action {
            $self.report_error(&er);
            $and;
        } else { $else; }
    };
}

macro_rules! do_or_report_and_return {
    ($self: expr, $action: expr) => {
        if let Err(er) = $action {
            $self.report_error(&er);
            return AstNode::Stmt(Stmt::poison());
        }
    };
}

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

    pub fn parse(mut self) -> (Ast, bool) {
        self.ast.text = Some(self.txt.to_string());
        let block = self.stmt_block(&[]);
        self.ast.nodes.extend(block);
        (self.ast, self.had_error)
    }

    fn stmt_block(&mut self, end_at: &[Token]) -> Vec<AstNode> {
        let mut result = vec![];
        while !self.is_at_end() {
            let node = match self.stmt() {
                Ok(node) => node,
                Err((er, node)) => {
                    let is_ending_token = end_at.iter().find(|e| {
                        er.kind == CTErrorKind::Unexpected(e.clone().clone())
                    });
                    match is_ending_token {
                        Some(_) => {
                            self.advance();
                            break;
                        },
                        None => {
                            println!("{er:?}");
                            self.report_error(
                                &Spanned::new(er, self.current.start, self.current.len)
                            );
                        }
                    }
                    node
                }
            };
            result.push(node);
        }
        result
    }

    fn stmt(&mut self) -> Result<AstNode, (CTError, AstNode)> {
        match &**self.current {
            Token::Mut | Token::Var => Ok(self.var_creation()),
            Token::LeftBrace => Ok(self.block()),
            Token::If => Ok(self.if_stmt()),
            t @ _ => {
                let expr = match self.expr() {
                    Some(v) => v,
                    None => {
                        return Err((
                            CTError::new(
                                CTErrorKind::Unexpected(t.clone())),
                            AstNode::Expr(Spanned::new(Expr::Poison, 0, 0))
                        ))
                    }
                };
                if self.current.obj_ref() == &Token::Equals {
                    Ok(AstNode::Stmt(self.assignment(expr)))
                } else {
                    Ok(AstNode::Expr(expr))
                }
            }
        }
    }

    fn safe_stmt(&mut self) -> AstNode {
        match self.stmt()  {
            Ok(stmt) => stmt,
            Err(er) => {
                self.report_error(&Spanned::new(
                    er.0, self.current.start, self.current.len
                ));
                AstNode::Stmt(Stmt::poison())
            }
        }
    }

    fn var_creation(&mut self) -> AstNode {
        let (start, end) = (self.current.start, self.current.len);
        let mut mutable = false;
        let mut poisoned = false;
        self.if_current_advance_and(&Token::Mut, || mutable = true);
        match self.consume(&Token::Var) {
            Ok(_) => (),
            Err(er) => {
                poisoned = true;
                self.report_error(&er);
            }
        }
        let name = match self.consume_ident() {
            Ok(n) => n,
            Err(er) => {
                self.report_error(&er);
                return AstNode::Stmt(Spanned::new(Stmt::Poison, 0, 0));
            }
        };
        let name = Spanned::new(name.to_string(), start, end);
        let ty = if &Token::Colon == &**self.current {
            self.advance();
            let (start, end) = (self.current.start, self.current.len);
            let ty = self.consume_ident().unwrap();
            Spanned::new(ty.to_string(), start, end)
        } else { Spanned::new("_".to_string(), 0, 0) };
        let expr = match self.consume(&Token::Equals) {
            Ok(_) => {
                Some(self.expr().unwrap())
            },
            Err(_) => None
        };
        if let Err(er) = self.consume(&Token::Semicolon) {
            self.advance();
            self.report_error(&er);
        };
        let mut span = Spanned::new(
            Stmt::VarCreation { 
                is_mut: mutable,
                name, 
                ty, 
                value: expr,
            }, 
            start, self.previous.start - start + self.previous.len
        );
        if poisoned { span.poison(); }
        AstNode::Stmt(span)
    }

    fn block(&mut self) -> AstNode {
        if let Err(er) = self.consume(&Token::LeftBrace) {
            self.report_error(&er);
        }
        let start = self.current.start;
        let nodes = self.stmt_block(&[Token::RightBrace]);
        AstNode::Stmt(Spanned::new(
            Stmt::Block { nodes }, start, 
            self.current.start - start + self.current.len
        ))
    }

    fn if_stmt(&mut self) -> AstNode {
        let start = self.current.start;
        do_or_report_and_return!(self, self.consume(&Token::If));
        do_or_report_and_return!(self, self.consume(&Token::LeftParen));
        let condition = match self.expr() {
            Some(expr) => expr,
            None => {
                self.report_error(
                    &Spanned::new(
                        CTError::new(CTErrorKind::ExpectedExpr),
                        self.current.start, self.current.len
                    )
                );
                return AstNode::Stmt(Stmt::poison())
            }
        };
        let true_branch;
        let mut false_branch = None;
        do_or_report_and!(self, self.consume(&Token::RightParen) => {
            true_branch = Box::new(AstNode::Stmt(Stmt::poison()))
        } on_true => {
            true_branch = Box::new(self.safe_stmt());
        });
        if self.current.obj_ref() == &Token::Else {
            self.advance();
            false_branch = Some(Box::new(self.safe_stmt()));
        }
        AstNode::Stmt(
            Spanned::new(
                Stmt::If { 
                    condition: condition, 
                    true_branch: true_branch, 
                    false_branch: false_branch
                },
                start, self.current.start - start + self.current.len
            )
        )
    }
    
    fn assignment(&mut self, left: Spanned<Expr>) -> Spanned<Stmt> {
        if let Err(er) = self.consume(&Token::Equals) {
            self.report_error(&er);
        }
        let right = self.expr().unwrap_or_else(|| {
            self.report_error(&self.make_error(
                CTErrorKind::ExpectedExpr,
            ));
            Spanned::new(Expr::Poison, 0, 0)
        });
        if let Err(er) = self.consume(&Token::Semicolon) {
            self.report_error(&er);
        };
        Spanned::new(Stmt::Assignment { 
            left: Box::new(left), 
            right: Box::new(right) 
        }, 0, 0)
    }
    
    fn _consume_string(&mut self) -> Result<&'a str, Spanned<CTError>> {
        if let Token::Str(s) = &**self.current {
            self.advance();
            Ok(s)
        } else {
            Err(self.make_error(CTErrorKind::ExpectedStr))
        }
    }

    fn consume_ident(&mut self) -> Result<&'a str, Spanned<CTError>> {
        if let Token::Identifier(s) = &**self.current {
            self.advance();
            Ok(s)
        } else {
            Err(self.make_error(CTErrorKind::ExpectedIdent))
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
            ExprRole::Object => self.object(ast),
            ExprRole::Literal => self.literal(ast),
            ExprRole::Grouping => self.grouping(ast),
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

    fn object(&mut self, ast: &mut Ast) {
        let (start, end) = (self.current.start, self.current.len);        
        match self.consume_ident() {
            Ok(ident) => {
                ast.nodes.push(AstNode::Expr(Spanned::new(
                    Expr::Var(ident.to_string()), start, end)
                ));
            },
            Err(er) => {
                self.advance();
                self.report_error(&er);
                ast.nodes.push(AstNode::Expr(Spanned::new(
                    Expr::Poison, start, end,
                )))
            }
        };
    }

    fn literal(&mut self, ast: &mut Ast) {
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

    fn is_at_end(&self) -> bool {
        self.current.obj_ref() == &Token::Eof
    }
}
