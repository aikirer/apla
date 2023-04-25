use std::collections::HashMap;

use crate::{
    spanned::Spanned,
    token::{ExprRole, PrecedenceLevel, Token}, 
    compile_time::util::func::Func,
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
    pub functions: HashMap<String, Func>,
    pub ast: Ast,
    pub txt: &'a str,
    pub had_error: bool,
    at: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [SpanToken], txt: &'a str) -> Self {
        Self {
            tokens,
            current: &tokens[0],
            previous: &tokens[0],
            ast: Ast::new(),
            functions: HashMap::new(),
            txt,
            had_error: false,
            at: 0,
        }
    }

    pub fn parse(mut self) -> Self {
        self.ast.text = Some(self.txt.to_string());
        let block = self.stmt_block(&[]);
        self.ast.nodes.extend(block);
        self
    }

    fn stmt_block(&mut self, end_at: &[Token]) -> Vec<AstNode> {
        let mut result = vec![];
        while !self.is_at_end() {
            let node = match self.stmt() {
                Ok(node) => node,
                Err((er, node)) => {
                    let is_ending_token = end_at.iter().find(|e| {
                        er.kind == CTErrorKind::Unexpected((*e).clone())
                    });
                    match is_ending_token {
                        Some(_) => {
                            self.advance();
                            return result;
                        },
                        None => {
                            self.advance();
                            self.report_error(
                                &Spanned::new(er, 
                                    self.previous.start, 
                                    self.previous.len
                                )
                            );
                        }
                    }
                    Some(node)
                }
            };
            if let Some(node) = node {
                result.push(node);
            }
        }
        result
    }

    fn stmt(&mut self) -> Result<Option<AstNode>, (CTError, AstNode)> {
        match &**self.current {
            Token::Mut | Token::Var => {
                Ok(Some(self.var_creation()))
            },
            Token::LeftBrace => Ok(Some(self.block())),
            Token::If => Ok(Some(self.if_stmt())),
            Token::Return => Ok(Some(self.ret_stmt())),
            Token::While => Ok(Some(self.while_stmt())),
            Token::Func => {
                self.func_dec();
                // function doesn't have a node
                Ok(None)
            }
            t => {
                let expr = match self.expr() {
                    Some(v) => {
                        v
                    },
                    None => {
                        return Err((
                            CTError::new(
                                CTErrorKind::Unexpected(t.clone())),
                            AstNode::Expr(Spanned::new(Expr::Poison, 0, 0))
                        ))
                    }
                };
                if self.current.obj_ref() == &Token::Equals {
                    Ok(Some(AstNode::Stmt(self.assignment(expr))))
                } else {
                    do_or_report_and!(self,
                        self.consume(&Token::Semicolon)
                        => {});
                    Ok(Some(AstNode::Expr(expr)))
                }
            }
        }
    }

    fn safe_stmt(&mut self) -> AstNode {
        match self.stmt()  {
            Ok(stmt) => {
                match stmt {
                    Some(stmt) => stmt,
                    None => {
                        self.report_error(&Spanned::new(
                            CTError::new(CTErrorKind::ExpectedStmt),
                            self.current.start, self.current.len,
                        ));
                        AstNode::Stmt(Stmt::poison())
                    }
                }
            },
            Err(er) => {
                self.report_error(&Spanned::new(
                    er.0, self.current.start, self.current.len
                ));
                AstNode::Stmt(Stmt::poison())
            }
        }
    }

    fn parse_var_name_and_type(
        &mut self, start: usize, end: usize
    ) -> Result<(Spanned<String>, Spanned<String>), AstNode>
    {
        let name = match self.consume_ident() {
            Ok(n) => n,
            Err(er) => {
                self.report_error(&er);
                return Err(AstNode::Stmt(Spanned::new(Stmt::Poison, 0, 0)));
            }
        };
        let name = Spanned::new(name.to_string(), start, end);
        let ty = if Token::Colon == **self.current {
            self.advance(); // ':'
            let (start, end) = (self.current.start, self.current.len);
            let ty = match self.consume_ident() {
                Ok(t) => t,
                Err(er) => {
                    self.report_error(&er);
                    return Err(AstNode::Stmt(Stmt::poison()))
                }
            };
            Spanned::new(ty.to_string(), start, end)
        } else { Spanned::new("_".to_string(), 0, 0) };
        Ok((name, ty))
    }

    fn varless_var_creation(&mut self) -> AstNode {
        let (start, end) = (self.current.start, self.current.len);
        let mut mutable = false;
        self.if_current_advance_and(&Token::Mut, || mutable = true);
        let (name, ty) = match self.parse_var_name_and_type(start, end) {
            Ok(a) => a,
            Err(er) => {
                return er;
            }
        };
        let span = Spanned::new(
            Stmt::VarCreation { 
                is_mut: mutable,
                name, 
                ty, 
                value: None,
            }, 
            start, self.previous.start - start + self.previous.len
        );
        AstNode::Stmt(span)
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
        let (name, ty) = match self.parse_var_name_and_type(start, end) {
            Ok(a) => a,
            Err(er) => {
                return er;
            }
        };
        let expr = match self.consume(&Token::Equals) {
            Ok(_) => {
                Some(self.expr().unwrap())
            },
            Err(_) => None
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
        if let Err(er) = self.consume(&Token::Semicolon) {
            self.report_error(&er);
            self.advance();
        }
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
                    condition, 
                    true_branch, 
                    false_branch
                },
                start, self.current.start - start + self.current.len
            )
        )
    }

    fn ret_stmt(&mut self) -> AstNode {
        let return_span = Spanned::from_other_span((), self.current);
        do_or_report_and_return!(self, self.consume(&Token::Return));
        let expr = match self.consume(&Token::Semicolon) {
            Ok(_) => None,
            Err(_) => match self.expr() {
                Some(expr) => {
                    do_or_report_and!(self,
                        self.consume(&Token::Semicolon) => {});
                    Some(expr)
                },
                None => {
                    self.report_error(&Spanned::from_other_span(
                        CTError::new(CTErrorKind::ExpectedExpr), 
                        self.current,
                    ));
                    return AstNode::Expr(Spanned::new(Expr::Poison, 0, 0));
                },
            }
        };
        let len = self.previous.start + self.previous.len - return_span.start;
        AstNode::Stmt(
            Spanned::new(
                Stmt::Return { val: expr },    
                return_span.start, len
            )
        )
    }

    fn while_stmt(&mut self) -> AstNode {
        let start = self.current.start;
        do_or_report_and_return!(self, self.consume(&Token::While));
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
        do_or_report_and_return!(self, self.consume(&Token::RightParen));
        let body = Box::new(self.safe_stmt());
        AstNode::Stmt(Spanned::new(
            Stmt::While { 
                condition, body 
            },
            start, self.current.start - start + self.current.len
        ))
    }

    fn func_dec(&mut self) {
        do_or_report_and!(self, self.consume(&Token::Func) => {});
        let name = match self.consume_ident() {
            Ok(n) => n,
            Err(er) => {
                self.report_error(&er);
                return;
            },
        };
        do_or_report_and!(self, self.consume(&Token::LeftParen) => {});
        let mut args = vec![];
        loop {
            // this happens with no args
            if self.current.obj_ref() == &Token::RightParen {
                self.advance();
                break;
            }
            if self.is_at_end() { break; }
            let var = self.varless_var_creation();
            if var == AstNode::Stmt(Stmt::poison()) { self.advance(); }
            args.push(var);
            match self.consume_one_of(&[Token::RightParen, Token::Comma]) {
                Some(t) => if t == &Token::RightParen { break; },
                None => {
                    self.report_error(&Spanned::new(
                        CTError::new(CTErrorKind::Unexpected(self.current.cloned())),
                        self.current.start, self.current.len
                        )
                    );
                }
            }
        }
        let ret_type = match self.consume_ident() {
            Ok(r) => Spanned::new(r.to_string(), 
                self.previous.start, self.previous.len),
            Err(_) => {
                let in_between_spot = self.previous.start + self.previous.len;
                Spanned::new("void".to_string(), 
                    in_between_spot, self.current.start - in_between_spot)
            },
        };
        let body = self.block();
        self.functions.insert(
            name.to_string(), Func::new(name.to_string(), ret_type, args, body)
        );
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

    fn call_from_expr_role(
        &mut self, role: &ExprRole, ast: &mut Ast
    ) -> Option<()> 
    {
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
                if self.current.obj_ref() == &Token::LeftParen {
                    let ident = Spanned::from_other_span(
                        ident.to_string(), self.previous
                    );
                    self.func_call(ast, ident);
                } else {
                    ast.nodes.push(AstNode::Expr(Spanned::new(
                        Expr::Var(ident.to_string()), start, end)
                    ));
                }
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

    fn func_call(&mut self, ast: &mut Ast, name: Spanned<String>) {
        let start = self.current.start;
        do_or_report_and!(self, self.consume(&Token::LeftParen) => {});
        let mut args = vec![];
        while !self.is_at_end() {
            if self.current.obj_ref() == &Token::RightParen { 
                self.advance();
                break; 
            }
            let arg = match self.expr() {
                Some(arg) => arg,
                None => {
                    self.report_error(
                        &Spanned::from_other_span(
                            CTError::new(CTErrorKind::ExpectedExpr),
                            self.current
                        ));
                    Spanned::new(Expr::Poison, 0, 0)
                }
            };
            args.push(arg);
            if let Err(er) = self.consume(&Token::Comma) {
                if self.current.obj_ref() == &Token::RightParen {
                    self.advance();
                    break;
                }
                self.report_error(&er);
            }
        }
        ast.nodes.push(AstNode::Expr(
            Spanned::new(
                Expr::Call { name, args, }, 
                self.previous.start - start,
                self.previous.start + self.previous.len - start
            )
        ));
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
