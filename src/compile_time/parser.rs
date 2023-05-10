use std::collections::HashMap;

use crate::{
    spanned::Spanned,
    token::{ExprRole, PrecedenceLevel, Token}, 
    compile_time::util::func::Func, class::Class,
};

use super::{
    error::{CTError, CTErrorKind},
    ast::{
        expr::{Expr, Operator},
        Ast, AstNode, stmt::Stmt,
    },
};

macro_rules! do_or_report_and {
    ($self: expr, $action: expr => $and: stmt) => {
        if let Err(er) = $action {
            $self.report_error(&er);
            $and
        }
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
    pub classes: HashMap<String, Class>,
    pub ast: Ast,
    pub txt: &'a str,
    pub had_error: bool,
    pub new_parsed_files: Vec<(String, String)>,
    at: usize,
    parsed_files: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(
        tokens: &'a [SpanToken], 
        txt: &'a str,
        parsing_file: &'a str,
        parsed_files: &'a [&'a str]
    ) -> Self 
    {
        Self {
            tokens,
            current: &tokens[0],
            previous: &tokens[0],
            ast: Ast::new(),
            functions: HashMap::new(),
            classes: HashMap::new(),
            txt,
            had_error: false,
            at: 0,
            parsed_files: parsed_files.iter().map(|s| s.to_string()).collect(),
            new_parsed_files: vec![(parsing_file.to_string(), txt.to_string())],
        }
    }

    pub fn parse(mut self) -> Self {
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
                                &Spanned::from_other_span(er, self.previous)
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
            Token::Continue => Ok(Some(self.continue_stmt())),
            Token::Break => Ok(Some(self.break_stmt())),
            Token::Import => {
                self.import();
                Ok(None)
            },
            Token::Func => {
                // so hacky
                let func = match self.func_dec() {
                    Ok(f) => f,
                    Err(_) => return Ok(None),
                };
                self.functions.insert(func.0, func.1);
                // function doesn't have a node
                Ok(None)
            },
            Token::Class => {
                self.class_dec();
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
                            AstNode::Expr(Spanned::new_poisoned(Expr::Poison, 0, 0))
                        ))
                    }
                };
                match self.current.obj_ref() {
                    Token::Equals => Ok(Some(AstNode::Stmt(self.assignment(expr)))),

                    Token::PlusEquals | Token::MinusEquals | Token::SlashEquals |
                    Token::StarEquals | Token::PercentEquals => {
                        Ok(Some(AstNode::Stmt(self.compound_assignment(expr))))
                    },
                    _ => {
                        do_or_report_and!(self,
                            self.consume(&Token::Semicolon)
                            => {});
                        Ok(Some(AstNode::Expr(expr)))
                    }
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
                        self.report_error(&Spanned::from_other_span(
                            CTError::new(CTErrorKind::ExpectedStmt),
                            self.current,
                        ));
                        AstNode::Stmt(Stmt::poison())
                    }
                }
            },
            Err(er) => {
                self.report_error(&Spanned::from_other_span(
                    er.0, self.current
                ));
                AstNode::Stmt(Stmt::poison())
            }
        }
    }

    fn parse_type(&mut self) -> Result<Spanned<String>, Spanned<CTError>> {
        let file_id = self.current.file_id;
        let (pointer, mut_pointer) = if self.current.obj_ref() == &Token::Caret {
            self.advance();
            if self.current.obj_ref() == &Token::Mut {
                self.advance();
                (true, true)
            } else { (true, false) }
        } else { (false, false) };
        let (start, end) = (self.current.start, self.current.len);
        let mut ty = match self.consume_ident() {
            Ok(t) => t.to_string(),
            Err(er) => {
                self.report_error(&er);
                return Err(Spanned::from_other_span(
                    CTError::new(CTErrorKind::ExpectedIdent),
                    self.current
                ));
            }
        };
        if pointer {
            if mut_pointer {
                ty = "^mut ".to_string() + &ty;
            } else {
                ty = "^".to_string() + &ty;
            }
        }
        Ok(Spanned::new_with_file_id(ty.to_string(), start, end, file_id))
    }

    fn parse_var_name_and_type(
        &mut self, start: usize, end: usize
    ) -> Result<(Spanned<String>, Spanned<String>), AstNode>
    {
        let file_id = self.current.file_id;
        let name = match self.consume_ident() {
            Ok(n) => n,
            Err(er) => {
                self.report_error(&er);
                return Err(AstNode::Stmt(Spanned::new_poisoned(Stmt::Poison, 0, 0)));
            }
        };
        let name = Spanned::new_with_file_id(name.to_string(), start, end, file_id);
        let default_type = Spanned::new_with_file_id("_".to_string(), 0, 0, file_id);
        let ty = if self.current.obj_ref() == &Token::Colon {
            self.advance();
            self.parse_type().unwrap_or(default_type)
        } else { default_type };
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
        let file_id = self.current.file_id;
        let span = Spanned::new_with_file_id(
            Stmt::VarCreation { 
                is_mut: mutable,
                name, 
                ty, 
                value: None,
            }, 
            start, self.previous.start - start + self.previous.len, file_id
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
        let file_id = self.current.file_id;
        let mut span = Spanned::new_with_file_id(
            Stmt::VarCreation { 
                is_mut: mutable,
                name, 
                ty, 
                value: expr,
            }, 
            start, self.previous.start - start + self.previous.len, file_id
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
        let file_id = self.current.file_id;
        AstNode::Stmt(Spanned::new_with_file_id(
            Stmt::Block { nodes }, start, 
            self.current.start - start + self.current.len, file_id
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
                    &Spanned::from_other_span(
                        CTError::new(CTErrorKind::ExpectedExpr),
                        self.current
                    )
                );
                return AstNode::Stmt(Stmt::poison())
            }
        };
        let mut false_branch = None;
        let true_branch = match self.consume(&Token::RightParen) {
            Ok(_) => Box::new(self.safe_stmt()),
            Err(_) => Box::new(AstNode::Stmt(Stmt::poison()))
        };
        if self.current.obj_ref() == &Token::Else {
            self.advance();
            false_branch = Some(Box::new(self.safe_stmt()));
        }
        let file_id = self.current.file_id;
        AstNode::Stmt(
            Spanned::new_with_file_id(
                Stmt::If { 
                    condition, 
                    true_branch, 
                    false_branch
                },
                start, self.current.start - start + self.current.len, file_id
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
                    return AstNode::Expr(Spanned::new_poisoned(Expr::Poison, 0, 0));
                },
            }
        };
        let len = self.previous.start + self.previous.len - return_span.start;
        let file_id = self.current.file_id;
        AstNode::Stmt(
            Spanned::new_with_file_id(
                Stmt::Return { val: expr },    
                return_span.start, len, file_id
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
                    &Spanned::from_other_span(
                        CTError::new(CTErrorKind::ExpectedExpr),
                        self.current
                    )
                );
                return AstNode::Stmt(Stmt::poison())
            }
        };
        do_or_report_and_return!(self, self.consume(&Token::RightParen));
        let body = Box::new(self.safe_stmt());
        let file_id = self.current.file_id;
        AstNode::Stmt(Spanned::new_with_file_id(
            Stmt::While { 
                condition, body 
            },
            start, self.current.start - start + self.current.len, file_id
        ))
    }

    fn continue_stmt(&mut self) -> AstNode {
        let (start, len) = {
            let start_span = self.current.just_span_data();
            do_or_report_and_return!(self, self.consume(&Token::Continue));
            let next_span = self.current.just_span_data();
            do_or_report_and!(self, self.consume(&Token::Semicolon) => {});
            Spanned::get_start_and_len(&start_span, &next_span)
        };
        let file_id = self.current.file_id;
        AstNode::Stmt(Spanned::new_with_file_id(Stmt::Continue, start, len, file_id))
    }

    fn break_stmt(&mut self) -> AstNode {
        let (start, len) = {
            let start_span = self.current.just_span_data();
            do_or_report_and_return!(self, self.consume(&Token::Break));
            let next_span = self.current.just_span_data();
            do_or_report_and!(self, self.consume(&Token::Semicolon) => {});
            Spanned::get_start_and_len(&start_span, &next_span)
        };
        let file_id = self.current.file_id;
        AstNode::Stmt(Spanned::new_with_file_id(Stmt::Break, start, len, file_id))
    }

    fn import(&mut self) {
        do_or_report_and!(self, self.consume(&Token::Import) => return);
        let file_path = match self.consume_str() {
            Ok(s) => s.to_string() + ".apla",
            Err(er) => {
                self.report_error(&er);
                return;
            }
        };
        do_or_report_and!(self, self.consume(&Token::Semicolon) => ());
        if self.parsed_files.contains(&file_path) {
            return;
        }
        self.parsed_files.push(file_path.to_string());
        let file = match std::fs::read_to_string(file_path.to_string()) {
            Ok(content) => content,
            Err(_) => todo!(),
        };
        let parsed_files = self.parsed_files.iter().map(|str| str.as_ref()).collect::<Vec<&_>>();
        let (_, had_error, functions, classes, files) = crate::parse_file(
            &file, 
            &parsed_files, 
            &file_path,
        );
        self.had_error = had_error;
        self.functions.extend(functions);   
        self.classes.extend(classes);
        self.new_parsed_files.extend(files);
    }

    fn func_dec(&mut self) -> Result<(String, Func), ()>{
        do_or_report_and!(self, self.consume(&Token::Func) => {});
        let name = match self.consume_ident() {
            Ok(n) => n,
            Err(er) => {
                self.report_error(&er);
                return Err(());
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
                    self.report_error(&Spanned::from_other_span(
                        CTError::new(CTErrorKind::Unexpected(self.current.cloned())),
                        self.current
                        )
                    );
                }
            }
        }
        let file_id = self.current.file_id;
        let ret_type = match self.consume_ident() {
            Ok(r) => Spanned::from_other_span(r.to_string(), 
                self.previous),
            Err(_) => {
                let in_between_spot = self.previous.start + self.previous.len;
                Spanned::new_with_file_id("void".to_string(), 
                    in_between_spot, self.current.start - in_between_spot, file_id)
            },
        };
        let body = self.block();
        Ok((name.to_string(), Func::new(name.to_string(), ret_type, args, body)))
    }

    fn class_dec(&mut self) {
        do_or_report_and!(self, self.consume(&Token::Class) => {});
        let this = AstNode::Stmt(Spanned::zeroed(
            Stmt::VarCreation { 
                is_mut: false, 
                name: Spanned::zeroed("this".to_string()), 
                ty: Spanned::zeroed(super::THIS_AS_STR.to_string()), 
                value: None 
            }
        ));
        let name = match self.consume_ident() {
            Ok(n) => n,
            Err(er) => {
                self.report_error(&er);
                return;
            }
        };
        do_or_report_and!(self, self.consume(&Token::LeftBrace) => {});
        let mut class = Class::new(name.to_string());
        loop {
            if self.is_at_end() || self.current.obj_ref() == &Token::RightBrace {
                break;
            }
            match self.current.obj_ref() {
                Token::Var | Token::Mut => class.fields.push(self.var_creation()),
                Token::Func => {
                    let mut func = match self.func_dec() {
                        Ok(f) => f,
                        Err(_) => continue,
                    };
                    let mut new_args = vec![this.clone()];
                    new_args.extend(func.1.args);
                    func.1.args = new_args;
                    class.methods.insert(func.0, func.1);
                }
                _ => panic!(),
            }
        }
        if !self.is_at_end() {
            do_or_report_and!(self, self.consume(&Token::RightBrace) => {});
        }
        self.classes.insert(name.to_string(), class);
    }

    fn expr_or_reported(&mut self) -> Spanned<Expr> {
        self.expr().unwrap_or_else(|| {
            self.report_error(&self.make_error(
                CTErrorKind::ExpectedExpr,
            ));
            Spanned::new_poisoned(Expr::Poison, 0, 0)
        })
    }
    
    fn assignment(&mut self, left: Spanned<Expr>) -> Spanned<Stmt> {
        let start_span = self.current.just_span_data();
        do_or_report_and!(self, self.consume(&Token::Equals) => {});
        let right = self.expr_or_reported();
        do_or_report_and!(self, self.consume(&Token::Semicolon) => {});
        let (start, end) = Spanned::get_start_and_len(&start_span, self.previous);
        let file_id = self.current.file_id;
        Spanned::new_with_file_id(Stmt::Assignment { 
            left: Box::new(left), 
            right: Box::new(right) 
        }, start, end, file_id)
    }

    fn compound_assignment(&mut self, left: Spanned<Expr>) -> Spanned<Stmt> {
        let start_span = self.current.just_span_data();
        let op = self.current.obj_ref();
        self.advance();
        let expr = self.expr_or_reported();
        do_or_report_and!(self, self.consume(&Token::Semicolon) => {});
        let op = match op {
            Token::PlusEquals => Operator::Plus, 
            Token::MinusEquals => Operator::Minus, 
            Token::StarEquals => Operator::Star,
            Token::SlashEquals => Operator::Slash, 
            Token::PercentEquals => Operator::Percent,
            _ => panic!(),
        };
        let (start, end) = Spanned::get_start_and_len(&start_span, self.previous);
        let file_id = self.current.file_id;
        Spanned::new_with_file_id(Stmt::Assignment { 
            left: Box::new(left.clone()), 
            right: Box::new(Spanned::new_with_file_id(
                    Expr::Binary { 
                    op, 
                    left: Box::new(left), 
                    right: Box::new(expr), 
                }, start, end, file_id
            ))
        }, start, end, file_id)
    }

    fn consume_ident(&mut self) -> Result<&'a str, Spanned<CTError>> {
        if let Token::Identifier(s) = &**self.current {
            self.advance();
            Ok(s)
        } else {
            Err(self.make_error(CTErrorKind::ExpectedIdent))
        }
    } 
    
    fn consume_str(&mut self) -> Result<&'a str, Spanned<CTError>> {
        if let Token::Str(s) = &**self.current {
            self.advance();
            Ok(s)
        } else {
            Err(self.make_error(CTErrorKind::ExpectedStr))
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
            ExprRole::Index => self.index(ast),
            ExprRole::Get => self.expr_get(ast),
            ExprRole::GetPointer => self.get_pointer(ast),
            ExprRole::Deref => self.deref(ast),
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
        self.parse_prec(op.prec_level().one_up(), ast);
        let right = match ast.nodes.pop() {
            Some(node) => node,
            None => {
                self.report_error(&self.make_error(CTErrorKind::Unexpected(self.current.cloned())));
                poisoned = true;
                AstNode::Expr(Spanned::new_poisoned(Expr::Int(0), 0, 0))
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
        let file_id = self.current.file_id;
        let mut span = Spanned::new_with_file_id(
            Expr::new_binary(
                Box::new(left),
                Box::new(right),
                op,
            ),
            start, end, file_id
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
                AstNode::Expr(Spanned::new_poisoned(Expr::Int(0), 0, 0))
            }
        };
        let expr = match expr {
            AstNode::Expr(e) => e,
            _ => panic!(),
        };
        let (start, size) = (token_start, 
            (self.previous.start - token_start) + self.previous.len
        );
        let file_id = self.current.file_id;
        ast.nodes.push(AstNode::Expr(Spanned::new_with_file_id(Expr::Unary { 
            expr: Box::new(expr)
        }, start, size, file_id)));
    }

    fn object(&mut self, ast: &mut Ast) {
        match self.consume_ident() {
            Ok(ident) => {
                if self.current.obj_ref() == &Token::LeftParen {
                    let ident = Spanned::from_other_span(
                        ident.to_string(), self.previous
                    );
                    self.func_call(ast, ident);
                } else {
                    ast.nodes.push(AstNode::Expr(Spanned::from_other_span(
                        Expr::Var(ident.to_string()), self.current)
                    ));
                }
            },
            Err(er) => {
                self.advance();
                self.report_error(&er);
                ast.nodes.push(AstNode::Expr(Spanned::new_poisoned(
                    Expr::Poison, self.current.start, self.current.len,
                )))
            }
        };
    }

    fn literal(&mut self, ast: &mut Ast) {
        self.advance();
        ast.nodes.push(AstNode::Expr(Spanned::from_other_span(
            Expr::try_from_token(self.previous).unwrap(),
                self.previous)
        ));
    }

    fn grouping(&mut self, ast: &mut Ast) {
        do_or_report_and!(self, self.consume(&Token::LeftParen) => {});
        self.parse_prec(PrecedenceLevel::Assignment, ast);
        do_or_report_and!(self, self.consume(&Token::RightParen) => {});
    }

    fn index(&mut self, ast: &mut Ast) {
        let start_span = self.current.just_span_data();
        let object = match ast.nodes.pop() {
            Some(obj) => obj,
            None => {
                self.report_error(&Spanned::from_other_span(
                    CTError::new(CTErrorKind::ExpectedExpr), self.previous));
                AstNode::Expr(Spanned::new_poisoned(Expr::Poison, 0, 0))
            }
        };
        do_or_report_and!(self, self.consume(&Token::LeftBracket) => {});
        let index = self.expr_or_reported();
        do_or_report_and!(self, self.consume(&Token::RightBracket) => {});
        let last_span = self.previous.just_span_data();
        let (start, len) = Spanned::get_start_and_len(&start_span, &last_span);
        let file_id = self.current.file_id;
        ast.nodes.push(
            AstNode::Expr(Spanned::new_with_file_id(
                Expr::Index { 
                    object: Box::new(object), 
                    i: Box::new(index) 
                }, start, len, file_id))
        )
    }

    fn expr_get(&mut self, ast: &mut Ast) {
        let mut poisoned = false;
        self.advance();
        let left = ast.nodes.pop().unwrap();
        let start;
        self.parse_prec(PrecedenceLevel::Unary, ast);
        let right = match ast.nodes.pop() {
            Some(r) => r,
            None => {
                self.report_error(&Spanned::from_other_span(
                    CTError::new(CTErrorKind::ExpectedExpr), self.current));
                poisoned = true;
                AstNode::Expr(Spanned::new(Expr::Poison, 0, 0))
            },
        };
        let (left, right) = match (left, right) {
            (AstNode::Expr(a), AstNode::Expr(b)) => {
                start = a.just_span_data();
                (a, b)
            },
            _ => panic!(),
        };
        let end = self.current.just_span_data();
        let (start, end) = Spanned::get_start_and_len(&start, &end);
        let file_id = self.current.file_id;
        let mut span = Spanned::new_with_file_id(
            Expr::Get {
                left: Box::new(left),
                right: Box::new(right),
            }, start, end, file_id
        );
        if poisoned { span.poison(); }
        ast.nodes.push(AstNode::Expr(span))
    }

    fn get_pointer(&mut self, ast: &mut Ast) {
        self.advance();
        let is_mut = if self.current.obj_ref() == &Token::Mut {
            self.advance();
            true
        } else { false };
        self.parse_prec(PrecedenceLevel::Unary, ast);
        let Some(node) = ast.nodes.pop() else {
            self.report_error(&Spanned::from_other_span(
                CTError::new(CTErrorKind::ExpectedExpr), self.current));
            return;
        };
        let (span_data, node) = match node {
            AstNode::Expr(e) => {
                (e.just_span_data(), Expr::MakePointer { expr: Box::new(e), is_mut })
            },
            _ => panic!(),
        };
        ast.nodes.push(AstNode::Expr(Spanned::from_other_span(
            node, &span_data)));
    }

    fn deref(&mut self, ast: &mut Ast) {
        self.advance();
        self.parse_prec(PrecedenceLevel::Unary, ast);
        let Some(node) = ast.nodes.pop() else {
            self.report_error(&Spanned::from_other_span(
                CTError::new(CTErrorKind::ExpectedExpr), self.current));
            return;
        };
        let (span_data, node) = match node {
            AstNode::Expr(e) => {
                (e.just_span_data(), Expr::Deref { expr: Box::new(e) })
            },
            _ => panic!(),
        };
        ast.nodes.push(AstNode::Expr(Spanned::from_other_span(
            node, &span_data)));
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
                    Spanned::new_poisoned(Expr::Poison, 0, 0)
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
        let file_id = self.current.file_id;
        ast.nodes.push(AstNode::Expr(
            Spanned::new_with_file_id(
                Expr::Call { name, args, }, 
                self.previous.start - start,
                self.previous.start + self.previous.len - start,
                file_id
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
        Spanned::from_other_span(
            CTError::new(error),
            self.current
        )
    }

    fn report_error(&mut self, error: &Spanned<CTError>) {
        self.had_error = true;
        super::error::report_error(error, &self.new_parsed_files);
    }

    fn is_at_end(&self) -> bool {
        self.current.obj_ref() == &Token::Eof
    }
}
