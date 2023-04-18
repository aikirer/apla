// use std::collections::HashMap;

// use crate::{spanned::Spanned, expr_type::ExprType};

// use super::{error::{CTError, report_error, CTErrorKind}, ast::{Ast, AstNode, expr::Expr, stmt::Stmt}, util::variable::Variable};


// fn make_error(
//     kind: CTErrorKind, start: usize, len: usize,
// ) -> Option<Spanned<CTError>> 
// {
//     Some(Spanned {
//         obj: CTError::new(kind),
//         start,
//         len,
//         poisoned: false,
//     })
// }


// pub trait Resolve {
//     type OkOutput;

//     fn resolve(
//         &mut self, ctx: &mut Ctx,
//     ) -> Result<Self::OkOutput, Option<Spanned<CTError>>>;

// }

// impl Ast {
//     pub fn resolve(&mut self) -> Result<(), Option<Spanned<CTError>>> {
//         let binding = self.text.as_ref().unwrap();
//         let mut ctx = Ctx::new(&binding);
//         let mut had_error = false;
//         for node in &mut self.nodes {
//             if let Err(er) = node.resolve(&mut ctx) {
//                 had_error = true;
//                 if let Some(er) = er {
//                     if let Some(text) = &self.text {
//                         report_error(&er, text)
//                     }
//                 }
//             }
//         }
//         if !had_error { Ok(()) } else { 
//             Err(make_error(CTErrorKind::HadError, 0, 0))
//         }
//     }
// }

// impl Resolve for AstNode {
//     type OkOutput = ();
//     fn resolve(&mut self, ctx: &mut Ctx) -> Result<Self::OkOutput, Option<Spanned<CTError>>> {
//         match self {
//             AstNode::Expr(e) => {e.resolve(ctx)?; ()},
//             AstNode::Stmt(e) => {e.resolve(ctx)?; ()},
//         };
//         Ok(())
//     }
// }

// impl Resolve for Spanned<Expr> {
//     type OkOutput = ExprType;
//     fn resolve(&mut self, ctx: &mut Ctx) -> Result<Self::OkOutput, Option<Spanned<CTError>>> {
        
//     }
// }

// impl Resolve for Spanned<Stmt> {
//     type OkOutput = ();

//     fn resolve(
//         &mut self, ctx: &mut Ctx,
//     ) -> Result<Self::OkOutput, Option<Spanned<CTError>>> 
//     {
//         match &mut **self {
//             Stmt::VarCreation { is_mut, name, ty, value } => {
//                 let mut poisoned = false;
//                 let ty = match ExprType::try_from(ty as &str) {
//                     Ok(t) => {
//                         match value.resolve(ctx) {
//                             Ok(t2) => {
//                                 if &t != &t2 {
//                                     report_error(&Spanned::new(CTError::new(
//                                         CTErrorKind::MismatchedTypes(t.clone(), t2)), 
//                                         value.start, value.len
//                                     ), ctx.text);
//                                 }
//                             },
//                             Err(er) => match er {
//                                 Some(er) => report_error(
//                                     &er, ctx.text
//                                 ),
//                                 None => poisoned = true,
//                             }
//                         }
//                         t
//                     },
//                     Err(er) => {
//                         report_error(
//                             &Spanned::new(er, ty.start, ty.len), 
//                             ctx.text
//                         );
//                         poisoned = true;
//                         ExprType::Int
//                     },
//                 };
//                 let mut var = Variable::new(name.to_string(), ty, *is_mut);
//                 if poisoned { var.poison() }
//                 ctx.variables.insert(name.to_string(), var);
//             },
//         }
//         Ok(())    
//     }
// }
