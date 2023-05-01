use std::fmt::Debug;

use crate::{
    expr_type::ExprType, compile_time::{ast::expr::Expr, resolver::Resolver, error::CTError, compile}, 
    run_time::{bytecode::OpCode, stack::StackVal, vm::VM, error::RTError}, spanned::Spanned, class::ParsedClass
};

pub trait Call
where
    Self: Debug
{
    fn get_arg_list(&self) -> Vec<&ExprType>;
    fn compile_call(
        &self, args: &[Spanned<Expr>], ctx: &compile::Ctx
    ) -> Vec<OpCode>;

    fn compile_to_code(&self, ctx: &compile::Ctx);

    fn resolve(
        &self, node: &Spanned<Expr>, resolver: &Resolver
    ) -> Result<(), Spanned<CTError>>;

    fn get_return_type(&self, node: &Spanned<Expr>) -> ExprType;

    fn call(&self, vm: &mut VM) -> Result<StackVal, RTError>;

    fn as_class(&self) -> Option<&ParsedClass> {
        None
    }
    fn as_class_mut(&mut self) -> Option<&mut ParsedClass> {
        None
    }
}
