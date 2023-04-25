use std::fmt::Debug;

use crate::{
    expr_type::ExprType, compile_time::{ast::expr::Expr, resolver::Resolver, error::CTError, compile, util::func::ParsedFunc}, 
    run_time::{bytecode::OpCode, stack::StackVal, vm::VM, error::RTError}, spanned::Spanned
};

pub trait Call 
where
    Self: Debug
{
    fn get_arg_list(&self) -> Vec<&ExprType>;
    fn compile_call(
        &self, args: &[Spanned<Expr>], ctx: &compile::Ctx
    ) -> Vec<OpCode>;

    fn resolve(
        &self, node: &Spanned<Expr>, resolver: &Resolver
    ) -> Result<(), Spanned<CTError>>;

    fn get_return_type(&self, node: &Spanned<Expr>) -> ExprType;

    fn as_parsed_func(&self) -> Option<&ParsedFunc>;

    fn call(&self, vm: &mut VM) -> Result<StackVal, RTError>;
}
