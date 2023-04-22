use std::fmt::Debug;

use crate::{
    expr_type::ExprType, compile_time::ast::AstNode, 
    run_time::{bytecode::OpCode, stack::StackVal, vm::VM, error::RTError}
};

pub trait Call 
where
    Self: Debug
{
    fn get_arg_list(&self) -> Vec<&ExprType>;
    fn compile_call(&self, node: &AstNode) -> Vec<OpCode>;

    fn runtime_call(&self, vm: &mut VM) -> Result<StackVal, RTError>;
}
