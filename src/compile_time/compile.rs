use std::collections::HashMap;

use crate::{run_time::bytecode::OpCode, call::Call};

pub type Output = Vec<OpCode>;

#[derive(Debug)]
pub struct Ctx<'a> {
    pub functions: &'a HashMap<String, Box<dyn Call>>
}

pub trait Compile {
    fn compile(&self, ctx: &Ctx) -> Output;    

    fn add(&self, out: &mut Output, instr: OpCode) {
        out.push(instr);
    }
}

// implementations in ast_compiler.rs
