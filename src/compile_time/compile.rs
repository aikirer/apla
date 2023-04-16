use crate::run_time::bytecode::OpCode;

pub type Output = Vec<OpCode>;

pub trait Compile {
    fn compile(&self) -> Output;    

    fn add(&self, out: &mut Output, instr: OpCode) {
        out.push(instr);
    }
}

// implementations in ast_compiler.rs
