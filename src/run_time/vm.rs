use super::{bytecode::OpCode, stack::{Stack}, error::RTError};
// use crate::binary_op;

pub struct VM {
    stack: Stack
}

// prefixed with Op
use OpCode::*;

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Stack::new()
        }
    }

    pub fn execute(&mut self, code: &[OpCode]) -> Result<(), RTError> {
        let mut at = 0;
        while let Some(opcode) = code.get(at) {
            match opcode {
                op @ (OpAdd | OpMultiply | OpDivide |
                    OpModulo | OpSubtract) => self.bin_op(op)?,
                OpNegate => todo!(),
                OpNumber(n) => self.stack.push(*n),
            }
            at += 1;
        }
        println!("{:?}", self.stack.pop());
        Ok(())
    }

    fn bin_op(&mut self, kind: &OpCode) -> Result<(), RTError>{
        let b = self.stack.pop()?;
        let a = self.stack.pop()?;
        self.stack.push_dir(match kind {
            OpAdd => (a + b)?,
            OpSubtract => (a - b)?,
            OpMultiply => (a * b)?,
            OpDivide => (a / b)?,
            OpModulo => (a % b)?,
            _ => panic!()
        });
        Ok(())
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

