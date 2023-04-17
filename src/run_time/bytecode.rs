#[derive(Debug)]
pub enum OpCode {
    OpAdd, OpSubtract, OpDivide, OpMultiply, OpModulo,
    OpNegate,

    OpNumber(i32), OpFloat(f32), OpString(String),
}
