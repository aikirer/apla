#[derive(Debug)]
pub enum OpCode {
    OpAdd, OpSubtract, OpDivide, OpMultiply, OpModulo,
    OpNegate, OpSmaller, OpGreater, OpSmallerEqual, OpGreaterEqual,
    OpEqual, OpNotEqual,

    OpCreateVar(String), OpGetVar(String),
    OpSet,

    OpPushScope, OpPopScope,

    OpIf(usize), OpElse(usize),

    OpNumber(i32), OpFloat(f32), OpString(String), OpBool(bool),
}
