#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
    OpAdd, OpSubtract, OpDivide, OpMultiply, OpModulo,
    OpNegate, OpSmaller, OpGreater, OpSmallerEqual, OpGreaterEqual,
    OpEqual, OpNotEqual,

    OpCreateVar(String), OpGetVar(String),
    OpSet,

    OpPushScope, OpPopScope,

    OpIf(usize), OpElse(usize),

    OpCall(String),

    OpNumber(i32), OpFloat(f32), OpString(String), OpBool(bool),

    OpLoop(usize), OpEndLoop(usize), OpBreak, OpContinue,

    OpIndex, OpGetField(String), OpGetMethod(String),
    OpMakePointer, OpDeref,

    OpReturn,
}
