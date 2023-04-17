use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum ExprType {
    Int, Float, String, Bool, ToBeInferred, Any,
}

impl Display for ExprType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            ExprType::Int => "int",
            ExprType::Float => "float",
            ExprType::String => "string",
            ExprType::Bool => "bool",
            ExprType::ToBeInferred => "not inferred",
            ExprType::Any => "any",
        })
    }
}