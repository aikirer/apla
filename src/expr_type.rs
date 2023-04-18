use std::fmt::Display;

use crate::compile_time::error::CTError;

#[derive(Clone, Debug, PartialEq)]
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

impl TryFrom<&str> for ExprType {
    type Error = CTError;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "int" => Ok(Self::Int), 
            "float" => Ok(Self::Float), 
            "str" => Ok(Self::String),
            "" => Ok(Self::ToBeInferred),
            _ => Err(CTError::new(crate::compile_time::error::CTErrorKind::ExpectedType)),
        }
    }
}
