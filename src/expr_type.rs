use std::fmt::Display;

use crate::{compile_time::error::{CTError, CTErrorKind}, class::ParsedClass};

#[derive(Clone, Debug, PartialEq)]
pub enum ExprType {
    Int, Float, String, Bool, ToBeInferred, Any, Null,
    Class(ParsedClass), Pointer(Box<ExprType>),
}

impl ExprType {
    pub fn is_indexable(&self) -> Option<ExprType> {
        match self {
            Self::String => Some(Self::String),
            _ => None,
        }
    }

    pub fn as_pointer(self) -> Self {
        Self::Pointer(Box::new(self))
    }
}

impl Display for ExprType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            ExprType::Int => "int".to_string(),
            ExprType::Float => "float".to_string(),
            ExprType::String => "string".to_string(),
            ExprType::Bool => "bool".to_string(),
            ExprType::ToBeInferred => "not inferred".to_string(),
            ExprType::Any => "any".to_string(),
            ExprType::Null => "null".to_string(),
            ExprType::Class(_) => "class".to_string(),
            ExprType::Pointer(p) => format!("pointer to {p}"),
        })
    }
}

impl TryFrom<&str> for ExprType {
    type Error = CTError;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let error = Err(CTError::new(CTErrorKind::ExpectedType));
        if value.is_empty() {
            return error;
        }
        let (is_pointer, type_str) = if value.chars().next().unwrap() == '^' {
            (true, &value[1..])
        } else {
            (false, value)
        };
        if type_str == "void" {
            return Ok(Self::Null);
        }
        let matched_type = match type_str {
            "int" => Self::Int,
            "float" => Self::Float,
            "str" => Self::String,
            "bool" => Self::Bool,
            "_" => Self::ToBeInferred,
            _ => return error,
        };
        if is_pointer {
            Ok(Self::Pointer(Box::new(matched_type)))
        } else {
            Ok(matched_type)
        }
    }
}


#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_types() {
        let int = ExprType::try_from("int");
        let str_ptr = ExprType::try_from("^str");
        let to_be_inferred_ptr = ExprType::try_from("^_");
        assert_eq!(int, Ok(ExprType::Int));
        assert_eq!(str_ptr, Ok(ExprType::Pointer(Box::new(ExprType::String))));
        assert_eq!(to_be_inferred_ptr, Ok(ExprType::Pointer(Box::new(ExprType::ToBeInferred))));
    }
}
