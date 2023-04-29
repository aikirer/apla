use std::fmt::Display;

use crate::{compile_time::error::{CTError, CTErrorKind}, class::ParsedClass};

#[derive(Clone, Debug, PartialEq)]
pub enum ExprType {
    Int, Float, String, Bool, ToBeInferred, Any, Null,
    Class(ParsedClass), Pointer {
        points_to: Box<ExprType>,
        is_mut: bool,
    },
}

impl ExprType {
    pub fn is_indexable(&self) -> Option<ExprType> {
        match self {
            Self::String => Some(Self::String),
            _ => None,
        }
    }

    pub fn as_pointer(self, is_mut: bool) -> Self {
        Self::Pointer {
            points_to: Box::new(self), is_mut
        }
    }

    pub fn derefed(&self) -> Result<Self, CTErrorKind> {
        match self {
            Self::Pointer { points_to, is_mut: _ } => Ok(*points_to.clone()),
            _ => Err(CTErrorKind::CantDeref)
        }
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
            ExprType::Pointer { points_to, is_mut } => {
                if *is_mut {
                    format!("@mut {points_to}")
                } else {
                    format!("@{points_to}")
                }
            }
        })
    }
}

impl TryFrom<&str> for ExprType {
    type Error = CTError;
    // This is terrible
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let error = Err(CTError::new(CTErrorKind::ExpectedType));
        if value.is_empty() {
            return error;
        }
        let (is_pointer, mut type_str) = if value.chars().next().unwrap() == '^' {
            (true, &value[1..])
        } else {
            (false, value)
        };
        let mut_pointer = if type_str.get(0..=2) == Some("mut") {
            type_str = &type_str[4..]; // 'mut '
            true
        } else { false };
        let matched_type = match type_str {
            "int" => Self::Int,
            "float" => Self::Float,
            "str" => Self::String,
            "bool" => Self::Bool,
            "void" => Self::Null,
            "_" => Self::ToBeInferred,
            _ => return error,
        };
        if is_pointer {
            Ok(dbg!(Self::Pointer { 
                points_to: Box::new(matched_type), 
                is_mut: mut_pointer
            }))
        } else {
            Ok(matched_type)
        }
    }
}
