use std::fmt::Display;

use crate::{compile_time::{error::{CTError, CTErrorKind}, self}, class::ParsedClass};

#[derive(Clone, Debug)]
pub enum ExprType {
    Int, Float, String, Bool, ToBeInferred, Any, Null,
    ClassThis,
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
            ExprType::ClassThis => "this".to_string(),
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

pub fn extract_type_data_from_str(str: &str) -> (bool, bool, &str) {
    if str.get(0..1) == Some("^") {
        if str.get(1..4) == Some("mut") {
            (true, true, &str[5..]) // skip whitespace after mut
        } else {
            (true, false, &str[1..])
        }
    } else {
        (false, false, str)
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
        let (is_pointer, mut_pointer, type_str) = extract_type_data_from_str(value);
        let matched_type = match type_str {
            "int" => Self::Int,
            "float" => Self::Float,
            "str" => Self::String,
            "bool" => Self::Bool,
            "void" => Self::Null,
            compile_time::THIS_AS_STR => Self::ClassThis,
            "_" => Self::ToBeInferred,
            _ => return error,
        };
        if is_pointer {
            Ok(Self::Pointer { 
                points_to: Box::new(matched_type), 
                is_mut: mut_pointer
            })
        } else {
            Ok(matched_type)
        }
    }
}

impl PartialEq for ExprType {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Class(a) => match other {
                Self::Class(b) => a.name == b.name,
                _ => false,
            }
            Self::Pointer { points_to, is_mut } => {
                let is_mut_first = is_mut;
                let points_to_first = points_to;
                match other {
                    Self::Pointer { points_to, is_mut } => 
                        points_to == points_to_first && is_mut == is_mut_first,
                    _ => false,
                }
            },
            _ => match (self, other) {
                (ExprType::Int, ExprType::Int) |
                (ExprType::Float, ExprType::Float) |
                (ExprType::String, ExprType::String) |
                (ExprType::Bool, ExprType::Bool) |
                (ExprType::ToBeInferred, ExprType::ToBeInferred) |
                (ExprType::Any, ExprType::Any) |
                (ExprType::Null, ExprType::Null) |
                (ExprType::ClassThis, ExprType::ClassThis) => true,
                _ => false,
            }
        }
    }
}
