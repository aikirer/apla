use std::fmt::Display;

use crate::{token::{Token, PrecedenceLevel}, run_time::bytecode::OpCode, expr_type::ExprType, spanned::Spanned};

#[derive(Debug, Clone)]
pub enum Operator {
    Plus, Minus, Slash, Star, Percent, Smaller, SmallerEqual,
    Greater, GreaterEqual, Equal, NotEqual
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Slash => "/",
            Operator::Star => "*",
            Operator::Percent => "%",
            Operator::Smaller => "<",
            Operator::SmallerEqual => "<=",
            Operator::Greater => ">",
            Operator::GreaterEqual => ">=",
            Operator::Equal => "==",
            Operator::NotEqual => "!=",
        })
    }
}

#[derive(Debug)]
pub enum Expr {
    Int(i32), 
    Float(f32), 
    String(String),
    Bool(bool),
    Var(String),
    Binary {
        op: Operator,
        left: Box<Spanned<Expr>>,
        right: Box<Spanned<Expr>>,
    },
    Unary {
        expr: Box<Spanned<Expr>>
    },

    Poison
}

impl Operator {
    pub fn try_from_token(token: &Token) -> Option<Self> {
        match token {
            Token::Plus => Some(Self::Plus),
            Token::Minus => Some(Self::Minus),
            Token::Slash => Some(Self::Slash),
            Token::Star => Some(Self::Star),
            Token::Percent => Some(Self::Percent),
            Token::Greater => Some(Self::Greater),
            Token::GreaterEqual => Some(Self::GreaterEqual),
            Token::Smaller => Some(Self::Smaller),
            Token::SmallerEqual => Some(Self::SmallerEqual),
            Token::EqualsEquals => Some(Self::Equal),
            Token::NotEqual => Some(Self::NotEqual),
            _ => None,
        }
    }

    pub fn as_token(&self) -> Token {
        match self {
            Self::Plus => Token::Plus,
            Self::Minus => Token::Minus,
            Self::Slash => Token::Slash,
            Self::Star => Token::Star, 
            Self::Percent => Token::Percent,
            Self::Smaller => Token::Smaller,
            Self::SmallerEqual => Token::SmallerEqual,
            Self::Greater => Token::Greater,
            Self::GreaterEqual => Token::GreaterEqual,
            Self::Equal => Token::EqualsEquals,
            Self::NotEqual => Token::NotEqual,
        }
    }

    pub fn prec_level(&self) -> PrecedenceLevel {
        self.as_token().get_data().prec_level
    }

    pub fn as_opcode(&self) -> OpCode {
        match self {
            Operator::Plus => OpCode::OpAdd,
            Operator::Minus => OpCode::OpSubtract,
            Operator::Slash => OpCode::OpDivide,
            Operator::Star => OpCode::OpMultiply,
            Operator::Percent => OpCode::OpModulo,
            Operator::Smaller => OpCode::OpSmaller,
            Operator::SmallerEqual => OpCode::OpSmallerEqual,
            Operator::Greater => OpCode::OpGreater,
            Operator::GreaterEqual => OpCode::OpGreaterEqual,
            Operator::Equal => OpCode::OpEqual,
            Operator::NotEqual => OpCode::OpNotEqual,
        }
    }

    pub fn get_legal_types(&self) -> &[ExprType] {
        use ExprType as ET;
        match self {
            Operator::Plus => &[ET::Float, ET::Int, ET::String],
            Operator::Equal | Operator::NotEqual => &[ET::Any],
            _ => &[ET::Int, ET::Float],
        }
    }
}

impl Expr {
    pub fn try_from_token(token: &Token) -> Option<Expr> {
        match token {
            Token::Str(s) => Some(Expr::String(s.to_string())),
            Token::Float(f) => Some(Expr::Float(*f)),
            Token::Number(i) => Some(Expr::Int(*i)),
            _ => None,
        }
    }

    pub fn new_binary(
        left: Box<Spanned<Expr>>, right: Box<Spanned<Expr>>, op: Operator
    ) -> Expr 
    {
        Expr::Binary { 
            op, 
            left, 
            right 
        }
    }

    pub fn is_place(&self) -> bool {
        match self {
            Self::Var(_) => true,
            _ => false,
        }
    }
}
