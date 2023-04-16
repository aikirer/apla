use crate::{token::{Token, PrecedenceLevel}, run_time::bytecode::OpCode};

#[derive(Debug)]
pub enum Operator {
    Plus, Minus, Slash, Star, Percent
}

#[derive(Debug)]
pub enum Expr {
    Int(i32), 
    Float(f32), 
    String(String), 
    Ident(String),
    Binary {
        op: Operator,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Unary {
        expr: Box<Expr>
    }
}

impl Operator {
    pub fn try_from_token(token: &Token) -> Option<Self> {
        match token {
            Token::Plus => Some(Self::Plus),
            Token::Minus => Some(Self::Minus),
            Token::Slash => Some(Self::Slash),
            Token::Star => Some(Self::Star),
            Token::Percent => Some(Self::Percent),
            _ => None,
        }
    }

    pub fn as_token(&self) -> Token {
        match self {
            Self::Plus => Token::Plus,
            Self::Minus => Token::Minus,
            Self::Slash => Token::Slash,
            Self::Star => Token::Star, 
            Self::Percent => Token::Percent
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
        }
    }
}

impl Expr {
    pub fn try_from_token(token: &Token) -> Option<Expr> {
        match token {
            Token::Str(s) => Some(Expr::String(s.to_string())),
            Token::Float(f) => Some(Expr::Float(*f)),
            Token::Number(i) => Some(Expr::Int(*i)),
            Token::Identifier(s) => Some(Expr::Ident(s.to_string())),
            _ => None,
        }
    }

    pub fn new_binary(
        left: Box<Expr>, right: Box<Expr>, op: Operator
    ) -> Expr 
    {
        Expr::Binary { 
            op, 
            left, 
            right 
        }
    }
}
