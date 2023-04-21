use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Plus, Minus, Slash, Percent, Star, Colon, Dot, Equals, EqualsEquals,
    BitAnd, BitOr, Greater, Smaller, NotEqual, Bang, Comma,
    LeftParen, LeftBracket, LeftBrace, RightBrace, RightBracket, 
    RightParen, At, Semicolon,

    And, Or, BitRight, BitLeft, SmallerEqual, 
    GreaterEqual, PlusPlus, MinusMinus,

    Func, For, While, Class, Enum, If, Else, Break, Continue,
    Return, Var, Mut,

    Bool(bool), Number(i32), Float(f32), Identifier(String), Str(String),
    
    Unknown(char), 
    Eof,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            Token::Plus => "+".to_string(),
            Token::Minus => "-".to_string(),
            Token::Slash => "/".to_string(),
            Token::Percent => "%".to_string(),
            Token::Star => "*".to_string(),
            Token::Colon => ":".to_string(),
            Token::Dot => ".".to_string(),
            Token::Equals => "=".to_string(),
            Token::EqualsEquals => "==".to_string(),
            Token::BitAnd => "&".to_string(),
            Token::BitOr => "|".to_string(),
            Token::Greater => ">".to_string(),
            Token::Smaller => "<".to_string(),
            Token::NotEqual => "!=".to_string(),
            Token::Bang => "!".to_string(),
            Token::Comma => ",".to_string(),
            Token::LeftParen => "(".to_string(),
            Token::LeftBracket => "[".to_string(),
            Token::LeftBrace => "{".to_string(),
            Token::RightBrace => "}".to_string(),
            Token::RightBracket => "]".to_string(),
            Token::RightParen => ")".to_string(),
            Token::At => "@".to_string(),
            Token::Semicolon => ";".to_string(),
            Token::And => "&&".to_string(),
            Token::Or => "||".to_string(),
            Token::BitRight => ">>".to_string(),
            Token::BitLeft => "<<".to_string(),
            Token::SmallerEqual => "<=".to_string(),
            Token::GreaterEqual => ">=".to_string(),
            Token::PlusPlus => "++".to_string(),
            Token::MinusMinus => "--".to_string(),
            Token::Func => "func".to_string(),
            Token::For => "for".to_string(),
            Token::While => "while".to_string(),
            Token::Class => "class".to_string(),
            Token::Enum => "enum".to_string(),
            Token::If => "if".to_string(),
            Token::Else => "else".to_string(),
            Token::Break => "break".to_string(),
            Token::Continue => "continue".to_string(),
            Token::Return => "return".to_string(),
            Token::Var => "var".to_string(),
            Token::Mut => "mut".to_string(),
            Token::Bool(b) => b.to_string(),
            Token::Number(n) => n.to_string(),
            Token::Unknown(c) => c.to_string(),
            Token::Float(f) => f.to_string(),
            Token::Identifier(id) => id.to_string(),
            Token::Str(s) => format!("\"{s}\""),
            Token::Eof => "".to_string(),
        };
        write!(f, "{text}")
    }
}

impl Token {
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.to_string().len()
    }
}

pub fn str_to_reserved(str: &str) -> Option<Token> {
    use Token::*;
    match str {
        "func" => Some(Func),
        "for" => Some(For),
        "while" => Some(While),
        "class" => Some(Class), 
        "enum" => Some(Enum), 
        "if" => Some(If), 
        "else" => Some(Else), 
        "break" => Some(Break), 
        "continue" => Some(Continue),
        "return" => Some(Return),
        "var" => Some(Var),
        "mut" => Some(Mut),
        "true" => Some(Bool(true)),
        "false" => Some(Bool(false)),
        _ => None
    }
}

pub fn str_to_reserved_or_ident(str: &str) -> Token {
    str_to_reserved(str).unwrap_or(Token::Identifier(str.to_string()))
}

#[derive(Debug)]
pub enum PrecedenceLevel {
    None, Assignment, Or, And, Equality, Comparison,
    Term, Factor, Unary, Primary,
}

#[derive(Debug)]
pub enum ExprRole {
    Literal, Grouping, Binary, None, Unary, Object, 
}

#[derive(Debug)]
pub struct TokenData {
    pub prec_level: PrecedenceLevel,
    pub prefix: ExprRole,
    pub midfix: ExprRole,
}

impl TokenData {
    pub fn new(lvl: PrecedenceLevel, pre: ExprRole, mid: ExprRole) -> Self {
        TokenData {
            prec_level: lvl,
            prefix: pre,
            midfix: mid,
        }
    }
}

impl Token {
    pub fn get_data(&self) -> TokenData {
        use ExprRole as Role;
        use PrecedenceLevel as Lvl;
        use Token::*;
        let data = match self {
            LeftParen => (Lvl::Factor, Role::Grouping, Role::None),
            Plus => (Lvl::Term, Role::None, Role::Binary),
            Minus => (Lvl::Term, Role::Unary, Role::Binary),
            Percent | Slash | Star => (Lvl::Factor, Role::None, Role::Binary),
            Bang => (Lvl::Factor, Role::Unary, Role::None),
            Str(_) => (Lvl::Factor, Role::Literal, Role::None),
            Float(_) | Number(_) => (Lvl::Factor, Role::Literal, Role::None),
            Identifier(_) => (Lvl::Factor, Role::Object, Role::None),
            Bool(_) => (Lvl::Factor, Role::Literal, Role::None),
            Greater | Smaller | GreaterEqual | SmallerEqual |
            EqualsEquals | NotEqual => 
                (Lvl::Comparison, ExprRole::None, ExprRole::Binary),
            And => (Lvl::And, ExprRole::None, ExprRole::Binary),
            Or => (Lvl::Or, ExprRole::None, ExprRole::Binary),
            _ => (Lvl::None, Role::None, Role::None),
        };
        TokenData::new(data.0, data.1, data.2)
    }
}
