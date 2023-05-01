pub const THIS_AS_STR: &str = "$this";

pub mod parser;
pub mod error;
pub mod compile;
pub mod ast_compiler;
pub mod optimize;
pub mod ast;
pub mod util;
pub mod resolver;
pub mod method_translator;
