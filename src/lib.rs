pub const LOG: bool = false;

use std::collections::HashMap;

use apla_std::Std;
use class::Class;
use compile_time::{util::func::Func, ast::Ast};
use scanner::Scanner;

use crate::{
    compile_time::{parser::Parser, 
        ast_compiler::Compiler, resolver::Resolver}, 
    run_time::vm::VM
};

pub mod expr_type;
pub mod scanner;
pub mod compile_time;
pub mod token;
pub mod run_time;
pub mod spanned;
pub mod call;
pub mod class;
pub mod apla_std;
pub mod named_obj_container;

pub fn parse_file(
    input: &str, parsed_files: &[&str], parsing_file: &str,
) -> (Ast, bool, HashMap<String, Func>, HashMap<String, Class>, Vec<(String, String)>) 
{
    let scanner = Scanner::new(&input); 
    let mut tokens = crate::measure_time!(scanner.scan(), "scanning");
    for token in tokens.iter_mut() {
        token.file_id = (parsed_files.len().checked_sub(1).unwrap_or(0)).try_into().unwrap_or(0);
    }
    let parser = Parser::new(&tokens, &input, parsing_file, parsed_files);
    let (ast, had_error, functions, classes, files) = {
        let comp = crate::measure_time!(parser.parse(), "parsing");
        (comp.ast, comp.had_error, comp.functions, comp.classes, comp.new_parsed_files)
    };
    (ast, had_error, functions, classes, files)
}

pub fn run(input: String, parsed_files: &[&str], file_name: &str) -> Result<(), ()> {
    let apla_std = Std::new();
    let (mut ast, had_error, mut functions, classes, files) =
        parse_file(&input, parsed_files, file_name);
    if had_error {
        return Err(())
    }
    crate::measure_time!({
        // ast.optimize();
        for _code in functions.values_mut() {
            // code.node.optimize();
        }
    }, "optimizing");
    let callables = crate::measure_time!(
        match Resolver::resolve(
            &mut ast, &functions, classes, apla_std, files
        ) 
        {
            Ok(functions) => functions,
            Err(_) => {
                return Err(())
            },
        }, "resolving"
    );
    let (bytecode, callables) = {
        let compiler = Compiler::new(&ast, callables);
        crate::measure_time!(compiler.compile(), "compiling")
    };
    log(format!("generated code: {bytecode:?}"));
    let functions = callables.iter()
        .map(|(name, callable)| (name.to_string(), callable.as_ref()))
        .collect();
    let mut vm = VM::new(functions);
    if let Err(er) = crate::measure_time!(vm.execute(&bytecode), "executing") {
        println!("[RUNTIME ERROR] {er}");
    }
    Ok(())
}

pub fn repl() {
    loop {
        let mut buf = String::new();
        if let Err(er) = std::io::stdin().read_line(&mut buf) {
            eprintln!("Couldn't read input! {er}");
            std::process::exit(1);
        };
        _ = run(buf, &[], "");
    }
}

pub fn run_file(file_name: &str) {
    let content = match std::fs::read_to_string(file_name.trim()) {
        Ok(s) => s,
        Err(er) => {
            eprintln!("couldn't read the file '{file_name}': {er}");
            std::process::exit(1);
        }
    };
    _ = run(content, &[file_name], file_name);
}

fn log(str: String) {
    if LOG {
        println!("{str}");
    }
}

#[macro_export]
macro_rules! measure_time {
    ($func: expr, $name: expr) => {{      
        let start = std::time::Instant::now();
        let result = $func;
        if LOG {        
            log(format!("{} took {}ms", $name, std::time::Instant::now()
                .duration_since(start)
                .as_millis()));
        }

        result
    }};
}
