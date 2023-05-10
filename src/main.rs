use std::env::args;

fn main() {
    if apla::LOG {
        std::env::set_var("RUST_BACKTRACE", "1");
    }
    let args = args().collect::<Vec<String>>();
    match args.len() {
        2 => apla::run_file(&args[1]),
        // 1 => apla::repl(),
        _ => {
            eprintln!("Usage: apla <file-name>");
            std::process::exit(1);
        }
    }
}
