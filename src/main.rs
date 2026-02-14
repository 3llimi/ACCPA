use std::io::Read;
use clap::Parser;

mod ast;
mod build;
mod error;
mod eval;
mod stellalexer;
mod stellaparser;
mod stellaparserlistener;
mod typecheck;

#[derive(clap::Parser)]
struct Args {
    program: Option<std::path::PathBuf>,
}

type AntlrResult<T> = Result<T, Box<antlr_rust::errors::ANTLRError>>;

type StellaParser<'a, T> = stellaparser::stellaParser<
    'a,
    antlr_rust::common_token_stream::CommonTokenStream<
        'a,
        stellalexer::stellaLexer<'a, antlr_rust::InputStream<T>>,
    >,
    antlr_rust::DefaultErrorStrategy<'a, stellaparser::stellaParserContextType>,
>;

fn create_parser(input: &str) -> StellaParser<&str> {
    let input_stream = antlr_rust::InputStream::new(input);
    let lexer = stellalexer::stellaLexer::new(input_stream);
    let token_stream = antlr_rust::common_token_stream::CommonTokenStream::new(lexer);
    stellaparser::stellaParser::new(token_stream)
}

fn parse_program(input: &str) -> AntlrResult<ast::Program> {
    let mut parser = create_parser(input);
    let program = parser.program()?;
    Ok(build::build_program(&program))
}

fn parse_expr(input: &str) -> AntlrResult<ast::Expr> {
    let mut parser = create_parser(input);
    let expr = parser.expr()?;
    Ok(build::build_expr(&expr))
}

fn main() {
    let args = Args::parse();

    // Parse program from stdin or file
    let input_program = match &args.program {
        Some(path) => std::fs::read_to_string(path).expect("Failed to read from the file"),
        None => {
            let mut program = String::new();
            std::io::stdin()
                .read_to_string(&mut program)
                .expect("IO Error");
            program
        }
    };

    // Parse the program
    let program = match parse_program(&input_program) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Parse Error: {:?}", e);
            std::process::exit(1);
        }
    };

    // Type check the program
    match typecheck::typecheck_program(&program) {
        Ok(()) => {
            // Success - exit with 0
            std::process::exit(0);
        }
        Err(type_error) => {
            // Print error to stderr and exit with non-zero
            eprintln!("{}", type_error);
            std::process::exit(1);
        }
    }
}