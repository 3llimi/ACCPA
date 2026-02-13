use std::io::Read;

use clap::Parser;

mod ast;
mod build;
mod eval;
mod stellalexer;
mod stellaparser;
mod stellaparserlistener;
mod typecheck;

#[derive(clap::Parser)]
struct Args {
    program: Option<std::path::PathBuf>,
}

/// Type alias for a [Result] with [ANTLRError] type as error.
/// The error type is boxed because it is relatively big
/// (check https://rust-lang.github.io/rust-clippy/master/index.html#result_large_err for more info).
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

    println!("test started");

    // Parse program
    let input_program = match &args.program {
        Some(path) => std::fs::read_to_string(path).expect("Failed to read from the file"),
        None => {
            // Read the program from stdin
            println!("Waiting for the program:");

            let mut program = String::new();
            std::io::stdin()
                .read_to_string(&mut program)
                .expect("IO Error");
            program
        }
    };

    println!("\nParsing the program...");
    let program = parse_program(&input_program).expect("Parse Error");

    typecheck::typecheck_program(&program).expect("Type Error");

    println!("\nProgram looks fine!");

    // Parse input expression
    let input_expr = match &args.program {
        None => return,
        Some(_) => {
            // Read the input for the program from stdin
            println!("Waiting for the input for the program:");
            let mut data = String::new();
            std::io::stdin()
                .read_to_string(&mut data)
                .expect("IO Error");
            data
        }
    };

    println!("\nParsing input expression...");
    let expr = parse_expr(&input_expr).expect("Parse Error");

    // typecheck::typecheck_expr(&program).expect("Type Error");

    // Evaluate the program with the given expression as input
    println!("\nEvaluating...");
    let _result = eval::evaluate_program(&program, &expr);

    println!("Done!");
}
