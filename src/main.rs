use dopey_rover::parsing::lexer;
use dopey_rover::parsing::parser;

fn main() {
    let program = r#"new { x: true, y: true - false} + true - false * false"#;

    if let Ok((_, toks)) = lexer::tokenize(program) {
        println!("{:?}", parser::parse_arithmatic_expression(&*toks));
    }
}
