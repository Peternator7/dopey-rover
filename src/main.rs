use dopey_rover::parsing::lexer;
use dopey_rover::parsing::parser;

fn main() {
    let program = r#"True - ( False + "random")"#;

    if let Ok((_, toks)) = lexer::tokenize(program) {
        println!("{:?}", toks);
        println!(
            "{:?}",
            parser::expression::parse_top_level_expression(&*toks)
        );
    }
}
