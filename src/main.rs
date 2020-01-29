use dopey_rover::parsing::lexer;
use dopey_rover::parsing::parser;

fn main() {
    let program = r#"[a, b, c d]"#;

    if let Ok((_, toks)) = lexer::tokenize(program) {
        println!("{:?}", toks);
        println!(
            "{:?}",
            parser::expression::parse_top_level_expression(&*toks) //parser::pattern::parse_top_level_pattern(&*toks)
        );
    }
}
