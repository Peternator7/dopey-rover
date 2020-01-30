use dopey_rover::parsing::lexer;
use dopey_rover::parsing::parser;

fn main() {
    let program = r#"MyFunction a b = {
    True = new {};
    False = new {};
    Some = new {};
    None = new {};
    (1 + 1) :: "a" :: []
};"#;

    if let Ok((_, toks)) = lexer::tokenize(program) {
        // println!("{:?}", toks);
        println!(
            "{:?}",
            parser::statement::parse_statement(&*toks).unwrap().1 //parser::pattern::parse_top_level_pattern(&*toks)
        );
    }
}
