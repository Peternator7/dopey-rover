use dopey_rover::parsing::lexer;
use dopey_rover::parsing::parser;

fn main() {
    let program = r#"MyFunction a b = {
        a = 1.0;
        try a;
        a
    };"#;

    if let Ok((_, toks)) = lexer::tokenize(program) {
        println!("{:?}", toks);
        println!(
            "{:?}",
            parser::statement::parse_statement(&*toks) //parser::pattern::parse_top_level_pattern(&*toks)
        );
    }
}
