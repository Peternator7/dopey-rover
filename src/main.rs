use dopey_rover::parsing::lexer;
use dopey_rover::parsing::parser;

fn main() {
    let program = r#"Module.MyFunction MyVar.Y MyVar.X.Y.Z || return True"#;

    if let Ok((_, toks)) = lexer::tokenize(program) {
        println!("{:?}", toks);
        println!(
            "{:?}",
            parser::expression::parse_top_level_expression(&*toks)
        );
    }
}
