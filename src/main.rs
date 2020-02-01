use dopey_rover::parsing::lexer;
use dopey_rover::parsing::parser;

fn main() {
    let program = r#"
True = {};
Some = {
    Value = True,
    Map a b = a + b,
};

Functor = trait {
    Map fn a,
    Lift a
};

Options = ["Red", "Blue", "Yellow"];

MyFunction a b = {
    False = {};
    Some = {};
    None = {};
    (1 + 1) :: "a" :: []
};"#;

    if let Ok((_, toks)) = lexer::tokenize(program) {
        // println!("{:?}", toks);
        println!(
            "{:?}",
            parser::parse_program(&*toks).unwrap().1 //parser::pattern::parse_top_level_pattern(&*toks)
        );
    }
}
