use dopey_rover::parsing::lexer;
use dopey_rover::parsing::parser;

fn main() {
    let program = r#"
True = {};
False = {};

DoubleYIfXIsPositive x y = {
    if x >= 0 {
        2 * y
    } else {
        y
    }
};

};"#;

    if let Ok((_, toks)) = lexer::tokenize(program) {
        // println!("{:?}", toks);
        println!(
            "{:?}",
            parser::parse_program(&*toks).unwrap().1 //parser::pattern::parse_top_level_pattern(&*toks)
        );
    }
}
