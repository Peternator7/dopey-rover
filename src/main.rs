use dopey_rover::parsing;

fn main() {
    let program = r#"
True = {};
False = {};

DoubleYIfXIsPositive x y = {
    MyValue = {
        Test x = x + 1,
        Arr = (Test 3) :: [a,b,c]
    };
};
"#;

    println!(
        "{:?}",
        parsing::parse_module(program).unwrap() //parser::pattern::parse_top_level_pattern(&*toks)
    );
}
