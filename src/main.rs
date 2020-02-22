use dopey_rover::parsing;
use serde_json::to_string;

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
        "{}",
        to_string(&parsing::parse_module("program", program).unwrap()).unwrap() //parser::pattern::parse_top_level_pattern(&*toks)
    );
}
