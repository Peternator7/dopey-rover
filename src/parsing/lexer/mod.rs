use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::character::complete::{alpha1, alphanumeric0, line_ending, multispace0, space0, space1};
use nom::combinator::{map, map_res, not, recognize};
use nom::multi::many0;
use nom::sequence::{pair, preceded, terminated, tuple};
use nom::IResult;

type ParsedToken<'a> = IResult<&'a str, Token>;

pub mod tokens;

pub use tokens::Token;

fn parse_float(s: &str) -> ParsedToken {
    map(nom::number::complete::float, Token::Number)(s)
}

fn parse_alphanumeric(s: &str) -> ParsedToken {
    map(recognize(pair(alpha1, alphanumeric0)), |s| match s {
        "function" => Token::Function,
        "interface" => Token::Interface,
        "if" => Token::If,
        "else" => Token::Else,
        "import" => Token::Import,
        "from" => Token::From,
        "with" => Token::With,
        "new" => Token::New,
        "is" => Token::Is,
        "return" => Token::Return,
        _ => Token::Ident(s.to_string()),
    })(s)
}

fn parse_string(s: &str) -> ParsedToken {
    map(
        tuple((tag("\""), recognize(take_while(|c| c != '"')), tag("\""))),
        |(_, s, _): (&str, &str, &str)| Token::StringLiteral(s.to_string()),
    )(s)
}

fn parse_get_set(s: &str) -> ParsedToken {
    map_res(recognize(pair(tag("@"), alphanumeric0)), |s| match s {
        "@get" => Ok(Token::Get),
        "@set" => Ok(Token::Set),
        _ => Result::Err("Invalid Token"),
    })(s)
}

fn parse_operators(s: &str) -> ParsedToken {
    alt((
        map(tag("??"), |_| Token::Coalesce),
        map(tag("&&"), |_| Token::AndAnd),
        map(tag("||"), |_| Token::OrOr),
        map(tag("!="), |_| Token::NotEquals),
        map(tag("=="), |_| Token::EqualEquals),
        map(tag(">="), |_| Token::GreaterThanOrEqualTo),
        map(tag("<="), |_| Token::LessThanOrEqualTo),
        map(tag(">"), |_| Token::GreaterThan),
        map(tag("<"), |_| Token::LessThan),
        map(tag("+"), |_| Token::Plus),
        map(tag("-"), |_| Token::Minus),
        map(tag("/"), |_| Token::Divide),
        map(tag("*"), |_| Token::Multiply),
        map(tag("%"), |_| Token::Modulus),
    ))(s)
}

fn parse_symbol(s: &str) -> ParsedToken {
    alt((
        parse_operators,
        map(tag("("), |_| Token::OpenParen),
        map(tag(")"), |_| Token::CloseParen),
        map(tag("{"), |_| Token::OpenBrace),
        map(tag("}"), |_| Token::CloseBrace),
        map(tag("["), |_| Token::OpenBracket),
        map(tag("]"), |_| Token::CloseBracket),
        map(tag(":"), |_| Token::Colon),
        map(tag("."), |_| Token::Period),
        map(tag(","), |_| Token::Comma),
        map(tag("!"), |_| Token::Exclamation),
        map(tag("="), |_| Token::Equals),
        map(tag("|"), |_| Token::Pipe),
    ))(s)
}

/// Parse 1 or more whitespace characters. We would likely only ever parse this on the
/// very first token
fn parse_whitespace(s: &str) -> ParsedToken {
    map(space1, |output: &str| Token::Indented(output.len()))(s)
}

fn parse_newline(s: &str) -> ParsedToken {
    map(preceded(line_ending, space0), |ws: &str| {
        Token::Indented(ws.len())
    })(s)
}

fn parse_token(s: &str) -> ParsedToken {
    alt((
        parse_symbol,
        parse_float,
        parse_alphanumeric,
        parse_get_set,
        parse_string
        // parse_newline,
        // parse_whitespace,
    ))(s)
}

pub fn tokenize(s: &str) -> IResult<&str, Vec<Token>> {
    many0(terminated(parse_token, multispace0))(s)
}
