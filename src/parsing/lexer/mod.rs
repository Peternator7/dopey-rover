use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::character::complete::{alpha1, alphanumeric0, alphanumeric1, multispace0};
use nom::combinator::{map, not, recognize, value};
use nom::multi::many0;
use nom::sequence::{pair, preceded, terminated};
use nom::IResult;

use nom_locate::LocatedSpan;

type ParsedToken<'a> = IResult<LocatedSpan<&'a str>, Token<'a>>;

pub mod tokens;

pub use tokens::{Token, TokenType};

fn parse_float(s: LocatedSpan<&str>) -> ParsedToken {
    let (s, pos) = nom_locate::position(s)?;
    let (s, f) = nom::number::complete::float(s)?;
    Ok((s, Token::new(pos, TokenType::Number(f))))
}

fn parse_alphanumeric(s: LocatedSpan<&str>) -> ParsedToken {
    let (s, pos) = nom_locate::position(s)?;
    let (s, text) = recognize(pair(alpha1, alphanumeric0))(s)?;
    let tt = match text.fragment {
        "function" => TokenType::Function,
        "trait" => TokenType::Trait,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "import" => TokenType::Import,
        "from" => TokenType::From,
        "with" => TokenType::With,
        "new" => TokenType::New,
        "is" => TokenType::Is,
        "return" => TokenType::Return,
        "match" => TokenType::Match,
        "try" => TokenType::Try,
        "and" => TokenType::And,
        "or" => TokenType::Or,
        _ => TokenType::Ident(text.fragment),
    };

    Ok((s, Token::new(pos, tt)))
}

fn parse_string(s: LocatedSpan<&str>) -> ParsedToken {
    let (s, pos) = nom_locate::position(s)?;
    let (s, _) = tag("\"")(s)?;
    let (s, text) = recognize(take_while(|c| c != '"'))(s)?;
    let (s, _) = tag("\"")(s)?;
    Ok((s, Token::new(pos, TokenType::StringLiteral(text.fragment))))
}

fn parse_get_set(s: LocatedSpan<&str>) -> ParsedToken {
    let (s, pos) = nom_locate::position(s)?;
    let (s, _) = tag("@")(s)?;
    let (s, tok) = alt((
        value(TokenType::Get, tag("get")),
        value(TokenType::Set, tag("set")),
    ))(s)?;
    let (s, _) = not(alphanumeric1)(s)?;
    Ok((s, Token::new(pos, tok)))
}

fn parse_operators(s: LocatedSpan<&str>) -> ParsedToken {
    let (s, pos) = nom_locate::position(s)?;
    let (s, tok) = alt((
        map(tag("!="), move |_| TokenType::NotEquals),
        map(tag("=="), move |_| TokenType::EqualEquals),
        map(tag(">="), move |_| TokenType::GreaterThanOrEqualTo),
        map(tag("<="), move |_| TokenType::LessThanOrEqualTo),
        map(tag(">"), move |_| TokenType::GreaterThan),
        map(tag("<"), move |_| TokenType::LessThan),
        map(tag("+"), move |_| TokenType::Plus),
        map(tag("-"), move |_| TokenType::Minus),
        map(tag("/"), move |_| TokenType::Divide),
        map(tag("*"), move |_| TokenType::Multiply),
        map(tag("%"), move |_| TokenType::Modulus),
    ))(s)?;

    Ok((s, Token::new(pos, tok)))
}

fn parse_symbol(s: LocatedSpan<&str>) -> ParsedToken {
    let (s, pos) = nom_locate::position(s)?;
    let (s, tok) = alt((
        map(tag("?"), |_| TokenType::QuestionMark),
        map(tag("("), |_| TokenType::OpenParen),
        map(tag(")"), |_| TokenType::CloseParen),
        map(tag("{"), |_| TokenType::OpenBrace),
        map(tag("}"), |_| TokenType::CloseBrace),
        map(tag("["), |_| TokenType::OpenBracket),
        map(tag("]"), |_| TokenType::CloseBracket),
        map(tag("::"), |_| TokenType::DoubleColon),
        map(tag(":"), |_| TokenType::Colon),
        map(tag(";"), |_| TokenType::SemiColon),
        map(tag("."), |_| TokenType::Period),
        map(tag(","), |_| TokenType::Comma),
        map(tag("!"), |_| TokenType::Exclamation),
        map(tag("="), |_| TokenType::Equals),
        map(tag("|"), |_| TokenType::Pipe),
    ))(s)?;

    Ok((s, Token::new(pos, tok)))
}

/// Parse 1 or more whitespace characters. We would likely only ever parse this on the
/// very first token
// fn parse_whitespace(s: &str) -> ParsedToken {
//     map(space1, |output: &str| Token::Indented(output.len()))(s)
// }

// fn parse_newline(s: &str) -> ParsedToken {
//     map(preceded(line_ending, space0), |ws: &str| {
//         Token::Indented(ws.len())
//     })(s)
// }

fn parse_token(s: LocatedSpan<&str>) -> ParsedToken {
    alt((
        parse_operators,
        parse_symbol,
        parse_float,
        parse_alphanumeric,
        parse_get_set,
        parse_string
        // parse_newline,
        // parse_whitespace,
    ))(s)
}

pub fn tokenize<'a>(s: LocatedSpan<&'a str>) -> IResult<LocatedSpan<&'a str>, Vec<Token<'a>>> {
    preceded(multispace0, many0(terminated(parse_token, multispace0)))(s)
}
