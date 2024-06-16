// https://google.github.io/comprehensive-rust/error-handling/exercise.html
/*
HINT: start by fixing error handling in the parse function.
Once that is working correctly, update Tokenizer to implement Iterator<Item=Result<Token, TokenizerError>> and handle that in the parser.
 */
use std::iter::Peekable;
use std::str::Chars;
use thiserror::Error;

/// An arithmetic operator.
#[derive(Debug, PartialEq, Clone, Copy)]
enum Op {
    Add,
    Sub,
}

/// A token in the expression language.
#[derive(Debug, PartialEq)]
enum Token {
    Number(String),
    Identifier(String),
    Operator(Op),
}

/// An expression in the expression language.
#[derive(Debug, PartialEq)]
enum Expression {
    /// A reference to a variable.
    Var(String),
    /// A literal number.
    Number(u32),
    /// A binary operation.
    Operation(Box<Expression>, Op, Box<Expression>),
}

fn tokenize(input: &str) -> Tokenizer {
    return Tokenizer(input.chars().peekable());
}
#[derive(Error, Debug)]
enum TokenizerError {
    #[error("Unexpected character {0}")]
    UnexpectedCharacter(char),
}

struct Tokenizer<'a>(Peekable<Chars<'a>>);

impl<'a> Tokenizer<'a> {
    fn collect_number(&mut self, first_char: char) -> Token {
        let mut num = String::from(first_char);
        while let Some(&c @ '0'..='9') = self.0.peek() {
            num.push(c);
            self.0.next();
        }
        Token::Number(num)
    }

    fn collect_identifier(&mut self, first_char: char) -> Token {
        let mut ident = String::from(first_char);
        while let Some(&c @ ('a'..='z' | '_' | '0'..='9')) = self.0.peek() {
            ident.push(c);
            self.0.next();
        }
        Token::Identifier(ident)
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token, TokenizerError>;

    fn next(&mut self) -> Option<Result<Token, TokenizerError>> {
        let c = self.0.next()?;
        match c {
            '0'..='9' => Some(Ok(self.collect_number(c))),
            'a'..='z' => Some(Ok(self.collect_identifier(c))),
            '+' => Some(Ok(Token::Operator(Op::Add))),
            '-' => Some(Ok(Token::Operator(Op::Sub))),
            _ => Some(Err(TokenizerError::UnexpectedCharacter(c))),
        }
    }
}

#[derive(Error, Debug)]
enum ParseError {
    #[error("Tokenizer error: {0}")]
    TokenizerError(#[from] TokenizerError),
    #[error("Unexpected end of input")]
    EOF,
    #[error("Invalid 32-bit integer'")]
    InvalidNumber(#[from] std::num::ParseIntError),
    #[error("Unexpected token {0:?}")]
    UnexpectedToken(Token),
}

fn parse(input: &str) -> Result<Expression, ParseError> {
    let mut tokens = tokenize(input);

    fn parse_expr<'a>(tokens: &mut Tokenizer<'a>) -> Result<Expression, ParseError> {
        let tok = tokens.next().ok_or(ParseError::EOF)??;
        let expr = match tok {
            Token::Number(num) => {
                //let v = num.parse().expect("Invalid 32-bit integer'");
                let v = num.parse()?;
                Expression::Number(v)
            }
            Token::Identifier(ident) => Expression::Var(ident),
            Token::Operator(_) => return Err(ParseError::UnexpectedToken(tok)),
        };
        // Look ahead to parse a binary operation if present.
        Ok(match tokens.next() {
            None => expr,
            Some(Ok(Token::Operator(op))) => {
                let exp = parse_expr(tokens)?;
                Expression::Operation(Box::new(expr), op, Box::new(exp))
            }
            Some(Err(e)) => return Err(e.into()),
            Some(Ok(tok)) => return Err(ParseError::UnexpectedToken(tok)),
        })
    }

    parse_expr(&mut tokens)
}

fn main() {
    // official solution uses anyhow here
    if let Ok(expr) = parse("10+foo+20-30") {
        println!("{expr:?}");
    }
}
