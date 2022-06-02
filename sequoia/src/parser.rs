#[allow(dead_code)]
use crate::lexer::Token;
use chumsky::combinator::To;
use chumsky::prelude::*;
use chumsky::Stream;
use std::str::FromStr;

#[derive(Clone, Debug)]
pub enum Literal {
    Integer(i64),
    Str(String),
    Float(f64),
}
#[derive(Clone, Debug)]
pub enum BinaryOperator {
    Add,
    Neg,
    Sub,
    Mul,
    Div,
}
#[derive(Clone, Debug)]
pub enum UnaryOperator {
    Neg,
}
#[derive(Clone, Debug)]
pub enum Expr {
    Literal(Literal),
    BinaryOperator(Box<Expr>, BinaryOperator, Box<Expr>),
    UnaryOperator(UnaryOperator, Box<Expr>),
}
#[derive(Debug, Clone)]
enum Statement {
    Let {
        name: String,
        rhs: Expr,
    },

    Fn {
        name: String,
        args: Vec<String>,
        body: Expr,
    },
}

#[derive(Debug, Clone)]
enum Root {
    Statements(Vec<Statement>),
}
//As the name suggests, this is a TEST.
enum Test {
    Exprs(Vec<Expr>),
}
pub fn parse(
    tokens: Vec<(Token, std::ops::Range<usize>)>,
) -> Result<Expr, Vec<chumsky::error::Simple<Token>>> {
    let span = (&(tokens.last().unwrap()).1).clone();
    let stream = Stream::from_iter(span, tokens.iter().cloned());
    Expr::parser().parse(stream)
}

impl BinaryOperator {
    pub fn mul_parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::Multiply)
            .to(BinaryOperator::Mul)
            .or(just(Token::Divide).to(BinaryOperator::Div))
    }
}

impl UnaryOperator {
    fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::Subtract).to(UnaryOperator::Neg)
    }
}
impl Literal {
    fn parser<'a>(
        expr: impl chumsky::Parser<Token<'a>, Expr, Error = Simple<Token<'a>>>,
    ) -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        filter_map(|span, token| match token {
            Token::Integer(int) => Ok(Literal::Integer(i64::from_str(int).unwrap())),
            _ => Err(Simple::expected_input_found(
                span,
                [Some(Token::Integer("..."))],
                Some(token),
            )),
        })
    }
}

impl Expr {
    fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        recursive(|expr| {
            let mut final_parser = (); //just to shut rust up about variable not found
            let atom = Literal::parser(expr.clone()).map(Expr::Literal).boxed();
            let unary = UnaryOperator::parser()
                .repeated()
                .then(atom)
                .foldr(|op, expr| Expr::UnaryOperator(op, Box::new(expr)));
            let bin_parsers = [BinaryOperator::mul_parser().boxed()];
            let mut binary = unary.boxed();
            for parser in bin_parsers {
                binary = binary
                    .clone()
                    .then(parser.then(binary).repeated())
                    .foldl(|left, (op, right)| {
                        Expr::BinaryOperator(Box::new(left), op, Box::new(right))
                    })
                    .boxed();
            }
            binary
        })
    }
}
