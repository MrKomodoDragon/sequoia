#![allow(dead_code)]
use crate::lexer::Token;
use chumsky::prelude::*;
use chumsky::Stream;
use std::str::FromStr;

#[derive(Debug, Clone)]
pub struct Ident(String);

#[derive(Debug, Clone)]
pub struct Let {
    name: Ident,
    kind: Kind, //if you're wondering why it's not named something with the word type in it, blame sampersand.
    rhs: Expr,
}
#[derive(Clone, Debug)]
pub enum Literal {
    Integer(i64),
    Str(String),
    Float(f64),
}

#[derive(Debug, Clone)]
pub enum Kind {
    Int,
    Float,
    Str,
    List(Box<Kind>),
    Union(Box<Kind>),
    Optional(Box<Kind>),
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
    Let(Let),
}

#[derive(Debug, Clone)]
pub struct Root {
    statements: Vec<Statement>,
}
//As the name suggests, this is a TEST.
enum Test {
    Exprs(Vec<Expr>),
}
pub fn parse(
    tokens: Vec<(Token, std::ops::Range<usize>)>,
) -> Result<Root, Vec<chumsky::error::Simple<Token>>> {
    let span = (&(tokens.last().unwrap()).1).clone();
    let stream = Stream::from_iter(span, tokens.iter().cloned());
    Root::parser().parse(stream)
}

impl BinaryOperator {
    pub fn mul_parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::Multiply)
            .to(BinaryOperator::Mul)
            .or(just(Token::Divide).to(BinaryOperator::Div))
    }
    pub fn add_parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::Plus)
            .to(BinaryOperator::Add)
            .or(just(Token::Subtract).to(BinaryOperator::Sub))
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
    fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> + Clone {
        recursive(|expr| {
            let atom = Literal::parser(expr.clone()).map(Expr::Literal).boxed();
            let unary = UnaryOperator::parser()
                .repeated()
                .then(atom)
                .foldr(|op, expr| Expr::UnaryOperator(op, Box::new(expr)));
            let bin_parsers = [
                BinaryOperator::mul_parser().boxed(),
                BinaryOperator::add_parser().boxed(),
            ];
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

impl Let {
    pub fn parser<'a>(
        expr: impl chumsky::Parser<Token<'a>, Expr, Error = Simple<Token<'a>>>,
    ) -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::Let)
            .ignore_then(Ident::parser())
            .then_ignore(just(Token::Colon))
            .then(Kind::parser())
            .then_ignore(just(Token::Equal))
            .then(expr)
            .then_ignore(just(Token::Semicolon))
            .map(|((name, kind), value)| Let {
                name,
                kind,
                rhs: value,
            })
    }
}

impl Ident {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        filter_map(|span, token| {
            if let Token::Ident(ident) = token {
                Ok(Ident(ident.to_string()))
            } else {
                Err(Simple::expected_input_found(
                    span,
                    [Some(Token::Ident("..."))],
                    Some(token),
                ))
            }
        })
    }
}

impl Kind {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        filter_map(|span, token| match token {
            Token::Type("String") => Ok(Kind::Str),
            Token::Type("Int") => Ok(Kind::Int),
            Token::Type("Float") => Ok(Kind::Float),
            _ => Err(Simple::expected_input_found(
                span,
                [
                    Some(Token::Type("String")),
                    Some(Token::Type("Int")),
                    Some(Token::Type("Float")),
                ],
                Some(token),
            )),
        })
    }
}

impl Statement {
    pub fn parser<'a>(
        expr: impl chumsky::Parser<Token<'a>, Expr, Error = Simple<Token<'a>>> + Clone,
    ) -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        Let::parser(expr.clone()).map(Statement::Let)
    }
}

impl Root {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        Statement::parser(Expr::parser())
            .repeated()
            .then_ignore(end())
            .map(|stmts| Root { statements: stmts })
    }
}
