#![allow(dead_code)]
use crate::{ast::*, lexer::Token};
use chumsky::Stream;
use chumsky::{error::Simple, prelude::*};
use std::str::FromStr;

pub fn parse<'a>(
    tokens: Vec<(Token<'a>, std::ops::Range<usize>)>,
) -> Result<Root, Vec<Simple<Token<'a>>>> {
    let span = (&(tokens.last().unwrap()).1).clone();
    let stream = Stream::from_iter(span, tokens.iter().cloned());
    Root::parser().parse(stream)
}

impl Return {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::Return)
            .ignore_then(Expr::parser()).then_ignore(just(Token::Semicolon))
            .map(|expr| Return { expr })
    }
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

impl ComparisonOperators {
    fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::GreaterThan)
            .to(ComparisonOperators::GreaterThan)
            .or(just(Token::LessThan).to(ComparisonOperators::LessThan))
            .or(just(Token::GreaterOrEqual).to(ComparisonOperators::GreaterOrEqualTo))
            .or(just(Token::LessOrEqual).to(ComparisonOperators::LessThanOrEqualTo))
    }
}

impl UnaryOperator {
    fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::Subtract).to(UnaryOperator::Neg)
    }
}
impl Literal {
    fn parser<'a>(
        
    ) -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        filter_map(|span, token| match token {
            Token::Integer(int) => Ok(Literal::Integer(i64::from_str(int).unwrap())),
            Token::Float(float) => Ok(Literal::Float(f64::from_str(float).unwrap())),
            Token::Str(string) => Ok(Literal::Str(String::from(string))),
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
        recursive(|_expr| {
            let atom = Literal::parser()
                .map(Expr::Literal)
                .boxed()
                .or(IdentAst::parser().map(Expr::Ident).boxed());
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
                    .foldl(|left, (op, right)| {Expr::BinaryOperator(Box::new(left), op, Box::new(right))})
                    .boxed();
            }
            binary = binary
                .clone()
                .then(ComparisonOperators::parser().then(binary).repeated())
                .foldl(|left, (op, right)| {
                    Expr::ComparisonOperators(Box::new(left), op, Box::new(right))
                }).boxed();
            binary
        })
    }
}

impl Let {
    pub fn parser<'a>(
        expr: impl chumsky::Parser<Token<'a>, Expr, Error = Simple<Token<'a>>>,
    ) -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::Let)
            .ignore_then(IdentAst::parser())
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

impl IdentAst {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        filter_map(|span, token| {
            if let Token::Ident(ident) = token {
                Ok(IdentAst {
                    name: ident.to_string(),
                })
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
        Kind::union_parser()
            .or(Kind::optional_parser())
            .or(Kind::list_parser())
            .or(Kind::basic_parser())
    }
    pub fn basic_parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        select! {
            Token::Type("Str") => Kind::Str,
            Token::Type("Int") => Kind::Int,
            Token::Type("Float") => Kind::Float,
            Token::Type("Bool") => Kind::Bool,
        }
    }

    pub fn list_parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        Kind::basic_parser()
            .then_ignore(just([Token::BracketOpen, Token::BracketClose]))
            .map(|kind| Kind::List(Box::new(kind)))
    }
    pub fn union_parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        Kind::optional_parser()
            .or(Kind::list_parser())
            .or(Kind::basic_parser())
            .separated_by(just(Token::Union))
            .at_least(2)
            .map(|kinds| {
                Kind::Union(
                    kinds
                        .clone()
                        .iter()
                        .map(|kind| Box::new(kind.clone()))
                        .collect(),
                )
            })
    }

    pub fn optional_parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>>
    {
        Kind::list_parser()
            .or(Kind::basic_parser())
            .then_ignore(just(Token::Optional))
            .map(|kind| Kind::Optional(Box::new(kind)))
    }
}

impl Statement {
    pub fn parser<'a>(
        expr: impl chumsky::Parser<Token<'a>, Expr, Error = Simple<Token<'a>>> + Clone + 'a,
    ) -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        recursive(|stmt| {
            FunctionDecl::parser(stmt.clone())
                .map(Statement::FnDecl)
                .or(Let::parser(expr.clone()).map(Statement::Let))
                .or(Return::parser().map(Statement::Return))
                .or(FnCall::parser().map(Statement::FnCall))
        })
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

impl Arg {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        IdentAst::parser()
            .then_ignore(just(Token::Colon))
            .then(Kind::parser())
            .map(|(ident, kind)| Arg { name: ident, kind })
    }
}

impl FunctionDecl {
    pub fn parser<'a>(
        stmt: impl chumsky::Parser<Token<'a>, Statement, Error = Simple<Token<'a>>>,
    ) -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::Function)
            .ignore_then(IdentAst::parser())
            .then_ignore(just(Token::ParenOpen))
            .then(Arg::parser().separated_by(just(Token::Comma)))
            .then_ignore(just([
                Token::ParenClose,
                Token::Subtract,
                Token::GreaterThan,
            ]))
            .then(Kind::parser())
            .then_ignore(just(Token::BraceOpen))
            .then(stmt.repeated())
            .then_ignore(just(Token::BraceClose))
            .map(|(((name, args), r_kind), stmts)| FunctionDecl {
                name,
                args,
                return_kind: r_kind,
                statements: stmts,
            })
    }
}

impl FnCall {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        IdentAst::parser()
            .then_ignore(just(Token::ParenOpen))
            .then(Expr::parser().separated_by(just(Token::Comma)))
            .then_ignore(just(Token::ParenClose)).then_ignore(just(Token::Semicolon))
            .map(|(name, args)| FnCall { name, args })
    }
}
