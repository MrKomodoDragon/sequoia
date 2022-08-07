#![allow(dead_code)]
use crate::{ast::*, lexer::Token};
use chumsky::Stream;
use chumsky::{error::Simple, prelude::*};
use std::str::FromStr;

pub fn parse<'a>(
    tokens: Vec<(Token<'a>, std::ops::Range<usize>)>,
) -> (Option<Root>, Vec<Simple<Token<'_>>>){
    let span = (&(tokens.last().unwrap()).1).clone();
    let stream = Stream::from_iter(span, tokens.iter().cloned());
    let parser = Root::parser();
    let output = parser.parse_recovery_verbose(stream);
    output
}

impl Return {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::Return)
            .ignore_then(Expr::parser())
            .then_ignore(just(Token::Semicolon))
            .map(|expr| Return { expr }).labelled("Return")
    }
}
impl BinaryOperator {
    pub fn mul_parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::Multiply)
            .to(BinaryOperator::Mul)
            .or(just(Token::Divide).to(BinaryOperator::Div)).labelled("BinaryOperator, mul_parser")
    }
    pub fn add_parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::Plus)
            .to(BinaryOperator::Add)
            .or(just(Token::Subtract).to(BinaryOperator::Sub)).labelled("BinaryOperator, add_parser")
    }
}

impl ComparisonOperators {
    fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::GreaterThan)
            .to(ComparisonOperators::GreaterThan)
            .or(just(Token::LessThan).to(ComparisonOperators::LessThan))
            .or(just(Token::GreaterOrEqual).to(ComparisonOperators::GreaterOrEqualTo))
            .or(just(Token::LessOrEqual).to(ComparisonOperators::LessThanOrEqualTo)).labelled("ComparisonOperators")
    }
}

impl UnaryOperator {
    fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::Subtract).to(UnaryOperator::Neg).labelled("UnaryOperator")
    }
}
impl Literal {
    fn parser<'a>(
        expr: impl chumsky::Parser<Token<'a>, Expr, Error = Simple<Token<'a>>> + Clone,
    ) -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        filter_map(|span, token| match token {
            Token::Integer(int) => Ok(Literal::Integer(i64::from_str(int).unwrap())),
            Token::Float(float) => Ok(Literal::Float(f64::from_str(float).unwrap())),
            Token::Str(string) => Ok(Literal::Str(String::from(string))),
            Token::Boolean(bool) => Ok(Literal::Bool(bool::from_str(bool).unwrap())),
            _ => Err(Simple::expected_input_found(
                span,
                [
                    Some(Token::Integer("...")),
                    Some(Token::Float("...")),
                    Some(Token::Str("...")),
                    Some(Token::Boolean("...")),
                ],
                Some(token),
            )),
        })
        .or(expr
            .clone()
            .separated_by(just(Token::Comma))
            .delimited_by(just(Token::BracketOpen), just(Token::BracketClose))
            .map(|exprs| Literal::List(exprs)))
        .or(expr
            .clone()
            .separated_by(just(Token::Comma))
            .delimited_by(just(Token::ParenOpen), just(Token::ParenClose))
            .map(|exprs| Literal::Tuple(exprs))).labelled("Literal")
    }
}

impl Expr {
    fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> + Clone {
        recursive(|_expr| {
            let atom = Literal::parser(_expr)
                .map(Expr::Literal)
                .boxed().or(IdentAst::parser().map(Expr::Ident)).boxed();
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
            binary = binary
                .clone()
                .then(ComparisonOperators::parser().then(binary).repeated())
                .foldl(|left, (op, right)| {
                    Expr::ComparisonOperators(Box::new(left), op, Box::new(right))
                })
                .boxed();
            binary
        }).labelled("Expr")
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
            }).labelled("Let")
    }
}

impl IdentAst {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
       select! {
        Token::Ident(i) => IdentAst {name: i.to_string()}
       }.labelled("IdentAst")
    }
}

impl Kind {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        Kind::union_parser()
            .or(Kind::optional_parser())
            .or(Kind::list_parser())
            .or(Kind::basic_parser()).labelled("Kind::main_parser")
    }
    pub fn basic_parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        select! {
            Token::Type("Str") => Kind::Str,
            Token::Type("Int") => Kind::Int,
            Token::Type("Float") => Kind::Float,
            Token::Type("Bool") => Kind::Bool,
        }.labelled("Kind::basic_parser")
    }

    pub fn list_parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        Kind::basic_parser()
            .then_ignore(just([Token::BracketOpen, Token::BracketClose]))
            .map(|kind| Kind::List(Box::new(kind))).labelled("Kind::list_parser")
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
            }).labelled("Kind::unions_parser")
    }

    pub fn optional_parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>>
    {
        Kind::list_parser()
            .or(Kind::basic_parser())
            .then_ignore(just(Token::Optional))
            .map(|kind| Kind::Optional(Box::new(kind))).labelled("Kind::optional_parser")
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
                .or(While::parser(stmt.clone()).map(Statement::While))
                .or(If::parser(stmt.clone()).map(Statement::If))
        }).labelled("Statement")
    }
}

impl Root {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        Statement::parser(Expr::parser())
            .repeated()
            .then_ignore(end())
            .map(|stmts| Root { statements: stmts }).labelled("Root")
    }
}

impl Arg {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        IdentAst::parser()
            .then_ignore(just(Token::Colon))
            .then(Kind::parser())
            .map(|(ident, kind)| Arg { name: ident, kind }).labelled("Arg")
    }
}

impl FunctionDecl {
    pub fn parser<'a>(
        stmt: impl chumsky::Parser<Token<'a>, Statement, Error = Simple<Token<'a>>>,
    ) -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::Function)
            .ignore_then(IdentAst::parser())
            .then_ignore(just(Token::ParenOpen))
            .then(Arg::parser().then_ignore(just(Token::Comma)).repeated().or_not()).then(just([Token::Multiply, Token::Comma]).ignore_then(Arg::parser().separated_by(just(Token::Comma))).or_not())
            .then_ignore(just([
                Token::ParenClose,
                Token::Subtract,
                Token::GreaterThan,
            ]))
            .then(Kind::parser())
            .then_ignore(just(Token::BraceOpen))
            .then(stmt.repeated())
            .then_ignore(just(Token::BraceClose))
            .map(|((((name, args), kwargs), r_kind), stmts)| FunctionDecl {
                name,
                args,
                kwargs, 
                return_kind: r_kind,
                statements: stmts,
            }).labelled("FunctionDecl")
    }
}

impl FnCall {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        IdentAst::parser()
            .then_ignore(just(Token::ParenOpen))
            .then(Expr::parser().then_ignore(just(Token::Comma)).repeated().or_not())
            .then(Kwarg::parser().separated_by(just(Token::Comma)).or_not())
            .then_ignore(just(Token::ParenClose))
            .then_ignore(just(Token::Semicolon))
            .map(|((name, args), kwargs)| FnCall { name, args, kwargs }).labelled("FnCall")
    }
}

impl While {
    pub fn parser<'a>(
        stmt: impl chumsky::Parser<Token<'a>, Statement, Error = Simple<Token<'a>>>,
    ) -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just([Token::While])
            .ignore_then(Expr::parser()).then_ignore(just(Token::BraceOpen))
            .then(stmt.repeated()).then_ignore(just(Token::BraceClose))
            .map(|(cond, stmts)| While { cond, stmts }).labelled("While")
    }
}

impl Else {
    pub fn parser<'a>(stmt: impl chumsky::Parser<Token<'a>, Statement, Error = Simple<Token<'a>>>,) -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just([Token::Else])
            .ignore_then(Expr::parser()).then_ignore(just(Token::BraceOpen))
            .then(stmt.repeated()).then_ignore(just(Token::BraceClose))
            .map(|(cond, stmts)| Else { cond, stmts }).labelled("Else")
    }
}

impl ElsIf {
    pub fn parser<'a>(stmt: impl chumsky::Parser<Token<'a>, Statement, Error = Simple<Token<'a>>>,) -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just([Token::ElsIf])
            .ignore_then(Expr::parser()).then_ignore(just(Token::BraceOpen))
            .then(stmt.repeated()).then_ignore(just(Token::BraceClose))
            .map(|(cond, stmts)| ElsIf { cond, stmts }).labelled("ElsIf")
    }
}

impl If {
    pub fn parser<'a>(stmt: impl chumsky::Parser<Token<'a>, Statement, Error = Simple<Token<'a>>> + Clone,) -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::If).ignore_then(Expr::parser()).then_ignore(just(Token::BraceOpen)).then(stmt.clone().repeated()).then_ignore(just(Token::BraceClose)).then(ElsIf::parser(stmt.clone()).repeated().or_not()).then(Else::parser(stmt.clone()).or_not()).map(|(((cond, stmts), elsif), r#else)|{
            If { cond, stmts, elsif, r#else}
        }).labelled("If")
    }
}

impl Kwarg {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        IdentAst::parser().then_ignore(just(Token::Equal)).then(Expr::parser()).map(|(name, expr)| Kwarg { name, expr }).labelled("Kwarg")
    }
}

impl KwargName {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        filter_map(|span, token| {
            if let Token::Ident(ident) = token {
                Ok(KwargName {
                    name: ident.to_string(),
                })
            } else {
                Err(Simple::expected_input_found(
                    span,
                    [Some(Token::Ident("..."))],
                    Some(token),
                ))
            }
        }).labelled("KwargName")
    }
}