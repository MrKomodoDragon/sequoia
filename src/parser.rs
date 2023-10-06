#![allow(dead_code)]
#[allow(clippy::wildcard_imports)]
use crate::{ast::*, lexer::Token};
use chumsky::Stream;
use chumsky::{error::Simple, prelude::*};
use std::str::FromStr;

pub fn parse<'a>(
    tokens: Vec<(Token<'a>, std::ops::Range<usize>)>,
) -> Result<Spanned<Root>, Vec<Simple<Token<'a>>>> {
    let span = (&(tokens.last().unwrap()).1).clone();
    let stream = Stream::from_iter(span, tokens.iter().cloned());
    let parser = Root::parser();
    parser.parse(stream)
}
// impl Return {
//     pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Spanned<Self>, Error = Simple<Token<'a>>>
//     {
//         just(Token::Return)
//             .ignore_then(Expr::parser())
//             .then_ignore(just(Token::Semicolon))
//             .map_with_span(|expr, span| Spanned(Return { expr }, span))
//             .labelled("Return")
//     }
// }
impl BinaryOperator {
    pub fn mul_parser_or_modulo<'a>(
    ) -> impl chumsky::Parser<Token<'a>, Spanned<Self>, Error = Simple<Token<'a>>> {
        just(Token::Multiply)
            .to(BinaryOperator::Mul)
            .or(just(Token::Divide).to(BinaryOperator::Div))
            .or(just(Token::Modulus).to(BinaryOperator::Modulus))
            .map_with_span(|span, op| Spanned(span, op))
            .labelled("BinaryOperator, mul_parser")
    }
    pub fn add_parser<'a>(
    ) -> impl chumsky::Parser<Token<'a>, Spanned<Self>, Error = Simple<Token<'a>>> {
        just(Token::Plus)
            .to(BinaryOperator::Add)
            .or(just(Token::Subtract).to(BinaryOperator::Sub))
            .map_with_span(|span, op| Spanned(span, op))
            .labelled("BinaryOperator, add_parser")
    }

    pub fn and_or_parser<'a>(
    ) -> impl chumsky::Parser<Token<'a>, Spanned<Self>, Error = Simple<Token<'a>>> {
        just(Token::And)
            .to(BinaryOperator::AND)
            .or(just(Token::Or).to(BinaryOperator::OR))
            .map_with_span(|span, op| Spanned(span, op))
            .labelled("BinaryOperator, add_parser")
    }
}

impl ComparisonOperators {
    fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Spanned<Self>, Error = Simple<Token<'a>>> {
        just(Token::GreaterThan)
            .to(ComparisonOperators::GreaterThan)
            .or(just(Token::LessThan).to(ComparisonOperators::LessThan))
            .or(just(Token::GreaterOrEqual).to(ComparisonOperators::GreaterOrEqualTo))
            .or(just(Token::LessOrEqual).to(ComparisonOperators::LessThanOrEqualTo))
            .map_with_span(|span, op| Spanned(span, op))
            .labelled("ComparisonOperators")
    }
}
impl UnaryOperator {
    fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Spanned<Self>, Error = Simple<Token<'a>>> {
        just(Token::Not)
            .to(UnaryOperator::NOT)
            .or(just(Token::Subtract).to(UnaryOperator::Neg))
            .map_with_span(|span, op| Spanned(span, op))
            .labelled("UnaryOperator")
    }
}

impl Literal {
    fn parser<'a>(/* expr: impl chumsky::Parser<Token<'a>, Expr, Error = Simple<Token<'a>>> + Clone,*/
    ) -> impl chumsky::Parser<Token<'a>, Spanned<Self>, Error = Simple<Token<'a>>> {
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
        /*.or(expr
            .clone()
            .separated_by(just(Token::Comma))
            .delimited_by(just(Token::BracketOpen), just(Token::BracketClose))
            .map(|exprs| Literal::List(exprs)))*/
        /*.or(ArrayIndex::parser(expr.clone()).map(Literal::ArrrayIndex))*/
        .or(just(Token::None).to(Literal::None))
        .map_with_span(|span, literal| Spanned(span, literal))
        .labelled("Literal")
    }
}

fn unary_foldr(operator: Spanned<UnaryOperator>, expr: Spanned<Expr>) -> Spanned<Expr> {
    let span = (operator.1.start)..(expr.1.end);
    Spanned(Expr::UnaryOperator(operator, Box::new(expr)), span)
}

impl Expr {
    fn parser<'a>(
    ) -> impl chumsky::Parser<Token<'a>, Spanned<Self>, Error = Simple<Token<'a>>> + Clone {
        recursive(|_expr| {
            let atom = Literal::parser(/*expr.clone()*/)
                .map_with_span(|literal, span| Spanned(Expr::Literal(literal), span))
                .boxed();
            let unary = UnaryOperator::parser()
                .repeated()
                .then(atom)
                .foldr(unary_foldr);
            let bin_parsers = [
                BinaryOperator::mul_parser_or_modulo().boxed(),
                BinaryOperator::add_parser().boxed(),
                BinaryOperator::and_or_parser().boxed(),
            ];
            let mut binary = unary.boxed();
            binary = binary
                .clone()
                .then(ComparisonOperators::parser().then(binary).repeated())
                .foldl(|left, (op, right)| {
                    let span = (left.1.start)..(right.1.end);
                    Spanned(
                        Expr::ComparisonOperators(Box::new(left), op, Box::new(right)),
                        span,
                    )
                })
                .boxed();
            for parser in bin_parsers {
                binary = binary
                    .clone()
                    .then(parser.then(binary).repeated())
                    .foldl(|left, (op, right)| {
                        let span = (left.1.start)..(right.1.end);
                        Spanned(
                            Expr::BinaryOperator(Box::new(left), op, Box::new(right)),
                            span,
                        )
                    })
                    .boxed();
            }
            binary
        })
    }
}

impl Let {
    pub fn parser<'a>(
        expr: impl chumsky::Parser<Token<'a>, Spanned<Expr>, Error = Simple<Token<'a>>>,
    ) -> impl chumsky::Parser<Token<'a>, Spanned<Self>, Error = Simple<Token<'a>>> {
        just(Token::Let)
            .ignore_then(LetName::parser())
            .then(just(Token::MutableKeyword).or_not())
            .then_ignore(just(Token::Colon))
            .then(Kind::parser())
            .then(AssignOp::parser())
            .then(expr)
            .then_ignore(just(Token::Semicolon))
            .map(|((((name, mutable), kind), assign_op), rhs)| Let {
                name,
                mutable: mutable.is_some(),
                kind,
                assign_type: assign_op,
                rhs,
            })
            .map_with_span(|stmt, span| Spanned(stmt, span))
            .labelled("Let")
    }
}

impl IdentAst {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Spanned<Self>, Error = Simple<Token<'a>>>
    {
        select! {
         Token::Ident(i) => IdentAst {name: i.to_string()}
        }
        .map_with_span(|span, ident| Spanned(span, ident))
        .labelled("IdentAst")
    }
}

impl Kind {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Spanned<Self>, Error = Simple<Token<'a>>> {
        Kind::union_parser()
            .or(Kind::optional_parser())
            .or(Kind::list_parser())
            .or(Kind::basic_parser()).map_with_span(|span, tok| Spanned(span, tok))
            .labelled("Kind::main_parser")
    }
    pub fn basic_parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        select! {
            Token::Type("Str") => Kind::Str,
            Token::Type("Int") => Kind::Int,
            Token::Type("Float") => Kind::Float,
            Token::Type("Bool") => Kind::Bool,
            Token::Type("NoneType") => Kind::NoneType,
        }
        .labelled("Kind::basic_parser")
    }

    pub fn list_parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        Kind::basic_parser()
            .then(
                SeparateNumberParserBecauseIdkWhy::parser()
                    .or_not()
                    .delimited_by(just(Token::BracketOpen), just(Token::BracketClose))
                    .repeated(),
            )
            .foldl(|a, b| Kind::List {
                kind: Box::new(a),
                size: b,
            })
            .labelled("Kind::list_parser")
    }
    pub fn union_parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        Kind::optional_parser()
            .or(Kind::list_parser())
            .or(Kind::basic_parser())
            .separated_by(just(Token::Union))
            .at_least(2)
            .map(|kinds| Kind::Union(kinds.clone().iter().map(|kind| kind.clone()).collect()))
            .labelled("Kind::unions_parser")
    }

    pub fn optional_parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>>
    {
        Kind::list_parser()
            .or(Kind::basic_parser())
            .then_ignore(just(Token::Optional))
            .map(|kind| Kind::Optional(Box::new(kind)))
            .labelled("Kind::optional_parser")
    }
}

impl Statement {
    pub fn parser<'a>(
        expr: impl chumsky::Parser<Token<'a>, Spanned<Expr>, Error = Simple<Token<'a>>> + Clone + 'a,
    ) -> impl chumsky::Parser<Token<'a>, Spanned<Self>, Error = Simple<Token<'a>>> {
        recursive(|stmt| {
            /*ReAss::parser()
                .map(Statement::ReAssignment)
                .or(FunctionDecl::parser(stmt.clone()).map(Statement::FnDecl))
                .or(Let::parser(expr.clone()).map(Statement::Let))
                .or(Return::parser().map(Statement::Return))
                .or(FnCall::parser().map(Statement::FnCall))
                .or(While::parser(stmt.clone()).map(Statement::While))
                .or(If::parser(stmt.clone()).map(Statement::If))
                .or(Break::parser().map(Statement::Break))
                .or(Continue::parser().map(Statement::Continue))
                .or(ImportStmt::parser().map(Statement::Import))
                .or(Module::parser(stmt).map(Statement::Module))*/ Let::parser(expr).map(Statement::Let)
        }).map_with_span(|span, tok| Spanned(span, tok))
        .labelled("Statement")
    }
}

impl Root {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Spanned<Self>, Error = Simple<Token<'a>>> {
        Statement::parser(Expr::parser())
            .repeated()
            .then_ignore(end())
            .map(|stmts| Root { statements: stmts }).map_with_span(|span, tok| Spanned(span, tok))
            .labelled("Root")
    }
}
/*
impl Arg {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        IdentAst::parser()
            .then_ignore(just(Token::Colon))
            .then(Kind::parser())
            .map(|(ident, kind)| Arg { name: ident, kind })
            .labelled("Arg")
    }
}

impl FunctionDecl {
    pub fn parser<'a>(
        stmt: impl chumsky::Parser<Token<'a>, Statement, Error = Simple<Token<'a>>>,
    ) -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::Public)
            .or_not()
            .then(
                just(Token::Function)
                    .ignore_then(IdentAst::parser())
                    .then_ignore(just(Token::ParenOpen))
                    .then(Arg::parser().separated_by(just(Token::Comma)).or_not())
                    .then(
                        just([Token::Multiply, Token::Comma])
                            .ignore_then(Arg::parser().separated_by(just(Token::Comma)))
                            .or_not(),
                    )
                    .then_ignore(just([
                        Token::ParenClose,
                        Token::Subtract,
                        Token::GreaterThan,
                    ]))
                    .then(Kind::parser())
                    .then_ignore(just(Token::BraceOpen))
                    .then(stmt.repeated().or_not())
                    .then_ignore(just(Token::BraceClose)),
            )
            .map(
                |(public, ((((name, args), kwargs), r_kind), stmts))| FunctionDecl {
                    public: public.is_some(),
                    name,
                    args,
                    kwargs,
                    return_kind: r_kind,
                    statements: stmts,
                },
            )
            .labelled("FunctionDecl")
    }
}

impl FnCall {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        IdentAst::parser()
            .then(
                Kwarg::parser()
                    .separated_by(just(Token::Comma))
                    .delimited_by(just(Token::ParenOpen), just(Token::ParenClose))
                    .map(|kwargs| (vec![], kwargs))
                    .or(Expr::parser()
                        .separated_by(just(Token::Comma))
                        .delimited_by(just(Token::ParenOpen), just(Token::ParenClose))
                        .map(|args| (args, vec![])))
                    .or(Expr::parser()
                        .then_ignore(just(Token::Comma))
                        .repeated()
                        .then(Kwarg::parser().separated_by(just(Token::Comma)))
                        .delimited_by(just(Token::ParenOpen), just(Token::ParenClose))
                        .map(|(expr, kwargs)| (expr, kwargs))),
            )
            .then_ignore(just(Token::Semicolon))
            .map(|(name, (args, kwargs))| FnCall { name, args, kwargs })
            .labelled("FnCall")
    }
}

impl While {
    pub fn parser<'a>(
        stmt: impl chumsky::Parser<Token<'a>, Statement, Error = Simple<Token<'a>>>,
    ) -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just([Token::While])
            .ignore_then(Expr::parser())
            .then_ignore(just(Token::BraceOpen))
            .then(stmt.repeated())
            .then_ignore(just(Token::BraceClose))
            .map(|(cond, stmts)| While { cond, stmts })
            .labelled("While")
    }
}

impl Else {
    pub fn parser<'a>(
        stmt: impl chumsky::Parser<Token<'a>, Statement, Error = Simple<Token<'a>>>,
    ) -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just([Token::Else])
            .ignore_then(Expr::parser())
            .then_ignore(just(Token::BraceOpen))
            .then(stmt.repeated())
            .then_ignore(just(Token::BraceClose))
            .map(|(cond, stmts)| Else { cond, stmts })
            .labelled("Else")
    }
}

impl ElseIf {
    pub fn parser<'a>(
        stmt: impl chumsky::Parser<Token<'a>, Statement, Error = Simple<Token<'a>>>,
    ) -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just([Token::Else, Token::If])
            .ignore_then(Expr::parser())
            .then_ignore(just(Token::BraceOpen))
            .then(stmt.repeated())
            .then_ignore(just(Token::BraceClose))
            .map(|(cond, stmts)| ElseIf { cond, stmts })
            .labelled("ElseIf")
    }
}

impl If {
    pub fn parser<'a>(
        stmt: impl chumsky::Parser<Token<'a>, Statement, Error = Simple<Token<'a>>> + Clone,
    ) -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::If)
            .ignore_then(Expr::parser())
            .then_ignore(just(Token::BraceOpen))
            .then(stmt.clone().repeated())
            .then_ignore(just(Token::BraceClose))
            .then(ElseIf::parser(stmt.clone()).repeated().or_not())
            .then(Else::parser(stmt.clone()).or_not())
            .map(|(((cond, stmts), elsif), r#else)| If {
                cond,
                stmts,
                elsif,
                r#else,
            })
            .labelled("If")
    }
}

impl Kwarg {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        IdentAst::parser()
            .then_ignore(just(Token::Equal))
            .then(Expr::parser())
            .map(|(name, expr)| Kwarg { name, expr })
            .labelled("Kwarg")
    }
}
*/
impl LetName {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Spanned<Self>, Error = Simple<Token<'a>>> {
        filter_map(|span, token| {
            if let Token::Ident(ident) = token {
                Ok(LetName {
                    name: ident.to_string(),
                })
            } else {
                Err(Simple::expected_input_found(
                    span,
                    [Some(Token::Ident("..."))],
                    Some(token),
                ))
            }
        }).map_with_span(|span, tok| Spanned(span, tok))
        .labelled("LetName")
    }
}

impl AssignOp {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Spanned<Self>, Error = Simple<Token<'a>>> {
        just(Token::Equal)
            .to(AssignOp::Assign)
            .or(just([Token::Plus, Token::Equal]).to(AssignOp::Add))
            .or(just([Token::Subtract, Token::Equal]).to(AssignOp::Subtract))
            .or(just([Token::Multiply, Token::Equal]).to(AssignOp::Multiply))
            .or(just([Token::Divide, Token::Equal]).to(AssignOp::Divide)).map_with_span(|span, tok| Spanned(span, tok))
    }
}
/*
impl Break {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::Break).to(Break {})
    }
}

impl Continue {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::Continue).to(Continue {})
    }
}

impl Import {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        recursive(|import| {
            let head = IdentAst::parser()
                .then(just(Token::As).ignore_then(IdentAst::parser()).or_not())
                .map(|(name, alias)| Head::Single { name, alias })
                .or(import
                    .separated_by(just(Token::Comma))
                    .delimited_by(just(Token::BraceOpen), just(Token::BraceClose))
                    .map(Head::Many));
            IdentAst::parser()
                .then_ignore(just(Token::DoubleColon))
                .repeated()
                .then(head)
                .map(|(peths, head)| Import {
                    peth: peths
                        .iter()
                        .map(|a| a.clone().name)
                        .collect::<Vec<String>>()
                        .join("::"),
                    head: Box::new(head),
                })
        })
    }
}

impl ImportStmt {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::Import)
            .ignore_then(Import::parser())
            .then_ignore(just(Token::Semicolon))
            .map(|import| ImportStmt { import })
    }
}

impl Module {
    pub fn parser<'a>(
        stmt: impl chumsky::Parser<Token<'a>, Statement, Error = Simple<Token<'a>>> + 'a,
    ) -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        let ident = IdentAst::parser();
        let function_parser = FunctionDecl::parser(stmt).repeated().or_not();
        recursive(
            |module: Recursive<'_, Token<'_>, Self, Simple<Token<'_>>>| {
                just(Token::Mod)
                    .ignore_then(ident)
                    .then(
                        function_parser
                            .then(module.or_not())
                            .delimited_by(just(Token::BraceOpen), just(Token::BraceClose)),
                    )
                    .map(|(name, (functions, module))| {
                        let module_box = match module {
                            Some(i) => Some(Box::new(i)),
                            _ => None,
                        };
                        Module {
                            name,
                            modules: module_box,
                            functions,
                        }
                    })
            },
        )
    }
}

impl ReAss {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        StuffThatCanGoIntoReassignment::parser()
            .then(AssignOp::parser())
            .then(Expr::parser())
            .then_ignore(just(Token::Semicolon))
            .map(|((name, assignop), rhs)| ReAss {
                thing_to_be_reassigned: name,
                assignop,
                rhs,
            })
    }
}
*/
impl SeparateNumberParserBecauseIdkWhy {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Spanned<Self>, Error = Simple<Token<'a>>> {
        select! {
            Token::Integer(i) => SeparateNumberParserBecauseIdkWhy(i64::from_str(i).unwrap())
        }.map_with_span(|span, tok| Spanned(span, tok))
    }
}
/*
impl ArrayIndex {
    pub fn parser<'a>(
        expr: impl chumsky::Parser<Token<'a>, Expr, Error = Simple<Token<'a>>> + Clone,
    ) -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        IdentAst::parser()
            .then(expr.delimited_by(just(Token::BracketOpen), just(Token::BracketClose)))
            .map(|(name, index)| ArrayIndex {
                index: Box::new(index),
                arr_name: name,
            })
            .labelled("ArrayIndex")
    }
}

impl StuffThatCanGoIntoReassignment {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        ArrayIndex::parser(Expr::parser())
            .map(StuffThatCanGoIntoReassignment::ArrayIndex)
            .or(IdentAst::parser().map(StuffThatCanGoIntoReassignment::IdentAst))
    }
}
*/
