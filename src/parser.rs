#![allow(dead_code)]
use crate::lexer;
#[allow(clippy::wildcard_imports)]
use crate::{ast::*, lexer::Token};
use chumsky::Parser;
use chumsky::input::{Stream, ValueInput};
use chumsky::span::SimpleSpan;
use chumsky::{error::Simple, prelude::*};
use std::str::FromStr;

pub fn parse<'a>(
    tokens: Vec<(Token<'a>, SimpleSpan)>,
) -> ParseResult<Spanned<Root>, Rich<'a, Token<'a>>> {
    let span = (&(tokens.last().unwrap()).1).clone();
    let stream = Stream::from_iter(tokens).map((0..span.end).into(), |(t, s): (_, _)| (t, s));
    let parser = Root::parser();
    parser.parse(stream)
}

// impl Return {
//     pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Spanned<Self>, Error = Simple<Token<'a>>>
//     {
//         just(Token::Return)
//             .ignore_then(Expr::parser())
//             .then_ignore(just(Token::Semicolon))
//             .map_with(|expr, span| Spanned(Return { expr }, span))
//             .labelled("Return")
//     }
// }

impl BinaryOperator {
    pub fn exponent_parser<'a, I>()
    -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        just(Token::Pow)
            .to(BinaryOperator::Pow)
            .map_with(|span, op| Spanned(span, op.span()))
    }
    pub fn mul_parser_or_modulo<'a, I>()
    -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        just(Token::Multiply)
            .to(BinaryOperator::Mul)
            .or(just(Token::Divide).to(BinaryOperator::Div))
            .or(just(Token::Modulus).to(BinaryOperator::Modulus))
            .map_with(|span, op| Spanned(span, op.span()))
            .labelled("BinaryOperator, mul_parser")
    }
    pub fn add_parser<'a, I>() -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        just(Token::Plus)
            .to(BinaryOperator::Add)
            .or(just(Token::Subtract).to(BinaryOperator::Sub))
            .map_with(|span, op| Spanned(span, op.span()))
            .labelled("BinaryOperator, add_parser")
    }

    pub fn and_or_parser<'a, I>()
    -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        just(Token::And)
            .to(BinaryOperator::AND)
            .or(just(Token::Or).to(BinaryOperator::OR))
            .map_with(|span, op| Spanned(span, op.span()))
            .labelled("BinaryOperator, add_parser")
    }
}

impl ComparisonOperators {
    fn parser<'a, I>() -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        just(Token::GreaterThan)
            .to(ComparisonOperators::GreaterThan)
            .or(just(Token::LessThan).to(ComparisonOperators::LessThan))
            .or(just(Token::GreaterOrEqual).to(ComparisonOperators::GreaterOrEqualTo))
            .or(just(Token::LessOrEqual).to(ComparisonOperators::LessThanOrEqualTo))
            .map_with(|span, op| Spanned(span, op.span()))
            .labelled("ComparisonOperators")
    }
}
impl UnaryOperator {
    fn parser<'a, I>() -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        just(Token::Not)
            .to(UnaryOperator::NOT)
            .or(just(Token::Subtract).to(UnaryOperator::Neg))
            .map_with(|span, op| Spanned(span, op.span()))
            .labelled("UnaryOperator")
    }
}

impl Literal {
    fn parser<'a, I>(
        expr: impl Parser<'a, I, Spanned<Expr>, extra::Err<Rich<'a, Token<'a>>>> + Clone + 'a,
    ) -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        select! {
        Token::Integer(int) => Literal::Integer(i64::from_str(int).unwrap()),
        Token::Float(int) => Literal::Float(f64::from_str(int).unwrap()),
        Token::Str(string) => Literal::Str(String::from(string)),
        Token::Boolean(bool) => Literal::Bool(bool::from_str(bool).unwrap()),
                }
        .or(expr
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<Spanned<Expr>>>()
            .map(|exprs| Literal::List(exprs))
            .delimited_by(just(Token::BracketOpen), just(Token::BracketClose)))
        .or(ArrayIndex::parser(expr.clone()).map(Literal::ArrrayIndex))
        //.or(just(Token::None).to(Literal::None))
        .map_with(|span, literal| Spanned(span, literal.span()))
        .labelled("Literal")
    }
}

fn unary_foldr(operator: Spanned<UnaryOperator>, expr: Spanned<Expr>) -> Spanned<Expr> {
    let span = (operator.1.start)..(expr.1.end);
    Spanned(
        Expr::UnaryOperator(operator, Box::new(expr)),
        SimpleSpan::from(span),
    )
}

impl Expr {
    fn parser<'a, I>() -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>> + Clone
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        recursive(|expr| {
            let literal = Literal::parser(expr.clone())
                .map_with(|literal, span| Spanned(Expr::Literal(literal), span.span()));
            let ident =
                IdentAst::parser().map_with(|ident, span| Spanned(Expr::Ident(ident), span.span()));
            let atom = literal
                .or(ident)
                .or(expr.delimited_by(just(Token::ParenOpen), just(Token::ParenClose)))
                .boxed();
            let unary = UnaryOperator::parser().repeated().foldr(atom, unary_foldr);
            let bin_parsers = [
                BinaryOperator::exponent_parser().boxed(),
                BinaryOperator::mul_parser_or_modulo().boxed(),
                BinaryOperator::add_parser().boxed(),
                BinaryOperator::and_or_parser().boxed(),
            ];
            let mut binary = unary.boxed();
            binary = binary
                .clone()
                .foldl(
                    ComparisonOperators::parser().then(binary).repeated(),
                    |left: Spanned<Expr>, (op, right)| {
                        let span = (left.1.start)..(right.1.end);
                        Spanned(
                            Expr::ComparisonOperators(Box::new(left), op, Box::new(right)),
                            span.into(),
                        )
                    },
                )
                .boxed();
            for parser in bin_parsers {
                binary = binary
                    .clone()
                    .foldl(parser.then(binary).repeated(), |left, (op, right)| {
                        let span = (left.1.start)..(right.1.end);
                        Spanned(
                            Expr::BinaryOperator(Box::new(left), op, Box::new(right)),
                            span.into(),
                        )
                    })
                    .boxed();
            }
            binary
        })
    }
}

impl Let {
    pub fn parser<'a, I>(
        expr: impl Parser<'a, I, Spanned<Expr>, extra::Err<Rich<'a, Token<'a>>>> + Clone + 'a,
    ) -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>> + Clone
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
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
            .map_with(|stmt, span| Spanned(stmt, span.span()))
            .labelled("Let")
    }
}

impl IdentAst {
    pub fn parser<'a, I>() -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        select! {
         Token::Ident(i) => IdentAst {name: i.to_string()}
        }
        .map_with(|span, ident| Spanned(span, ident.span()))
        .labelled("IdentAst")
    }
}

impl Kind {
    pub fn parser<'a, I>()
    -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>> + Clone
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        Kind::union_parser()
            .or(Kind::optional_parser())
            .or(Kind::list_parser())
            .or(Kind::basic_parser())
            .map_with(|span, tok| Spanned(span.0, tok.span()))
            .labelled("Kind::main_parser")
    }
    pub fn basic_parser<'a, I>()
    -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>> + Clone
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        select! {
            Token::Type("Str") => Kind::Str,
            Token::Type("Int") => Kind::Int,
            Token::Type("Float") => Kind::Float,
            Token::Type("Bool") => Kind::Bool,
            Token::Type("NoneType") => Kind::NoneType,
        }
        .map_with(|span, tok| Spanned(span, tok.span()))
        .labelled("Kind::basic_parser")
    }

    pub fn list_parser<'a, I>()
    -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>> + Clone
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        Kind::basic_parser()
            .foldl(
                SeparateNumberParserBecauseIdkWhy::parser()
                    .or_not()
                    .delimited_by(just(Token::BracketOpen), just(Token::BracketClose))
                    .repeated(),
                |a, b| {
                    let span = match b.clone() {
                        Some(a) => (a.1.start)..(a.1.end + 3),
                        None => (a.1.start)..(a.1.end + 2),
                    };
                    Spanned(
                        Kind::List {
                            kind: Box::new(a),
                            size: b,
                        },
                        SimpleSpan::from(span),
                    )
                },
            )
            .map_with(|span, tok| Spanned(span.0, tok.span()))
            .labelled("Kind::list_parser")
    }
    pub fn union_parser<'a, I>()
    -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>> + Clone
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        Kind::optional_parser()
            .or(Kind::list_parser())
            .or(Kind::basic_parser())
            .separated_by(just(Token::Union))
            .at_least(2)
            .collect::<Vec<Spanned<Kind>>>()
            .map_with(|kinds, tok| {
                Spanned(
                    Kind::Union(kinds.clone().iter().map(|kind| kind.clone()).collect()),
                    tok.span(),
                )
            })
            .labelled("Kind::unions_parser")
    }

    pub fn optional_parser<'a, I>()
    -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>> + Clone
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        Kind::list_parser()
            .or(Kind::basic_parser())
            .then_ignore(just(Token::Optional))
            .map_with(|kind, tok| Spanned(Kind::Optional(Box::new(kind)), tok.span()))
            .labelled("Kind::optional_parser")
    }
}

impl Statement {
    pub fn parser<'a, I>(
        expr: impl Parser<'a, I, Spanned<Expr>, extra::Err<Rich<'a, Token<'a>>>> + Clone + 'a,
    ) -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
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
            .or(Module::parser(stmt).map(Statement::Module))*/
            Let::parser(expr).map(Statement::Let)
        })
        .map_with(|span, tok| Spanned(span, tok.span()))
        .labelled("Statement")
    }
}

impl Root {
    pub fn parser<'a, I>() -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        Statement::parser(Expr::parser())
            .repeated()
            .collect::<Vec<Spanned<Statement>>>()
            .then_ignore(end())
            .map(|stmts| Root { statements: stmts })
            .map_with(|span, tok| Spanned(span, tok.span()))
            .labelled("Root")
    }
}
/*
impl Arg {
    pub fn parser<'a>() -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>, {
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
    ) -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>, {
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
    pub fn parser<'a>() -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>, {
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
    ) -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>, {
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
    ) -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>, {
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
    ) -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>, {
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
    ) -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>, {
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
    pub fn parser<'a>() -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>, {
        IdentAst::parser()
            .then_ignore(just(Token::Equal))
            .then(Expr::parser())
            .map(|(name, expr)| Kwarg { name, expr })
            .labelled("Kwarg")
    }
}
*/
impl LetName {
    pub fn parser<'a, I>()
    -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>> + Clone
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        select! {
         Token::Ident(i) => LetName {name: i.to_string()}
        }
        .map_with(|span, ident| Spanned(span, ident.span()))
        .labelled("LetName")
    }
}

impl AssignOp {
    pub fn parser<'a, I>()
    -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>> + Clone
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        just(Token::Equal)
            .to(AssignOp::Assign)
            .or(just([Token::Plus, Token::Equal]).to(AssignOp::Add))
            .or(just([Token::Subtract, Token::Equal]).to(AssignOp::Subtract))
            .or(just([Token::Multiply, Token::Equal]).to(AssignOp::Multiply))
            .or(just([Token::Divide, Token::Equal]).to(AssignOp::Divide))
            .map_with(|span, tok| Spanned(span, tok.span()))
    }
}
/*
impl Break {
    pub fn parser<'a>() -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>, {
        just(Token::Break).to(Break {})
    }
}

impl Continue {
    pub fn parser<'a>() -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>, {
        just(Token::Continue).to(Continue {})
    }
}

impl Import {
    pub fn parser<'a>() -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>, {
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
    pub fn parser<'a>() -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>, {
        just(Token::Import)
            .ignore_then(Import::parser())
            .then_ignore(just(Token::Semicolon))
            .map(|import| ImportStmt { import })
    }
}

impl Module {
    pub fn parser<'a>(
        stmt: impl chumsky::Parser<Token<'a>, Statement, Error = Simple<Token<'a>>> + 'a,
    ) -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>, {
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
    pub fn parser<'a>() -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>, {
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
    pub fn parser<'a, I>()
    -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>> + Clone
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        select! {
            Token::Integer(i) => SeparateNumberParserBecauseIdkWhy(i64::from_str(i).unwrap())
        }
        .map_with(|span, tok| Spanned(span, tok.span()))
    }
}

impl ArrayIndex {
    pub fn parser<'a, I>(
        expr: impl Parser<'a, I, Spanned<Expr>, extra::Err<Rich<'a, Token<'a>>>> + Clone + 'a,
    ) -> impl Parser<'a, I, Spanned<Self>, extra::Err<Rich<'a, Token<'a>>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        IdentAst::parser()
            .then(expr.delimited_by(just(Token::BracketOpen), just(Token::BracketClose)))
            .map_with(|span, index| {
                Spanned(
                    ArrayIndex {
                        index: Box::new(span.1),
                        arr_name: span.0,
                    },
                    index.span(),
                )
            })
            .labelled("ArrayIndex")
    }
}
/*
impl StuffThatCanGoIntoReassignment {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Spanned<Self>, Error = Simple<Token<'a>>>
    {
        ArrayIndex::parser(Expr::parser())
            .map(StuffThatCanGoIntoReassignment::ArrayIndex)
            .or(IdentAst::parser().map(StuffThatCanGoIntoReassignment::IdentAst))
            .map_with(|span, index| Spanned(span, index))
    }
}
*/
