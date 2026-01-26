#![warn(clippy::all, clippy::pedantic)]
#![warn(clippy::cargo)]
use crate::ast::*;
use crate::interpreter::expr_eval;
use crate::interpreter::interpret;
use crate::interpreter::lower_type;
use crate::lexer::Token;
use crate::parser::parse;
use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::span::SimpleSpan;
use logos::Logos;
use std::time::{Duration, Instant};
mod ast;
mod interpreter;
mod lexer;
mod parser;
fn main() {
    let str = String::from(
        r#" let x: Int = 3**2;
    "#,
    );
    //let now = Instant::now();
    let lex: Vec<(Token, SimpleSpan)> = Token::lexer(&str)
        .spanned()
        .map(|(tok, span)| match tok {
            Ok(t) => (t, SimpleSpan::from(span)),
            Err(()) => (Token::Error, span.into()),
        })
        .collect();
    println!("Ran lexer succesfully");
    println!("{:#?}", lex);
    /*let ast_ = parse(lex);
    match ast_.into_result() {
        Ok(ast_) => {
            println!("{:#?}", ast_);
            interpret(ast_.0);
        }
        Err(errs) => {
            for err in errs {
                println!("{:#?}", err);
            }
        }
    }
    //println!("{:#?}", ast_.as_ref().unwrap());
    /*println!(
        "{:#?}",
        lower_type(Expr::Literal(Spanned(Literal::Integer(9), 0..7)))
    );*/
    //interpret(ast_.unwrap().0);
    let later = Instant::now() - now;
    //println!("{:#?}", later);
    //print!("Currently In Progress")*/
}
