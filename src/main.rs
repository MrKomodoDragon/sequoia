#![warn(clippy::all, clippy::pedantic)]
#![warn(clippy::cargo)]
use crate::ast::*;
use crate::interpreter::expr_eval;
use crate::interpreter::interpret;
use crate::interpreter::lower_type;
use crate::lexer::Token;
use crate::parser::parse;
use logos::Logos;
use std::time::{Duration, Instant};
mod ast;
mod interpreter;
mod lexer;
mod parser;
fn main() {
    let str = String::from(
        r#" let x: Int = 5*(2+1);
            let y: Int = 5+4*2/2;
            let z: Int[] = [9,9,9,9,9,9];
    "#,
    );
    let now = Instant::now();
    let lex: Vec<_> = Token::lexer(&str).spanned().collect();
    println!("Ran lexer succesfully");
    println!("{:#?}", lex);
    let ast_ = parse(lex);
    println!("{:#?}", ast_.as_ref().unwrap());
    println!(
        "{:#?}",
        lower_type(Expr::Literal(Spanned(Literal::Integer(9), 0..7)))
    );
    interpret(ast_.unwrap().0);
    let later = Instant::now() - now;
    //println!("{:#?}", later);
}
