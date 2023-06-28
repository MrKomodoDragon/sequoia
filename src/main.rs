#![warn(clippy::all, clippy::pedantic)]
#![warn(clippy::cargo)]
use logos::Logos;
use std::time::{Duration, Instant};

use crate::interpreter::expr_eval;
use crate::interpreter::interpret;
use crate::lexer::Token;
use crate::parser::parse;
mod ast;
mod interpreter;
mod lexer;
mod parser;
fn main() {
    let str = String::from(
        r#" let x: Int = 8 > 9;
            let y: Int = 8;
    "#,
    );
    let now = Instant::now();
    let lex: Vec<_> = Token::lexer(&str).spanned().collect();
    println!("Ran lexer succesfully");
    println!("{:#?}", lex);
    let ast_ = parse(lex);
    println!("{:#?}", ast_.as_ref().unwrap());
    let intrepreted = interpret(ast_.as_ref().unwrap().clone());
    println!("{:#?}", intrepreted);
    let later = Instant::now() - now;
    println!("{:#?}", later)
}
