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
        r#" let x: Float = -(9.0+9.982372047);
            let y: Int = 5+4*2/2;
            let z: Float = x*9;
    "#,
    );
    let now = Instant::now();
    let lex: Vec<_> = Token::lexer(&str).spanned().collect();
    println!("Ran lexer succesfully");
    println!("{:#?}", lex);
    let ast_ = parse(lex);
    println!("{:#?}", ast_.as_ref().unwrap());
    interpret(ast_.unwrap().0);
    let later = Instant::now() - now;
    println!("{:#?}", later);
}
