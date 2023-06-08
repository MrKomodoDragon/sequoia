#![warn(clippy::all, clippy::pedantic)]
#![warn(clippy::cargo)]
use logos::Logos;

use crate::interpreter::expr_eval;
use crate::lexer::Token;
use crate::parser::parse;
mod ast;
mod interpreter;
mod lexer;
mod parser;
fn main() {
    let str = String::from(
        r#"[9, 9, 9, 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9]
    "#,
    );
    let lex: Vec<_> = Token::lexer(&str).spanned().collect();
    println!("Ran lexer succesfully");
    println!("{:#?}", lex);
    let ast_ = parse(lex);
    println!("{:#?}", ast_.as_ref().unwrap());
    let intrepreted = expr_eval(ast_.clone().unwrap());
    println!("{:#?}", intrepreted)
}

