#![warn(clippy::all, clippy::pedantic)]
#![warn(clippy::cargo)]
use logos::Logos;

use crate::lexer::Token;
use crate::parser::parse;
use crate::interpreter::expr_eval;
mod ast;
mod lexer;
mod parser;
mod interpreter;
fn main() {
    let str = String::from(
        r#"999999999
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
