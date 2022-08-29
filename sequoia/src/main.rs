#![warn(
   clippy::all,
   clippy::pedantic,
)]
#![warn(clippy::cargo)]
use logos::Logos;

use crate::lexer::Token;
use crate::parser::parse;
mod ast;
mod lexer;
mod parser;
mod compiler;
fn main() {
    let str = String::from(
        r#" let x: NoneType = None;

    "#,
    );
    let lex: Vec<_> = Token::lexer(&str).spanned().collect();
    println!("Ran lexer succesfully");
    println!("{:#?}", lex);
    let ast_ = parse(lex);
    println!("{:#?}", ast_);
}
