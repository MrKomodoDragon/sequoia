#[allow(dead_code)]
use logos::Logos;

use crate::lexer::Token;
use crate::parser::parse;
mod ast;
mod lexer;
mod parser;
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
