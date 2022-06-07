#[allow(dead_code)]
use logos::Logos;

use crate::lexer::Token;
use crate::parser::parse;
mod lexer;
mod parser;
fn main() {
    let str = String::from(r#"let x: Int = 1+1;"#);
    let lex: Vec<_> = Token::lexer(&str).spanned().collect();
    println!("Ran lexer succesfully");
    println!("{:#?}", lex);
    let ast_ = parse(lex);
    println!("The returned AST: {:#?}", ast_.unwrap());
}
