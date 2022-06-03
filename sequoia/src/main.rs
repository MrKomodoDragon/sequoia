#[allow(dead_code)]
use logos::Logos;

use crate::lexer::Token;
use crate::parser::parse;
mod lexer;
mod parser;
fn main() {
    let str = String::from(r#"---9"#);
    let lex: Vec<_> = Token::lexer(&str).spanned().collect();
    println!("Ran lexer succesfully");
    println!("{:#?}", lex);
    let AST_ = parse(lex);
    println!("The returned AST: {:#?}", AST_.unwrap());
}
