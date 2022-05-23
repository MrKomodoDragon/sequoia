use std::ops::Deref;

use logos::Logos;

use crate::lexer::Token;
use crate::parser::parse;
mod lexer;
mod parser;
fn main() {
    let str = String::from(
        r#"
        from std::output import print;
        fun main {
            print "hello world!";
        }
        "#,
    );
    let lex: Vec<_> = Token::lexer(&str).spanned().collect();
    println!("Ran lexer succesfully");
    let tokens = lex.last();
    let thing = tokens.clone().unwrap();
    let span = &thing.1;
    parse(lex, span)
}
