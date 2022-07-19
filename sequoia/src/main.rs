#[allow(dead_code)]
use logos::Logos;

use crate::lexer::Token;
use crate::parser::parse;
mod ast;
mod lexer;
mod parser;
fn main() {
    let str = String::from(
        r#"fn bomb_is_sus(sus: Int) -> Int {
            return sus
            bomb_is_sus(9)
    }"#,
    );
    let lex: Vec<_> = Token::lexer(&str).spanned().collect();
    println!("Ran lexer succesfully");
    println!("{:#?}", lex);
    let ast_ = parse(lex);
    println!("The returned AST: {:#?}", ast_);
}
