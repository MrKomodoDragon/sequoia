#[allow(dead_code)]
use logos::Logos;

use crate::lexer::Token;
use crate::parser::parse;
mod ast;
mod lexer;
mod parser;
use chumsky::{debug, prelude::*};
fn main() {
    let str = String::from(r#"n bomb_is_sus(sus: Int) -> Int {
                   if true {
                        return true;
                    } elsif true {
                        return true;
                    } elsif false {
                        return false;
                    }
                    else true {
                        return true;
                    }
        
                    }
                    bomb_is_sus(9, bomb=9);"#);
    let lex: Vec<_> = Token::lexer(&str).spanned().collect();
    println!("Ran lexer succesfully");
    println!("{:#?}", lex);
    let ast_ = parse(lex);
    println!("{:#?}", ast_);
}
