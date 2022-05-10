use logos::Logos;

use crate::lexer::Token;

mod lexer;
fn main() {
    let str = String::from(
        r#"
        from std::output import print;
        fun main {
            print "hello world!";
        }
        "#,
    );
    let mut lex = Token::lexer(&str);
    let thing: Vec<Token> = lex.collect();
    println!("The tokens are {:?}", thing);
}
