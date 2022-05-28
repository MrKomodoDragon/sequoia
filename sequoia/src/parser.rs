use std::error::Error;

use crate::lexer::Token;
use chumsky::prelude::*;
use chumsky::Stream;
enum Expr {}

pub fn get_stream(tokens: Vec<(Token, std::ops::Range<usize>)>) -> Stream<std::ops::Range<usize>, dyn Iterator<Item = Vec<(Token, std::ops::Range<usize>)>>> {
    println!("{:#?}", tokens);
    let span = (&(tokens.last().unwrap()).1).clone();
    let stream = Stream::from_iter(span, tokens.iter());
    stream
}

pub fn parse(stream) {
    
}