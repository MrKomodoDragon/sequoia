use std::todo;

use crate::ast::*;
fn interpret(tree: Root) {
    for i in tree.statements {
        match i {
            crate::ast::Statement::Let(_) => todo!(),
            _ => todo!()
        }
    }
}

pub(crate) fn expr_eval(input: Expr) -> i64 {
    match input {
        Expr::Literal(literal) => literal_eval(literal),
        Expr::BinaryOperator(expr ,op, expr2) => match op {
            BinaryOperator::Add => expr_eval(*expr) + expr_eval(*expr2),
            BinaryOperator::Sub => expr_eval(*expr) - expr_eval(*expr2),
            BinaryOperator::Mul => expr_eval(*expr) * expr_eval(*expr2),
            BinaryOperator::Div => expr_eval(*expr) / expr_eval(*expr2),
            BinaryOperator::Modulus => expr_eval(*expr) % expr_eval(*expr2),
            BinaryOperator::AND => todo!(),
            BinaryOperator::OR => todo!(),
        }
        Expr::UnaryOperator(op, expr) => match op {
            UnaryOperator::Neg => -(expr_eval(*expr)),
            UnaryOperator::NOT => !(expr_eval(*expr)),
            
        },
        Expr::ComparisonOperators(_, _, _) => todo!(),
        Expr::Ident(_) => todo!(),
    }
}


fn literal_eval(input: Literal) -> i64 {
    match input {
        Literal::Integer(_) => literal_integer(input),
        Literal::Str(_) => todo!(),
        Literal::Float(_) => todo!(),
        Literal::List(_) => todo!(),
        Literal::Bool(_) => todo!(),
        Literal::ArrrayIndex(_) => todo!(),
        Literal::None => todo!(),
    }
    
}

fn literal_integer(input: Literal) -> i64 {
    match input {
        Literal::Integer(i) => i,
        _ => todo!()
    }
}