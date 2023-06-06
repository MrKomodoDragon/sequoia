use std::{todo, ops::Sub};

use crate::ast::*;
fn interpret(tree: Root) {
    for i in tree.statements {
        match i {
            crate::ast::Statement::Let(_) => todo!(),
            _ => todo!()
        }
    }
}
#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Str(String)
}

impl Sub for Value {
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(i), Value::Int(i2)) => Value::Int(i- i2),
            _ => todo!() 
        }
    }

    type Output = Value;
}


pub(crate) fn expr_eval(input: Expr) -> Value {
    match input {
        Expr::Literal(literal) => literal_eval(literal),
        Expr::BinaryOperator(expr ,op, expr2) => match op {
            BinaryOperator::Add => todo!(),
            BinaryOperator::Sub => expr_eval(*expr) - expr_eval(*expr2),
            BinaryOperator::Mul => todo!(),
            BinaryOperator::Div => todo!(),
            BinaryOperator::Modulus => todo!(),
            BinaryOperator::AND => todo!(),
            BinaryOperator::OR => todo!(),
        }
        Expr::UnaryOperator(op, expr) => todo!(),
        Expr::ComparisonOperators(_, _, _) => todo!(),
        Expr::Ident(_) => todo!(),
    }
}


fn literal_eval(input: Literal) -> Value {
    match input {
        Literal::Integer(i) => Value::Int(i),
        Literal::Str(_) => todo!(),
        Literal::Float(_) => todo!(),
        Literal::List(_) => todo!(),
        Literal::Bool(_) => todo!(),
        Literal::ArrrayIndex(_) => todo!(),
        Literal::None => todo!(),
    }
    
}

