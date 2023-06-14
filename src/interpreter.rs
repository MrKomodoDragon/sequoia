use std::{
    collections::HashMap,
    ops::{Add, Div, Mul, Neg, Not, Rem, Sub},
    println, todo,
};


fn eval_let_statement(let_stmt: Let, vars_dict: &mut HashMap<String, HashMap<String, Value>>)  {
    if let Some(inner_hashmap) = vars_dict.get_mut(&String::from("globals")) {
        inner_hashmap.insert(let_stmt.name.name, expr_eval(let_stmt.rhs));

    }
    println!("{:#?}", vars_dict);

}
use crate::ast::*;
pub fn interpret(tree: Root) {
    for i in tree.statements {
        let mut vars: HashMap<String, HashMap<String, Value>> = HashMap::from([(
            "globals".to_string(),
            HashMap::from([("dummy".to_string(), Value::Int(9))]),
        )]);
        match i {
            crate::ast::Statement::Let(i) => eval_let_statement(i, &mut vars),
            _ => todo!(),
        }
    }
}
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Int(i64),
    Str(String),
    Ident(String),
    Bool(bool),
    Float(f64),
    ArrrayIndex { arr_name: String, index: Box<Value> },
    List(Vec<Value>),
    None,
}

impl Sub for Value {
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(i), Value::Int(i2)) => Value::Int(i - i2),
            _ => todo!(),
        }
    }

    type Output = Value;
}

impl Add for Value {
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(i), Value::Int(i2)) => Value::Int(i + i2),
            _ => {
                panic!("Incompatible types are being added");
            }
        }
    }
    type Output = Value;
}

impl Mul for Value {
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(i), Value::Int(i2)) => Value::Int(i * i2),
            _ => todo!(),
        }
    }
    type Output = Value;
}

impl Div for Value {
    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(i), Value::Int(i2)) => Value::Int(i / i2),
            _ => todo!(),
        }
    }
    type Output = Value;
}

impl Rem for Value {
    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(i), Value::Int(i2)) => Value::Int(i % i2),
            _ => todo!(),
        }
    }
    type Output = Value;
}

impl Neg for Value {
    fn neg(self) -> Self::Output {
        if let Value::Int(i) = self {
            Value::Int(-i)
        } else {
            panic!("not an integer");
        }
    }
    type Output = Value;
}

impl Not for Value {
    fn not(self) -> Self::Output {
        match self {
            Value::Int(i) => Value::Int(!i),
            Value::Bool(i) => Value::Bool(!i),
            _ => todo!(),
        }
    }
    type Output = Value;
}

pub(crate) fn expr_eval(input: Expr) -> Value {
    match input {
        Expr::Literal(literal) => literal_eval(literal),
        Expr::BinaryOperator(expr, op, expr2) => match op {
            BinaryOperator::Add => expr_eval(*expr) + expr_eval(*expr2),
            BinaryOperator::Sub => expr_eval(*expr) - expr_eval(*expr2),
            BinaryOperator::Mul => expr_eval(*expr) * expr_eval(*expr2),
            BinaryOperator::Div => expr_eval(*expr) / expr_eval(*expr2),
            BinaryOperator::Modulus => expr_eval(*expr) % expr_eval(*expr2),
            BinaryOperator::AND => {
                let evaled_expr1 = expr_eval(*expr);
                let evaled_expr2 = expr_eval(*expr2);
                println!("{:?}", evaled_expr1);
                println!("{:?}", evaled_expr2);
                match (evaled_expr1, evaled_expr2) {
                    (Value::Bool(i1), Value::Bool(i2)) => Value::Bool(i1 && i2),
                    _ => todo!(),
                }
            }
            BinaryOperator::OR => {
                let evaled_expr1 = expr_eval(*expr);
                let evaled_expr2 = expr_eval(*expr2);
                println!("{:?}", evaled_expr1);
                println!("{:?}", evaled_expr2);
                match (evaled_expr1, evaled_expr2) {
                    (Value::Bool(i1), Value::Bool(i2)) => Value::Bool(i1 || i2),
                    _ => todo!(),
                }
            }
        },
        Expr::UnaryOperator(op, expr) => match op {
            UnaryOperator::Neg => -(expr_eval(*expr)),
            UnaryOperator::NOT => !(expr_eval(*expr)),
        },
        Expr::ComparisonOperators(expr, op, expr2) => match op {
            ComparisonOperators::GreaterThan => Value::Bool(expr_eval(*expr) > expr_eval(*expr2)),
            ComparisonOperators::LessThan => Value::Bool(expr_eval(*expr) < expr_eval(*expr2)),
            ComparisonOperators::GreaterOrEqualTo => {
                Value::Bool(expr_eval(*expr) >= expr_eval(*expr2))
            }
            ComparisonOperators::LessThanOrEqualTo => {
                Value::Bool(expr_eval(*expr) <= expr_eval(*expr2))
            }
        },
        Expr::Ident(i) => Value::Ident(i.name),
    }
}

fn literal_eval(input: Literal) -> Value {
    match input {
        Literal::Integer(i) => Value::Int(i),
        Literal::Str(i) => Value::Str(i),
        Literal::Float(i) => Value::Float(i),
        Literal::List(list) => {
            let mut evaled_exprs = Vec::new();
            for i in list {
                let i_evaled = expr_eval(i);
                evaled_exprs.push(i_evaled);
            }
            return Value::List(evaled_exprs);
        }
        Literal::Bool(i) => Value::Bool(i),
        Literal::ArrrayIndex(i) => Value::ArrrayIndex {
            arr_name: i.arr_name.name,
            index: Box::new(expr_eval(*(i.index))),
        },
        Literal::None => Value::None,
    }
}
