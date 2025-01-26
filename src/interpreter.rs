use std::{
    collections::HashMap,
    ops::{Add, Div, Mul, Neg, Not, Rem, Sub},
    println, todo,
};
/*
 * fn lower type(expr) {
 * match expr {
 * Integer => Kind::Int
 * Float => Kind::Float
 * String => Kind::String
 * }
 * }
 *
 * if expected.type == got.type {
 * execute logic
 * }
 *
 * else {raise error}
 * */

pub fn lower_type(expr: Expr) -> Kind {
    match expr {
        Expr::Literal(i) => match i.0 {
            Literal::Integer(_) => Kind::Int,
            Literal::Str(_) => Kind::Str,
            _ => todo!(),
        },
        _ => todo!(),
    }
}

fn eval_let_statement(let_stmt: Let, vars_dict: &mut HashMap<String, HashMap<String, Value>>) {
    print!(
        "at the beginning of the eval of let stmt: {:#?}",
        &vars_dict
    );
    match (let_stmt.kind.0, let_stmt.rhs.0.clone()) {
        (Kind::Int, Expr::Literal(i)) => {} /*{
        println!("evaling let stmt with integer");
        let eval_literaled = literal_eval(i);
        if let Value::Int(integer) = eval_literaled {
        if let Some(inner_hashmap) = vars_dict.get_mut(&String::from("globals")) {
        inner_hashmap.insert(let_stmt.0.name.name, expr_eval(let_stmt.rhs.clone()));
        }
        println!("Let stmt integer after: {:#?}", vars_dict);
        }
        }*/
        (Kind::Int, Expr::BinaryOperator(expr1, op, expr2)) => {
            println!("evaling let stmt with binop");
            let evaled_expr = expr_eval(let_stmt.rhs.clone(), vars_dict);
            if let Value::Int(integer) = evaled_expr {
                if let Some(inner_hashmap) = vars_dict.get_mut(&String::from("globals")) {
                    inner_hashmap.insert(let_stmt.name.0.name, evaled_expr);
                }
                println!("let stmt binops after: {:#?}", vars_dict);
            }
        }
        (Kind::Int, Expr::UnaryOperator(op, expr)) => {
            let evaled_expr = expr_eval(let_stmt.rhs.clone(), vars_dict);
            if let Value::Int(integer) = evaled_expr {
                if let Some(inner_hashmap) = vars_dict.get_mut(&String::from("globals")) {
                    // inner_hashmap.insert(let_stmt.name.0name, evaled_expr);
                }
            }
        }
        (Kind::Int, Expr::ComparisonOperators(_, _, _)) => {
            panic!("You have declared the type as Kind::Int but your ");
        }
        (Kind::Int, Expr::Ident(_)) => todo!(),
        (Kind::Float, Expr::Literal(_)) => todo!(),
        (Kind::Float, Expr::BinaryOperator(_, _, _)) => todo!(),
        (Kind::Float, Expr::UnaryOperator(_, _)) => todo!(),
        (Kind::Float, Expr::ComparisonOperators(_, _, _)) => todo!(),
        (Kind::Float, Expr::Ident(_)) => todo!(),
        (Kind::Str, Expr::Literal(_)) => todo!(),
        (Kind::Str, Expr::BinaryOperator(_, _, _)) => todo!(),
        (Kind::Str, Expr::UnaryOperator(_, _)) => todo!(),
        (Kind::Str, Expr::ComparisonOperators(_, _, _)) => todo!(),
        (Kind::Str, Expr::Ident(_)) => todo!(),
        (Kind::Bool, Expr::Literal(_)) => todo!(),
        (Kind::Bool, Expr::BinaryOperator(_, _, _)) => todo!(),
        (Kind::Bool, Expr::UnaryOperator(_, _)) => todo!(),
        (Kind::Bool, Expr::ComparisonOperators(_, _, _)) => todo!(),
        (Kind::Bool, Expr::Ident(_)) => todo!(),
        (Kind::NoneType, Expr::Literal(_)) => todo!(),
        (Kind::NoneType, Expr::BinaryOperator(_, _, _)) => todo!(),
        (Kind::NoneType, Expr::UnaryOperator(_, _)) => todo!(),
        (Kind::NoneType, Expr::ComparisonOperators(_, _, _)) => todo!(),
        (Kind::NoneType, Expr::Ident(_)) => todo!(),
        (Kind::List { kind, size }, Expr::Literal(_)) => todo!(),
        (Kind::List { kind, size }, Expr::BinaryOperator(_, _, _)) => todo!(),
        (Kind::List { kind, size }, Expr::UnaryOperator(_, _)) => todo!(),
        (Kind::List { kind, size }, Expr::ComparisonOperators(_, _, _)) => todo!(),
        (Kind::List { kind, size }, Expr::Ident(_)) => todo!(),
        (Kind::Union(_), Expr::Literal(_)) => todo!(),
        (Kind::Union(_), Expr::BinaryOperator(_, _, _)) => todo!(),
        (Kind::Union(_), Expr::UnaryOperator(_, _)) => todo!(),
        (Kind::Union(_), Expr::ComparisonOperators(_, _, _)) => todo!(),
        (Kind::Union(_), Expr::Ident(_)) => todo!(),
        (Kind::Optional(_), Expr::Literal(_)) => todo!(),
        (Kind::Optional(_), Expr::BinaryOperator(_, _, _)) => todo!(),
        (Kind::Optional(_), Expr::UnaryOperator(_, _)) => todo!(),
        (Kind::Optional(_), Expr::ComparisonOperators(_, _, _)) => todo!(),
        (Kind::Optional(_), Expr::Ident(_)) => todo!(),
    }
}
use crate::ast::*;
pub fn interpret(tree: Root) {
    let mut vars: HashMap<String, HashMap<String, Value>> =
        HashMap::from([("globals".to_string(), HashMap::from([]))]);
    for i in tree.statements {
        match i.0 {
            crate::ast::Statement::Let(i) => eval_let_statement(i.0, &mut vars),
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

pub(crate) fn expr_eval(input: Spanned<Expr>, vars_dict: &mut HashMap<String, HashMap<String, Value>>) -> Value {
    match input.0 {
        Expr::Literal(literal) => literal_eval(literal),
        Expr::BinaryOperator(expr,  op, expr2) => match op.0 {
            BinaryOperator::Add => expr_eval(*expr, vars_dict) + expr_eval(*expr2, vars_dict),
            BinaryOperator::Sub => expr_eval(*expr, vars_dict) - expr_eval(*expr2, vars_dict),
            BinaryOperator::Mul => expr_eval(*expr, vars_dict) * expr_eval(*expr2, vars_dict),
            BinaryOperator::Div => expr_eval(*expr, vars_dict) / expr_eval(*expr2, vars_dict),
            BinaryOperator::Modulus => expr_eval(*expr, vars_dict) % expr_eval(*expr2, vars_dict),
            BinaryOperator::AND => {
                let evaled_expr1 = expr_eval(*expr, vars_dict);
                let evaled_expr2 = expr_eval(*expr2, vars_dict);
                println!("{:?}", evaled_expr1);
                println!("{:?}", evaled_expr2);
                match (evaled_expr1, evaled_expr2) {
                    (Value::Bool(i1), Value::Bool(i2)) => Value::Bool(i1 && i2),
                    _ => todo!(),
                }
            }
            BinaryOperator::OR => {
                let evaled_expr1 = expr_eval(*expr, vars_dict);
                let evaled_expr2 = expr_eval(*expr2, vars_dict);
                println!("{:?}", evaled_expr1);
                println!("{:?}", evaled_expr2);
                match (evaled_expr1, evaled_expr2) {
                    (Value::Bool(i1), Value::Bool(i2)) => Value::Bool(i1 || i2),
                    _ => todo!(),
                }
            }
        },
        Expr::UnaryOperator(op, expr) => match op.0 {
            UnaryOperator::Neg => todo!(), /*-(expr_eval(*expr)),*/
            UnaryOperator::NOT => todo!(),
        },
        Expr::ComparisonOperators(expr, op, expr2) => match op.0 {
            ComparisonOperators::GreaterThan => Value::Bool(expr_eval(*expr, vars_dict) > expr_eval(*expr2, vars_dict)),
            ComparisonOperators::LessThan => Value::Bool(expr_eval(*expr, vars_dict) < expr_eval(*expr2, vars_dict)),
            ComparisonOperators::GreaterOrEqualTo => {
                Value::Bool(expr_eval(*expr, vars_dict) >= expr_eval(*expr2, vars_dict))
            }
            ComparisonOperators::LessThanOrEqualTo => {
                Value::Bool(expr_eval(*expr, vars_dict) <= expr_eval(*expr2, vars_dict))
            }
        },
        Expr::Ident(i) => {
            let string = i.0.name;
            let val = vars_dict.get("globals").unwrap().get(string.as_str()).unwrap();
            val.clone()
        },
    }
}

fn literal_eval(input: Spanned<Literal>) -> Value {
    match input.0 {
        Literal::Integer(i) => Value::Int(i),
        Literal::Str(i) => Value::Str(i),
        Literal::Float(i) => Value::Float(i),
        Literal::List(list) => todo!(), /* {
        let mut evaled_exprs = Vec::new();
        for i in list {
        let i_evaled = expr_eval(i);
        evaled_exprs.push(i_evaled);
        }
        return Value::List(evaled_exprs);
        }*/
        Literal::Bool(i) => Value::Bool(i),
        Literal::ArrrayIndex(i) => todo!(), //Value::ArrrayIndex {
        //arr_name: i.arr_name.name,
        //index: Box::new(expr_eval(*(i.index))),
        //},
        Literal::None => Value::None,
    }
}
