#[derive(Debug, Clone)]
pub struct IdentAst {
    pub name: String,
}
#[derive(Debug, Clone)]
pub struct Return {
    pub expr: Expr,
}
#[derive(Clone, Debug)]
pub struct Arg {
    pub name: IdentAst,
    pub kind: Kind,
}
#[derive(Clone, Debug)]
pub struct FunctionDecl {
    pub name: IdentAst,
    pub args: Vec<Arg>,
    pub return_kind: Kind,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Let {
    pub name: IdentAst,
    pub kind: Kind, //if you're wondering why it's not named something with the word type in it, blame sampersand.
    pub rhs: Expr,
}

#[derive(Clone, Debug)]
pub enum Literal {
    Integer(i64),
    Str(String),
    Float(f64),
    List(Vec<Expr>),
    Tuple(Vec<Expr>),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub enum Kind {
    Int,
    Float,
    Str,
    Bool,
    List(Box<Kind>),
    Union(Vec<Box<Kind>>),
    Optional(Box<Kind>),
}
#[derive(Clone, Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
}
#[derive(Clone, Debug)]
pub enum ComparisonOperators {
    GreaterThan,
    LessThan,
    GreaterOrEqualTo,
    LessThanOrEqualTo,
}
#[derive(Clone, Debug)]
pub enum UnaryOperator {
    Neg,
}
#[derive(Clone, Debug)]
pub enum Expr {
    Literal(Literal),
    BinaryOperator(Box<Expr>, BinaryOperator, Box<Expr>),
    UnaryOperator(UnaryOperator, Box<Expr>),
    ComparisonOperators(Box<Expr>, ComparisonOperators, Box<Expr>),
    Ident(IdentAst),
    
}
#[derive(Debug, Clone)]
pub enum Statement {
    Let(Let),
    FnDecl(FunctionDecl),
    Return(Return),
    FnCall(FnCall)
}

#[derive(Debug, Clone)]
pub struct Root {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct FnCall {
    pub name: IdentAst,
    pub args: Vec<Expr>,
}