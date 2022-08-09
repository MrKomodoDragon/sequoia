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
    pub args: Option<Vec<Arg>>,
    pub kwargs: Option<Vec<Arg>>,
    pub return_kind: Kind,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Let {
    pub name: IdentAst,
    pub mutable: bool,
    pub kind: Kind, //if you're wondering why it's not named something with the word type in it, blame sampersand.
    pub assign_type: AssignOp,
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
    FnCall(FnCall),
    While(While),
    If(If),
    Break(Break),
    Continue(Continue)
}

#[derive(Debug, Clone)]
pub struct Root {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct FnCall {
    pub name: IdentAst,
    pub args: Option<Vec<Expr>>,
    pub kwargs: Option<Vec<Kwarg>>,
}
#[derive(Debug, Clone)]
pub struct While {
    pub cond: Expr,
    pub stmts: Vec<Statement>,
}
#[derive(Debug, Clone)]
pub struct If {
    pub cond: Expr,
    pub stmts: Vec<Statement>,
    pub elsif: Option<Vec<ElseIf>>,
    pub r#else: Option<Else>,
}
#[derive(Debug, Clone)]
pub struct ElseIf {
    pub cond: Expr,
    pub stmts: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Else {
    pub cond: Expr,
    pub stmts: Vec<Statement>,
}
#[derive(Debug, Clone)]
pub struct Kwarg {
    pub name: IdentAst,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct KwargName {
    pub name: String,
}

#[derive(Debug, Clone)]
pub enum AssignOp {
    Assign,
    Add,
    Subtract,
    Multiply,
    Divide,
}
#[derive(Debug, Clone)]
pub struct Break {}
#[derive(Debug, Clone)]
pub struct Continue {}