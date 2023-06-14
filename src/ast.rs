use std::ops::Range;
#[derive(Debug, Clone)]
pub struct Spanned<T>(pub T, pub Range<usize>);
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
    pub public: bool,
    pub name: IdentAst,
    pub args: Option<Vec<Arg>>,
    pub kwargs: Option<Vec<Arg>>,
    pub return_kind: Kind,
    pub statements: Option<Vec<Statement>>,
}

#[derive(Debug, Clone)]
pub struct Let {
    pub name: LetName,
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
    Bool(bool),
    ArrrayIndex(ArrayIndex),
    None,
}

#[derive(Debug, Clone)]
pub enum Kind {
    Int,
    Float,
    Str,
    Bool,
    NoneType,
    List {
        kind: Box<Kind>,
        size: Option<SeparateNumberParserBecauseIdkWhy>,
    },
    Union(Vec<Kind>),
    Optional(Box<Kind>),
}
#[derive(Clone, Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Modulus,
    AND,
    OR,
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
    NOT,
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
    Return(Spanned<Return>),
    FnCall(FnCall),
    While(While),
    If(If),
    Break(Break),
    Continue(Continue),
    Import(ImportStmt),
    Module(Module),
    ReAssignment(ReAss),
}

#[derive(Debug, Clone)]
pub struct Root {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct FnCall {
    pub name: IdentAst,
    pub args: Vec<Expr>,
    pub kwargs: Vec<Kwarg>,
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
pub struct LetName {
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
#[derive(Debug, Clone)]
pub struct Import {
    pub peth: String,
    pub head: Box<Head>,
}
#[derive(Debug, Clone)]
pub struct Peth {
    pub peth: String,
    pub alias: Option<IdentAst>,
}
#[derive(Debug, Clone)]
pub enum Head {
    Single {
        name: IdentAst,
        alias: Option<IdentAst>,
    },
    Many(Vec<Import>),
}
#[derive(Debug, Clone)]
pub struct ImportStmt {
    pub import: Import,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub name: IdentAst,
    pub modules: Option<Box<Module>>,
    pub functions: Option<Vec<FunctionDecl>>,
}
#[derive(Debug, Clone)]
pub struct ReAss {
    pub thing_to_be_reassigned: StuffThatCanGoIntoReassignment,
    pub assignop: AssignOp,
    pub rhs: Expr,
}
#[derive(Debug, Clone)]
pub struct SeparateNumberParserBecauseIdkWhy(pub i64);

#[derive(Debug, Clone)]
pub struct ArrayIndex {
    pub arr_name: IdentAst,
    pub index: Box<Expr>,
}
#[derive(Debug, Clone)]
pub enum StuffThatCanGoIntoReassignment {
    IdentAst(IdentAst),
    ArrayIndex(ArrayIndex),
}
