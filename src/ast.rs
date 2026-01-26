// let meow: String = "taha";

use chumsky::span::SimpleSpan;
#[derive(Debug, Clone)]
pub struct Spanned<T>(pub T, pub SimpleSpan);
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
    pub name: Spanned<LetName>,
    pub mutable: bool,
    pub kind: Spanned<Kind>, //if you're wondering why it's not named something with the word type in it, blame sampersand.
    pub assign_type: Spanned<AssignOp>,
    pub rhs: Spanned<Expr>,
}

#[derive(Clone, Debug)]
pub enum Literal {
    Integer(i64),
    Str(String),
    Float(f64),
    List(Vec<Spanned<Expr>>),
    Bool(bool),
    ArrrayIndex(Spanned<ArrayIndex>),
    None,
    Exponent(i64),
}

#[derive(Debug, Clone)]
pub enum Kind {
    Int,
    Float,
    Str,
    Bool,
    NoneType,
    List {
        kind: Box<Spanned<Kind>>,
        size: Option<Spanned<SeparateNumberParserBecauseIdkWhy>>,
    },
    Union(Vec<Spanned<Kind>>),
    Optional(Box<Spanned<Kind>>),
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
    Pow,
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
    Literal(Spanned<Literal>),
    BinaryOperator(
        Box<Spanned<Expr>>,
        Spanned<BinaryOperator>,
        Box<Spanned<Expr>>,
    ),
    UnaryOperator(Spanned<UnaryOperator>, Box<Spanned<Expr>>),
    ComparisonOperators(
        Box<Spanned<Expr>>,
        Spanned<ComparisonOperators>,
        Box<Spanned<Expr>>,
    ),
    Ident(Spanned<IdentAst>),
}
#[derive(Debug, Clone)]
pub enum Statement {
    Let(Spanned<Let>),
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
    pub statements: Vec<Spanned<Statement>>,
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
    pub arr_name: Spanned<IdentAst>,
    pub index: Box<Spanned<Expr>>,
}
#[derive(Debug, Clone)]
pub enum StuffThatCanGoIntoReassignment {
    IdentAst(Spanned<IdentAst>),
    ArrayIndex(Spanned<ArrayIndex>),
}
