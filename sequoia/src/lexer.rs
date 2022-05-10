use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    // Conditionals
    #[token("&&")]
    And,

    #[token("||")]
    Or,

    //Equality
    #[token("==")]
    Equal,

    #[token("!=")]
    NotEqual,

    #[token(">=")]
    GreaterOrEqual,

    #[token(">")]
    GreaterThan,

    #[token("<=")]
    LessOrEqual,

    //EOL chars
    #[token(";")]
    Semicolon,

    #[token(":")]
    Colon,

    //Arithmetic operators
    #[token("+")]
    Plus,

    #[token("*")]
    Multiply,

    #[token("/")]
    Divide,

    #[token("-")]
    Subtract,

    #[token("%")]
    Modulus,

    //Parentheses and Brackets
    #[token("(")]
    ParenOpen,

    #[token(")")]
    ParenClose,

    #[token("[")]
    BracketOpen,

    #[token("]")]
    BracketClose,

    #[token("{")]
    BraceOpen,

    #[token("}")]
    BraceClose,

    //Var Assign
    #[token("=")]
    VarAssign,

    //Keywords
    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("elsif")]
    ElseIf,

    #[token("let")]
    #[token("var")]
    Scope,

    #[token("const")]
    Const,

    #[token("def")]
    #[token("fun")]
    Function,

    #[token("import")]
    #[token("use")]
    Import,

    #[token("from")]
    From,

    #[token("::")]
    ModuleSeparator,

    #[token("return")]
    Return,

    #[token("Break")]
    Break,

    #[token("continue")]
    Continue,

    //Basic types
    #[regex(r#"(?x)" (?: \\. | [^\\"] )* ""#)]
    Str,

    #[regex("-?[0-9]+", |lex| lex.slice().parse())]
    Integer(i64),

    #[regex("-?[0-9]+\\.[0-9]+", |lex| lex.slice().parse())]
    Float(f64),

    #[regex(r"[a-zA-Z0-9_]+", priority=2)]
    Name,

    #[error]
    #[regex(r"//.*", logos::skip)]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}
