use logos::Logos;
#[derive(Logos, Debug, PartialEq, Clone, Eq, Hash)]
pub enum Token<'a> {
    // Conditionals
    #[token("&&")]
    And,

    #[token("||")]
    Or,

    //Type related operators
    #[token("|")]
    Union,

    #[token("?")]
    Optional,

    #[token(",")]
    Comma,

    //Equality
    #[token("==")]
    IsEqual,

    #[token("!=")]
    NotEqual,

    #[token(">=")]
    GreaterOrEqual,

    #[token(">")]
    GreaterThan,

    #[token("<")]
    LessThan,

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

    #[token("=")]
    Equal,

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

    //Keywords
    #[token("if")]
    If,

    #[token("while")]
    While,

    #[token("else")]
    Else,

    #[token("elsif")]
    ElsIf,

    #[token("let")]
    Let,

    #[token("mut")]
    MutableKeyword,

    #[token("fn")]
    Function,

    #[token("import")]
    #[token("use")]
    Import,

    #[token("from")]
    From,

    #[token("::")]
    DoubleColon,

    #[token("return")]
    Return,

    #[token("break")]
    Break,

    #[token("continue")]
    Continue,

    #[token("as")]
    As,

    #[token("mod")]
    Mod,

    #[token("pub")]
    Public,

    #[token("None")]
    None,
    //Basic types
    #[regex(r#"(?x)" (?: \\. | [^\\"] )* ""#)]
    Str(&'a str),

    #[regex("([1-9][0-9]*)|0")]
    Integer(&'a str),

    #[regex("[0-9]+\\.[0-9]+")]
    Float(&'a str),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", priority = 2)]
    Ident(&'a str),

    #[token("true")]
    #[token("false")]
    Boolean(&'a str),

    #[token("Str")]
    #[token("Int")]
    #[token("Float")]
    #[token("Bool")]
    #[token("NoneType")]
    Type(&'a str),

    #[error]
    #[regex(r"//.*", logos::skip)]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}
