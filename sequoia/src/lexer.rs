use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
enum Token {
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

}
