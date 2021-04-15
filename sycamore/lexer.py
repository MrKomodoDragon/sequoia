import sly
import sys

class sycamoreLexer(sly.Lexer):
    tokens = {AND, OR, EQ, NE, GE, GT, LE, LT, EOS, COLON, PLUS, MULTIPLY, DIVIDE, SUBTRACT, MODULUS, PAREN_OPEN, PAREN_CLOSE, BRACKET_OPEN, BRACKET_CLOSE,
              VAR_ASSIGN, IDENTIFIER, DOUBLE_QUOTE_STRING, SINGLE_QUOTE_STRING, CONST, RETURN, BREAK, IMPORT, SCOPE, IF, ELSE, FUNCTION, ELSE_IF, CONTINUE}

    AND = r'&&'
    OR = r'\|\|'

    EQ = '=='
    NE = '!='
    GE = '>='
    GT = '>'
    LE = '<='
    LT = '<'

    EOS = ';'
    COLON = ':'

    PLUS = r'\+'
    MULTIPLY = r'\*'
    DIVIDE = r'\/'
    SUBTRACT = r'\-'
    MODULUS = '%'

    PAREN_OPEN = r'\('
    PAREN_CLOSE = r'\)'
    BRACKET_OPEN = r'\{'
    BRACKET_CLOSE = r'\{'

    VAR_ASSIGN = '='

    IDENTIFIER = r'[a-zA-Z_][a-zA-Z0-9_]*'

    IDENTIFIER['if'] = IF
    IDENTIFIER['else'] = ELSE
    IDENTIFIER['elsif'] = ELSE_IF

    IDENTIFIER['let'] = SCOPE
    IDENTIFIER['val'] = SCOPE

    IDENTIFIER['const'] = CONST
    IDENTIFIER['def'] = FUNCTION
    IDENTIFIER['fun'] = FUNCTION
    IDENTIFIER['import'] = IMPORT
    IDENTIFIER['use'] = IMPORT

    IDENTIFIER['return'] = RETURN
    IDENTIFIER['break'] = BREAK
    IDENTIFIER['continue'] = CONTINUE
    DOUBLE_QUOTE_STRING = r"\"(?s:[^\"\\\\]|\\\\.)*\""
    SINGLE_QUOTE_STRING = r"'(?s:[^'\\\\]|\\\\.)*'"

    

    @ _(r'\n+')
    def ignore_newline(self, t):
        self.lineno += len(t.value)

    ignore = '\t'

    ignore_comments = r'\#.*'

    def error(self, t):
        print(F'ERROR at line {self.lineno}: Illegal Character {t.value[0]!r}')
        sys.exit(1)

lexer = sycamoreLexer()

data = '''
#test hello world
print("Hello World!");
'''

for thing in lexer.tokenize(data):
    print(thing)

