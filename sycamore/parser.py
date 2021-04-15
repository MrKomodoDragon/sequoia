from sly import Parser
from .lexer import *

class SycamoreParser(Parser):
    tokens = sycamoreLexer.tokens

    def __init__(self):
        self.env = {}
        
    @_('expr PLUS term')
    def expr(self, p):
        return p.expr + p.term

    @_('var_assign')
    def statement(self, p):
        return p.var_assign

    @_('IDENTIFER VAR_ASSIGN STRING')
    def var_assign(self, p):
        return ('var_assign', p.IDENTIFIER, p.STRING)

    @_('IDENTIFER VAR_ASSIGN expr')
    def var_assign(self, p):
        return ('var_assign', p.IDENTIFIER, p.expr)
    
lexer = sycamoreLexer()
data = 'let x = 9;'
thing = lexer.tokenize(data)
parser = SycamoreParser()
parser.parse(thing)