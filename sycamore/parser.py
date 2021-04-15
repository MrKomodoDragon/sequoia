from sly import Parser
import sycamore.lexer as lexer

class SycamoreParser(Parser):
    tokens = lexer.sycamoreLexer.tokens

    def __init__(self):
        self.env = {}
        
    @_('expr PLUS term EOS')
    def expr(self, p):
        return p.expr + p.term

    @_('var_assign')
    def statement(self, p):
        return p.var_assign

    @_('IDENTIFER VAR_ASSIGN STRING EOS')
    def var_assign(self, p):
        return ('var_assign', p.IDENTIFIER, p.STRING)

    @_('IDENTIFER VAR_ASSIGN expr EOS')
    def var_assign(self, p):
        return ('var_assign', p.IDENTIFIER, p.expr)
    
lexer = lexer.sycamoreLexer()
data = 'let x = 9;'
thing = lexer.tokenize(data)
parser = SycamoreParser()
parser.parse(thing)