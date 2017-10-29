import lex

reserved = (
    'FUN',
    'RETURN',
    'LIST',
    'INT',
    'STRING',
    )

tokens = reserved + (
    'AMPERSAND',
    'AND',
    'ARROW',
    'ASSIGN',
    'ASTERISK',
    'AT',
    'BACKSLASH',
    'BANG',
    'COLON',
    'COMMA',
    'DOLLAR',
    'EQ',
    'GEQ',
    'GT',
    'IDENTIFIER',
    'INTEGER',
    'LBRACE',
    'LBRACKET',
    'LEQ',
    'LPAR',
    'LSHIFT',
    'LT',
    'MINUS',
    'NEQ',
    'OR',
    'PERCENT',
    'PERIOD',
    'PLUS',
    'QUESTION',
    'RBRACE',
    'RBRACKET',
    'RPAR',
    'RSHIFT',
    'SEMICOLON',
    'SLASH',
    'TILDE',
    'UNDERSCORE',
    'VBAR',
    )

keywords = {}
for r in reserved:
    keywords[r.lower()] = r

def t_IDENTIFIER(t):
    r'[a-zA-Z_]([a-zA-Z0-9_-]*[a-zA-Z0-9_]|)'
    t.type = keywords.get(t.value, 'IDENTIFIER')
    return t

def t_newline(t):
    r'\n+'
    t.lineno += len(t.value)
    
def t_INTEGER(t):
    r'\d+'
    try:
        t.value = int(t.value)
    except ValueError:
        print "Line %d: Number %s is too large!" % (t.lineno, t.value)
        t.value = 0
        
    return t

def t_oneline_comment(t):
    r'//.*'

t_ignore = ' \t'

def t_error(t):
    print "Illegal character '%s'" % t.value[0]
    t.skip(1)

t_AMPERSAND = r'&'
t_AND = r'&&'
t_ARROW = r'->'
t_ASSIGN = r'='
t_ASTERISK = r'\*'
t_AT = r'@'
t_BACKSLASH = r'\\'
t_BANG = r'!'
t_COLON = r':'
t_COMMA = r','
t_DOLLAR = r'\$'
t_EQ = r'=='
t_GEQ = r'<='
t_GT = r'>'
t_LBRACE = r'\{'
t_LBRACKET = r'\['
t_LEQ = r'>='
t_LPAR = r'\('
t_LSHIFT = r'<<'
t_LT = r'<'
t_MINUS = r'-'
t_NEQ = r'!='
t_OR = r'\|\|'
t_PERCENT = r'%'
t_PERIOD = r'\.'
t_PLUS = r'\+'
t_RBRACE = r'\}'
t_RBRACKET = r'\]'
t_RPAR = r'\)'
t_RSHIFT = r'>>'
t_SEMICOLON = r';'
t_SLASH = r'/'
t_TILDE = r'~'
t_UNDERSCORE = r'_'
t_VBAR = r'\|'

lexer = lex.lex()

if __name__ == "__main__":
    try:
        lex.runmain()
    except lex.LexError, info:
        print "%s: %s..." % (info, info.text[:10])
