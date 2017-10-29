
import sys, os, yacc
import lexer

from lexer import tokens

precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'ASTERISK', 'SLASH')
)

def p_tl(t):
    'tl : definitions'
    t[0] = ('tl', t[1])

def p_definitions_empty(t):
    'definitions :'
    t[0] = []
    
def p_definitions(t):
    'definitions : definitions definition'
    t[0] = t[1] + [t[2]]

def p_definition(t):
    'definition : type_expr IDENTIFIER LPAR arg_decl_list RPAR code_block'
    t[0] = ('function', t[2], t[4], t[6])

def p_type_expr(t):
    '''type_expr : INT
                 | STRING
                 | LIST''' 
    t[0] = ('type_expr', t[1])

def p_arg_decl_list_empty(t):
    'arg_decl_list : arg_decl'
    t[0] = [t[1]]

def p_arg_decl_list(t):
    'arg_decl_list : arg_decl_list COMMA arg_decl'
    t[0] = t[1] + [t[3]]

def p_arg_decl(t):
    'arg_decl : type_expr IDENTIFIER'
    t[0] = ('arg_decl', t[1], t[2])

def p_code_block(t):
    'code_block : LBRACE statements RBRACE'
    t[0] = ('code_block', t[2])

def p_statements_empty(t):
    'statements :'
    t[0] = []
    
def p_statements(t):
    'statements : statements statement'
    t[0] = t[1] + [t[2]]

def p_statement(t):
    'statement : expr'
    t[0] = ('stmnt', t[1])

def p_expr_plus(t):
    'expr : expr PLUS expr'
    t[0] = t[1] + t[3]

def p_expr_minus(t):
    'expr : expr MINUS expr'
    t[0] = t[1] - t[3]

def p_expr_times(t):
    'expr : expr ASTERISK expr'
    t[0] = t[1] * t[3]

def p_expr_divide(t):
    'expr : expr SLASH expr'
    t[0] = t[1] / t[3]

def p_expr_paren(t):
    'expr : LPAR expr RPAR'
    t[0] = t[2]

def p_expr_int(t):
    'expr : INTEGER'
    t[0] = t[1]

# Error rule for syntax errors
def p_error(t):
    print "Syntax error in input!"
    print t.type
    print t.lineno
    
parser = yacc.yacc()

if __name__ == '__main__':
    f = open(sys.argv[1], "r")
    s = f.read()
    f.close()
    print yacc.parse(s, debug = 0)
