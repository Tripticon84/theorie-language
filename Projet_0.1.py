# -*- coding: utf-8 -*-
from genereTreeGraphviz2 import printTreeGraph


reserved={
        'print':'PRINT',
        'if':'IF',
        'else':'ELSE',
        'for':'FOR',
        'while':'WHILE'
    }

tokens = [ 'NUMBER','MINUS', 'PLUS','TIMES','DIVIDE', 'LPAREN',
          'RPAREN', 'OR', 'AND', 'SEMI', 'EGAL', 'NAME', 'INF', 'SUP',
          'EGALEGAL','INFEG', 'LACC', 'RACC', 'MOD' ]+ list(reserved.values())

t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_OR = r'\|'
t_AND = r'\&'
t_SEMI = r';'
t_EGAL = r'\='
#t_NAME = r'[a-zA-Z_][a-zA-Z_0-9]*'
t_INF = r'\<'
t_SUP = r'>'
t_INFEG = r'\<\='
t_EGALEGAL = r'\=\='
t_LACC =  r'\{'
t_RACC = r'\}'
t_MOD = r'\%'

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'NAME')    # Check for reserved words
    return t


def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

import ply.lex as lex
lex.lex()

names={}
precedence = (
        ('left','OR' ),
        ('left','AND'),
        ('nonassoc', 'INF', 'INFEG', 'EGALEGAL', 'SUP'),
        ('left','PLUS', 'MINUS' ),
        ('left','TIMES', 'DIVIDE', 'MOD'),
        )

def p_start(p):
    'start : bloc'
    print(p[1])
    # printTreeGraph(p[1])
    evalInst(p[1])

def p_bloc(p):
    '''bloc : bloc statement SEMI
    | statement SEMI'''
   # | LACC bloc statement RACC''' # gérer les ';' après les if/else/for/while
    if len(p)==4  :
        p[0] = ('bloc', p[1], p[2])
    else :
        p[0] = ('bloc', 'empty', p[1])

def p_statement_if(p):
    'statement : IF LPAREN expression RPAREN LACC bloc RACC'
    p[0] = ('if', p[3], p[6])

def p_statement_if_else(p):
    'statement : IF LPAREN expression RPAREN LACC bloc RACC ELSE LACC bloc RACC'
    p[0] = ('if-else', p[3], p[6], p[10])

def p_statement_expr(p):
    'statement : PRINT LPAREN expression RPAREN'
    p[0] = ('print', p[3])

def p_statement_assign(p):
    'statement : NAME EGAL expression'
    p[0] = ('assign', p[1], p[3])

def p_statement_for(p):
    'statement : FOR LPAREN NAME EGAL expression SEMI expression SEMI NAME EGAL expression RPAREN LACC bloc RACC'
    p[0] = ('for', ('assign', p[3], p[5]), p[7], ('assign', p[9], p[11]), p[14])

def p_expression_binop_inf(p):
    'expression : expression INF expression'
    p[0] = ('<', p[1], p[3])

def p_expression_binop_infEGAL(p):
    'expression : expression INFEG expression'
    p[0] = ('<=', p[1], p[3])

def p_expression_binop_sup(p):
    'expression : expression SUP expression'
    p[0] = ('>', p[1], p[3])

def p_expression_binop_egal(p):
    'expression : expression EGALEGAL expression'
    p[0] = ('==', p[1], p[3])

def p_expression_binop_and(p):
    'expression : expression AND expression'
    p[0] = ('&&', p[1], p[3])

def p_expression_binop_or(p):
    'expression : expression OR expression'
    p[0] = ('||', p[1], p[3])

def p_expression_binop_plus(p):
    'expression : expression PLUS expression'
    p[0] = ('+', p[1], p[3])

def p_expression_binop_times(p):
    'expression : expression TIMES expression'
    p[0] = ('*', p[1], p[3])

def p_expression_binop_minus(p):
    'expression : expression MINUS expression'
    p[0] = ('-', p[1], p[3])

def p_expression_binop_divide(p):
    'expression : expression DIVIDE expression'
    p[0] = ('/', p[1], p[3])

def p_expression_binop_mod(p):
    'expression : expression MOD expression'
    p[0] = ('%', p[1], p[3])

def p_expression_group(p):
    'expression : LPAREN expression RPAREN'
    p[0] = p[2]

def p_expression_number(p):
    'expression : NUMBER'
    p[0] = p[1]

def p_expression_name(p):
    'expression : NAME'
    p[0] = p[1]

def p_error(p):
    if p:
        print(f"Syntax error at '{p.value}' (line {p.lineno})")
    else:
        print("Syntax error at EOF")

def evalExpr(expression):
    # print("evalExpr de", expression)
    if type(expression) is int or type(expression) is float:
        return expression
    if type(expression) is str:
        return names[expression]
    if type(expression) is tuple:
        operateur = expression[0]
        if operateur == '+':
            return evalExpr(expression[1]) + evalExpr(expression[2])
        if operateur == '*':
            return evalExpr(expression[1]) * evalExpr(expression[2])
        if operateur == '-':
            return evalExpr(expression[1]) - evalExpr(expression[2])
        if operateur == '/':
            return evalExpr(expression[1]) / evalExpr(expression[2])
        if operateur == '<':
            return evalExpr(expression[1]) < evalExpr(expression[2])
        if operateur == '<=':
            return evalExpr(expression[1]) <= evalExpr(expression[2])
        if operateur == '>':
            return evalExpr(expression[1]) > evalExpr(expression[2])
        if operateur == '==':
            return evalExpr(expression[1]) == evalExpr(expression[2])
        if operateur == '&&':
            return evalExpr(expression[1]) and evalExpr(expression[2])
        if operateur == '||':
            return evalExpr(expression[1]) or evalExpr(expression[2])
        if operateur == '%':
            return evalExpr(expression[1]) % evalExpr(expression[2])

def evalInst(t):
    # print("evalInst de", t)
    if t == 'empty': return
    if t[0] == 'bloc':
        evalInst(t[1])
        evalInst(t[2])
    elif t[0] == 'print':
        valeur = evalExpr(t[1])
        print(">>", valeur)
    elif t[0] == 'assign':
        print("la variable ", t[1], "=", evalExpr(t[2]))
        nom_variable = t[1]
        valeur = evalExpr(t[2])
        names[nom_variable] = valeur
    elif t[0] == 'if':
        print("si, ", t[1], "\n    alors ", t[2])
        condition = evalExpr(t[1])
        if (condition): evalInst(t[2])
    elif t[0] == 'if-else':
        print("si, ", t[1], "\n    alors ", t[2], "\n    sinon ", t[3])
        condition = evalExpr(t[1])
        if (condition): evalInst(t[2])
        else: evalInst(t[3])
    elif t[0] == 'while':
        print("tant que, ", t[1], "\n    faire ", t[2])
        condition = evalExpr(t[1])
        while (condition):
            evalInst(t[2])
            condition = evalExpr(t[1])
    elif t[0] == 'for':
        print("pour, ", t[1], "\n    tant que ", t[2], "\n    incrémenter ", t[3])
        initialisation = t[1]
        condition = t[2]
        incrementation = t[3]
        corps = t[4]
        evalInst(initialisation)
        while evalExpr(condition):
            evalInst(corps)
            evalInst(incrementation)


import ply.yacc as yacc
yacc.yacc()
# s = 'print(1+2/5-2);print(1+2+1);'
# s = 'if (2+2 <= 4) {print(1);}'
s = '''
for (i=0; i<10; i=i+1) {
    if (i%2 == 0) {
        print(i);
    } else {
        print(i*2);
    };
};
'''
# s = 'for (i=0; i<10; i=i+1) {print(i);};'
yacc.parse(s)
