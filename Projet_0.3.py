# -*- coding: utf-8 -*-

# Imports

import ply.lex as lex
import ply.yacc as yacc
from genereTreeGraphviz2 import printTreeGraph

# Config

DEBUG = True
DISPLAY_TREE = False

# Lexeur — Tokens

reserved = {
    "print":    "PRINT",
    "if":       "IF",
    "else":     "ELSE",
    "for":      "FOR",
    "while":    "WHILE",
    "function": "FUNC",
    "return":   "RETURN",
}

tokens = [
    "NUMBER",
    "NAME",
    "PLUS",
    "MINUS",
    "TIMES",
    "DIVIDE",
    "MOD",
    "LPAREN",
    "RPAREN",
    "LACC",
    "RACC",
    "SEMI",
    "EGAL",
    "EGALEGAL",
    "INF",
    "INFEG",
    "SUPEGAL",
    "SUP",
    "AND",
    "OR",
    "COMMA",
    "PLUSPLUS",
    "MINUSMINUS",
    "PLUSEQUAL",
    "MINUSEQUAL",
    "TIMESEQUAL",
    "MODEQUAL",
] + list(reserved.values())


# Règles tokens

t_INFEG    = r"\<\="
t_SUPEGAL  = r"\>\="
t_INF      = r"\<"
t_SUP      = r"\>"
t_EGALEGAL = r"\=\="
t_PLUS     = r"\+"
t_MINUS    = r"\-"
t_TIMES    = r"\*"
t_DIVIDE   = r"\/"
t_MOD      = r"\%"
t_LPAREN   = r"\("
t_RPAREN   = r"\)"
t_LACC     = r"\{"
t_RACC     = r"\}"
t_SEMI     = r"\;"
t_EGAL     = r"\="
t_AND      = r"\&\&"
t_OR       = r"\|\|"
t_COMMA    = r"\,"
t_PLUSPLUS = r"\+\+"
t_MINUSMINUS = r"\-\-"
t_PLUSEQUAL = r"\+="
t_MINUSEQUAL = r"\-="
t_TIMESEQUAL = r"\*="
t_MODEQUAL = r"\%="

t_ignore   = " \t"


# Règles

def t_NAME(t):
    r"[a-zA-Z_][a-zA-Z_0-9]*"
    t.type = reserved.get(t.value, "NAME")  # Mots réservés
    return t

def t_NUMBER(t):
    r"\d+"
    t.value = int(t.value)
    return t

def t_newline(t):
    r"\n+"
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print(f"Caractère illégal : '{t.value[0]}'")
    t.lexer.skip(1)

# Parseur — Grammaire

# Tables de symboles
names     = {}  # Variables globales
fonctions = {}  # Fonctions définies
scope_stack = [names]  # Pile de scopes (global + locaux)
return_value = None

# Priorités des opérateurs (du moins au plus prioritaire)
precedence = (
    ("right",    "PLUSEQUAL", "MINUSEQUAL", "TIMESEQUAL", "MODEQUAL"),
    ("left",     "OR"),
    ("left",     "AND"),
    ("nonassoc", "INF", "INFEG", "EGALEGAL", "SUP"),
    ("left",     "PLUS", "MINUS"),
    ("left",     "TIMES", "DIVIDE", "MOD"),
)


# Main

def p_prog(p):
    """prog : prog_item prog
            | prog_item"""
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ("bloc", p[1], p[2])


def p_prog_item(p):
    """prog_item : func
                 | func SEMI
                 | statement SEMI"""
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[1]


def p_func(p):
    """func : FUNC NAME LPAREN params RPAREN LACC bloc RACC"""
    p[0] = ("func", p[2], p[4], p[7], None)

def p_bloc(p):
    """bloc : bloc statement SEMI
            | statement SEMI"""
    # TODO: gérer les ';' après les if/else/for/while
    if len(p) == 4:
        p[0] = ("bloc", p[1], p[2])
    else:
        p[0] = ("bloc", "empty", p[1])

def p_params(p):
    """params : NAME
              | NAME COMMA params
              | empty"""
    if len(p) == 2:
        if p[1] == "empty":
            p[0] = []
        else:
            p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[3]

def p_empty(p):
    "empty :"
    p[0] = "empty"


# Instructions

def p_statement_assign(p):
    "statement : NAME EGAL expression"
    p[0] = ("assign", p[1], p[3])

def p_statement_plusequal(p):
    "statement : NAME PLUSEQUAL expression"
    p[0] = ("assign", p[1], ("+", p[1], p[3]))

def p_statement_minusequal(p):
    "statement : NAME MINUSEQUAL expression"
    p[0] = ("assign", p[1], ("-", p[1], p[3]))

def p_statement_timesequal(p):
    "statement : NAME TIMESEQUAL expression"
    p[0] = ("assign", p[1], ("*", p[1], p[3]))

def p_statement_modequal(p):
    "statement : NAME MODEQUAL expression"
    p[0] = ("assign", p[1], ("%", p[1], p[3]))

def p_statement_plusplus(p):
    """statement : NAME PLUSPLUS
                 | PLUSPLUS NAME"""
    if p[1] == '++':
        p[0] = ("assign", p[2], ("+", p[2], 1))
    else:
        p[0] = ("assign", p[1], ("+", p[1], 1))

def p_statement_minusminus(p):
    """statement : NAME MINUSMINUS
                 | MINUSMINUS NAME"""
    if p[1] == '--':
        p[0] = ("assign", p[2], ("-", p[2], 1))
    else:
        p[0] = ("assign", p[1], ("-", p[1], 1))

def p_args(p):
    """args : expression
            | expression COMMA args
            | empty"""
    if len(p) == 2:
        if p[1] == "empty":
            p[0] = []
        else:
            p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[3]


def p_statement_return(p):
    "statement : RETURN expression"
    p[0] = ("return", p[2])

def p_statement_print(p):
    "statement : PRINT LPAREN expression RPAREN"
    p[0] = ("print", p[3])

def p_statement_if(p):
    "statement : IF LPAREN expression RPAREN LACC bloc RACC"
    p[0] = ("if", p[3], p[6])

def p_statement_if_else(p):
    "statement : IF LPAREN expression RPAREN LACC bloc RACC ELSE LACC bloc RACC"
    p[0] = ("if-else", p[3], p[6], p[10])

def p_statement_while(p):
    "statement : WHILE LPAREN expression RPAREN LACC bloc RACC"
    p[0] = ("while", p[3], p[6])

def p_statement_for(p):
    "statement : FOR LPAREN NAME EGAL expression SEMI expression SEMI NAME EGAL expression RPAREN LACC bloc RACC"
    p[0] = ("for", ("assign", p[3], p[5]), p[7], ("assign", p[9], p[11]), p[14])

def p_statement_expr(p):
    "statement : expression"
    p[0] = p[1]

# Expressions

def p_expression_call(p):
    "expression : NAME LPAREN args RPAREN"
    p[0] = ("call", p[1], p[3])

def p_expression_binop_plus(p):
    "expression : expression PLUS expression"
    p[0] = ("+", p[1], p[3])

def p_expression_binop_minus(p):
    "expression : expression MINUS expression"
    p[0] = ("-", p[1], p[3])

def p_expression_binop_times(p):
    "expression : expression TIMES expression"
    p[0] = ("*", p[1], p[3])

def p_expression_binop_divide(p):
    "expression : expression DIVIDE expression"
    p[0] = ("/", p[1], p[3])

def p_expression_binop_mod(p):
    "expression : expression MOD expression"
    p[0] = ("%", p[1], p[3])

def p_expression_binop_inf(p):
    "expression : expression INF expression"
    p[0] = ("<", p[1], p[3])

def p_expression_binop_infegal(p):
    "expression : expression INFEG expression"
    p[0] = ("<=", p[1], p[3])

def p_expression_binop_supegal(p):
    "expression : expression SUPEGAL expression"
    p[0] = (">=", p[1], p[3])

def p_expression_binop_sup(p):
    "expression : expression SUP expression"
    p[0] = (">", p[1], p[3])

def p_expression_binop_egal(p):
    "expression : expression EGALEGAL expression"
    p[0] = ("==", p[1], p[3])

def p_expression_binop_and(p):
    "expression : expression AND expression"
    p[0] = ("&&", p[1], p[3])

def p_expression_binop_or(p):
    "expression : expression OR expression"
    p[0] = ("||", p[1], p[3])

def p_expression_group(p):
    "expression : LPAREN expression RPAREN"
    p[0] = p[2]

def p_expression_number(p):
    "expression : NUMBER"
    p[0] = p[1]

def p_expression_name(p):
    "expression : NAME"
    p[0] = p[1]

def p_error(p):
    if p:
        print(f"Erreur de syntaxe à '{p.value}' (ligne {p.lineno})")
    else:
        print("Erreur de syntaxe à la fin du fichier")


# Interpréteur — Évaluation

# Évalue une expression et retourne sa valeur.
def evalExpr(expression):
    if isinstance(expression, (int, float)):
        return expression
    if isinstance(expression, str):
        # Chercher la variable dans la pile de scopes (du plus local au plus global)
        for scope in reversed(scope_stack):
            if expression in scope:
                return scope[expression]
        raise NameError(f"Variable non définie : {expression}")
    if isinstance(expression, tuple):
        op = expression[0]
        if op == "call":
            return evalCall(expression[1], expression[2]) # Nom de la fonction et arguments

        gauche = evalExpr(expression[1])
        droite = evalExpr(expression[2])

        if op == "+":
            return gauche + droite
        elif op == "-":
            return gauche - droite
        elif op == "*":
            return gauche * droite
        elif op == "/":
            return gauche / droite
        elif op == "%":
            return gauche % droite
        elif op == "<":
            if gauche < droite:
                return 1
            else:
                return 0
        elif op == "<=":
            if gauche <= droite:
                return 1
            else:
                return 0
        elif op == ">":
            if gauche > droite:
                return 1
            else:
                return 0
        elif op == ">=":
            if gauche >= droite:
                return 1
            else:
                return 0
        elif op == "==":
            if gauche == droite:
                return 1
            else:
                return 0
        elif op == "&&":
            if gauche and droite:
                return 1
            else:
                return 0
        elif op == "||":
            if gauche or droite:
                return 1
            else:
                return 0


# Évalue une instruction (nœud de l'AST).
def evalInst(t):

    global return_value

    if t == "empty":
        return

    noeud = t[0]

    if noeud == "bloc":
        evalInst(t[1])
        if return_value is None:
            evalInst(t[2])

    elif noeud == "assign":
        valeur = evalExpr(t[2])
        scope_stack[-1][t[1]] = valeur
        if DEBUG:
            print(f"la variable {t[1]} = {valeur}")

    elif noeud == "print":
        result = evalExpr(t[1])
        print("calc >", result)


    elif noeud == "if":
        if DEBUG:
            print(f"si {t[1]}\n    alors {t[2]}")
        if evalExpr(t[1]):
            evalInst(t[2])

    elif noeud == "if-else":
        if DEBUG:
            print(f"si {t[1]}\n    alors {t[2]}\n    sinon {t[3]}")
        if evalExpr(t[1]):
            evalInst(t[2])
        else:
            evalInst(t[3])

    elif noeud == "while":
        if DEBUG:
            print(f"tant que {t[1]}\n    faire {t[2]}")
        while evalExpr(t[1]):
            evalInst(t[2])

    elif noeud == "for":
        if DEBUG:
            print(f"pour {t[1]}\n    tant que {t[2]}\n    incrémenter {t[3]}")
        evalInst(t[1])          # initialisation
        while evalExpr(t[2]):   # condition
            evalInst(t[4])      # corps
            evalInst(t[3])      # incrémentation

    elif noeud == "func":
        if DEBUG:
            print(f"Définition de la fonction '{t[1]}'")
        fonctions[t[1]] = (t[2], t[3], t[4])

    elif noeud == "call":
        nom = t[1]
        if nom not in fonctions:
            print(f"Fonction inconnue : {nom}")
            return
        evalCall(t[1], t[2])

    elif noeud == "return":
        return_value = evalExpr(t[1])
        if DEBUG:
            print(f"Retour de la fonction : {return_value}")

def evalCall(nom, args):
    global return_value

    func_data = fonctions[nom]
    params = func_data[0]
    corps = func_data[1]
    return_expr = func_data[2]

    local_scope = {}

    for param, arg in zip(params, args):
        local_scope[param] = evalExpr(arg)

    # Ajouter le scope local à la pile
    scope_stack.append(local_scope)
    old_return = return_value
    return_value = None

    try:
        evalInst(corps)
        if return_value is not None:
            result = return_value
        elif return_expr:
            result = evalExpr(return_expr)
        else:
            result = None
    finally:
        scope_stack.pop()  # Retirer le scope local
        return_value = old_return

    return result


# =============================================================================
lex.lex()
yacc.yacc()
# Tests

# Test 1 : Variables, affectations et expressions
# s = '''
# var1 = 42;
# var2 = 8;
# print(var1 + var2);

# a = 5;
# b = 3;
# print(a * b + 2);
# print((a + b) * 2);
# '''

# Test 2 : Opérateurs arithmétiques
# s = '''
# print(5 + 3);
# print(10 - 2);
# print(4 * 6);
# print(15 / 3);
# print(17 % 5);
# print(5 == 5);
# '''

# Test 2b : Opérateurs logiques
# s = '''
# print(1 && 1);
# print(1 && 0);
# print(0 && 1);
# print(0 && 0);
# print(0 || 0);
# print(1 || 0);
# print(0 || 1);
# print(1 || 1);
# print(10 > 3);
# print(10 >= 10);
# print(5 < 10);
# print(5 <= 5);
# '''

# Test 2c : Opérateurs d'affectation (+=, -=, *=, %=)
# s = '''
# x = 10;
# print(x);
# x += 5;
# print(x);
# x -= 3;
# print(x);
# x *= 2;
# print(x);
# x %= 7;
# print(x);
# z = 20;
# print(z--);
# print(++z);
# '''


# Test 3 : Conditionnelles (si-alors et si-alors-sinon)
# s = '''
# x = 15;
# if (x > 10) {
#     print(1);
# };
# y = 5;
# if (y > 10) {
#     print(100);
# } else {
#     print(200);
# };
# '''

# Test 4 : Boucles (while et for)
# s = '''
# i = 0;
# while (i < 3) {
#     print(i);
#     i = i + 1;
# };
# for (j = 0; j < 5; j = j + 1) {
#     print(j);
# };
# '''

# Test 5 : Fonctions (définition, appel, paramètres, retour)
# s = '''
# function carre(x) { return x * x; };
# function add(a, b) { return a + b; };
# function saluer() { print(42); };

# res = carre(7);
# print(res);
# print(add(3, 4));
# saluer();
# '''

# Test 6 : Récursivité (factorielle)
# s = '''
# function fact(n) {
#     if (n <= 1) {
#         return 1;
#     };
#     return n * fact(n - 1);
# };
# print(fact(5));
# print(fact(10));
# '''

# Test 7a : Portée des variables (globales et locales)
# s = '''
# x = 100;
# function test(y) {
#     z = x + y;
#     return z;
# };
# function getter() {
#     return x;
# };
# print(test(5));
# print(getter());
# x = 20;
# print(getter());
# '''

# Test 7b : Portée des variables (variable dans fonction)
# Erreur attendue : "Variable non définie : z"
# s = '''
# x = 100;
# function test(y) {
#     z = x + y;
#     return z;
# };
# test(5);
# print(z);
# '''


prog = yacc.parse(s)

if DISPLAY_TREE:
    printTreeGraph(prog)
evalInst(prog)
