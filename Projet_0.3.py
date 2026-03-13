# -*- coding: utf-8 -*-

# =============================================================================
# Imports
# =============================================================================

import ply.lex as lex
import ply.yacc as yacc
from genereTreeGraphviz2 import printTreeGraph

# =============================================================================
# Config
# =============================================================================

DEBUG = False

# =============================================================================
# Lexeur — Tokens
# =============================================================================

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
    "SUP",
    "AND",
    "OR",
] + list(reserved.values())


# --- Règles simples (les règles multi-caractères d'abord pour éviter les conflits) ---

t_INFEG    = r"\<\="
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
t_INF      = r"\<"
t_SUP      = r"\>"
t_AND      = r"\&\&"
t_OR       = r"\|\|"

t_ignore   = " \t"


# --- Règles complexes ---

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


# =============================================================================
# Parseur — Grammaire
# =============================================================================

# Tables de symboles
names     = {}  # Variables globales
fonctions = {}  # Fonctions définies

# Priorités des opérateurs (du moins prioritaire au plus prioritaire)
precedence = (
    ("left",     "OR"),
    ("left",     "AND"),
    ("nonassoc", "INF", "INFEG", "EGALEGAL", "SUP"),
    ("left",     "PLUS", "MINUS"),
    ("left",     "TIMES", "DIVIDE", "MOD"),
)


# --- Programme et fonctions ---

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
    """func : FUNC NAME LPAREN RPAREN LACC bloc RACC
            | FUNC NAME LPAREN RPAREN LACC bloc RETURN SEMI RACC"""
    if len(p) == 8:
        p[0] = ("func", p[2], p[6])
    elif len(p) == 10:
        p[0] = ("func", p[2], p[6], "return")


def p_bloc(p):
    """bloc : bloc statement SEMI
            | statement SEMI"""
    # TODO: gérer les ';' après les if/else/for/while
    if len(p) == 4:
        p[0] = ("bloc", p[1], p[2])
    else:
        p[0] = ("bloc", "empty", p[1])


# --- Instructions ---

def p_statement_assign(p):
    "statement : NAME EGAL expression"
    p[0] = ("assign", p[1], p[3])


def p_statement_call(p):
    "statement : NAME LPAREN RPAREN"
    p[0] = ("call", p[1])


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


# --- Expressions ---

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


# =============================================================================
# Interpréteur — Évaluation
# =============================================================================

# Évalue une expression et retourne sa valeur.
def evalExpr(expression):
    if isinstance(expression, (int, float)):
        return expression
    if isinstance(expression, str):
        return names[expression]
    if isinstance(expression, tuple):
        op = expression[0]
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
            return gauche < droite
        elif op == "<=":
            return gauche <= droite
        elif op == ">":
            return gauche > droite
        elif op == "==":
            return gauche == droite
        elif op == "&&":
            return gauche and droite
        elif op == "||":
            return gauche or droite


# Évalue une instruction (nœud de l'AST).
def evalInst(t):
    if t == "empty":
        return

    noeud = t[0]

    if noeud == "bloc":
        evalInst(t[1])
        evalInst(t[2])

    elif noeud == "assign":
        valeur = evalExpr(t[2])
        if DEBUG:
            print(f"calc > la variable {t[1]} = {valeur}")
        names[t[1]] = valeur

    elif noeud == "print":
        result = evalExpr(t[1])
        if DEBUG:
            print("calc >", result)
        else:
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
        fonctions[t[1]] = t[2]

    elif noeud == "call":
        nom = t[1]
        if nom not in fonctions:
            print(f"Fonction inconnue : {nom}")
            return
        evalInst(fonctions[nom])


# =============================================================================
# Point d'entrée
# =============================================================================

lex.lex()
yacc.yacc()

# s = 'print(1+2/5-2); print(1+2+1);'
# s = 'if (2+2 <= 4) { print(1); }'
# s = '''
# for (i=0; i<=10; i=i+1) {
#     if (i%2 == 0) {
#         print(i);
#     };
# };
# '''
s = '''

    for (i=0; i<10; i=i+1) {
        if (i%2 == 0) {
            print(i);
        };
    };

'''


prog = yacc.parse(s)
printTreeGraph(prog)
evalInst(prog)
