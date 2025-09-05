#ifndef PARSER_H
#define PARSER_H
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>  

/* =================== Config =================== */
#define MAX_TOKEN_LENGTH 128
#define MAX_TOKENS       2048
#define MAX_VARS         512
#define MAX_LINE         2048

/* =================== AST =================== */
typedef enum {
    N_INT, N_STRING, N_VAR,
    N_UNARY,      // op, left
    N_BINARY,     // op, left, right
    N_ASSIGN,     // var-name em value, left=expr
    N_PRINT,      // extra = lista ligada por right
    N_INPUT,      // value = nome var
    N_IF,         // left=cond, extra=then, right=else?
    N_WHILE,      // left=cond, extra=body
    N_BLOCK       // extra = primeiro stmt; encadeado via right
} NodeType;

typedef enum {
    OP_PLUS, OP_MINUS, OP_MUL, OP_DIV,
    OP_EQ, OP_NE, OP_LT, OP_LE, OP_GT, OP_GE,
    OP_AND, OP_OR, OP_NOT,
} OpType;

typedef struct Node {
    NodeType type;
    OpType   op;
    char value[128]; // número em texto, string, ou nome de variável
    struct Node* left;
    struct Node* right;
    struct Node* extra;
    int line, col;
} Node;

static Node* node_new(NodeType t, int line, int col) {
    Node* n = (Node*)calloc(1, sizeof(Node));
    n->type = t;
    n->line = line;
    n->col  = col;
    return n;
}

static void node_free(Node* n) {
    if (!n) return;
    node_free(n->left);
    node_free(n->right);
    node_free(n->extra);
    free(n);
}

/* =================== Parser =================== */
typedef struct {
    Token* toks;
    int pos;
    int count;
} Parser;

static Token* P_peek(Parser* P) {
    if (P->pos < P->count) return &P->toks[P->pos];
    return &P->toks[P->count-1];
}
static Token* P_prev(Parser* P) {
    if (P->pos>0) return &P->toks[P->pos-1];
    return &P->toks[0];
}
static int P_match(Parser* P, TokenType t) {
    if (P_peek(P)->type == t) { P->pos++; return 1; }
    return 0;
}
static Token* P_consume(Parser* P, TokenType t, const char* msg) {
    Token* tk = P_peek(P);
    if (tk->type == t) { P->pos++; return tk; }
    set_error(ERR_PARSE, tk->line, tk->col, "%s (i found '%s')", msg, tk->lexeme);
    return NULL;
}

/* Forward decls */
static Node* parse_statement(Parser* P);
static Node* parse_block(Parser* P);
static Node* parse_expression(Parser* P);

/* precedência: || -> && -> igualdade -> relacional -> aditivo -> multiplicativo -> unário -> primário */
static Node* parse_primary(Parser* P) {
    Token* tk = P_peek(P);
    if (tk->type == T_NUMBER) {
        P->pos++;
        Node* n = node_new(N_INT, tk->line, tk->col);
        strncpy(n->value, tk->lexeme, sizeof(n->value)-1);
        return n;
    }
    if (tk->type == T_STRING) {
        P->pos++;
        Node* n = node_new(N_STRING, tk->line, tk->col);
        strncpy(n->value, tk->lexeme, sizeof(n->value)-1);
        return n;
    }
    if (tk->type == T_IDENTIFIER) {
        P->pos++;
        Node* n = node_new(N_VAR, tk->line, tk->col);
        strncpy(n->value, tk->lexeme, sizeof(n->value)-1);
        return n;
    }
    if (P_match(P, T_LPAREN)) {
        Node* e = parse_expression(P);
        if (!e) return NULL;
        if (!P_consume(P, T_RPAREN, "expected ')'")) { node_free(e); return NULL; }
        return e;
    }
    set_error(ERR_PARSE, tk->line, tk->col, "invalid primary expression");
    return NULL;
}

static Node* make_unary(OpType op, Node* a, int line, int col) {
    Node* n = node_new(N_UNARY, line, col); n->op = op; n->left = a; return n;
}
static Node* make_binary(OpType op, Node* a, Node* b, int line, int col) {
    Node* n = node_new(N_BINARY, line, col); n->op = op; n->left = a; n->right = b; return n;
}

static Node* parse_unary(Parser* P) {
    Token* tk = P_peek(P);
    if (tk->type == T_NOT || tk->type == T_MINUS || tk->type == T_PLUS) {
        P->pos++;
        Node* rhs = parse_unary(P);
        if (!rhs) return NULL;
        OpType op = (tk->type==T_NOT)? OP_NOT : (tk->type==T_MINUS? OP_MINUS : OP_PLUS);
        return make_unary(op, rhs, tk->line, tk->col);
    }
    return parse_primary(P);
}

static Node* parse_mul(Parser* P) {
    Node* left = parse_unary(P);
    if (!left) return NULL;
    for (;;) {
        Token* tk = P_peek(P);
        if (tk->type == T_STAR || tk->type == T_SLASH) {
            P->pos++;
            Node* right = parse_unary(P);
            if (!right) { node_free(left); return NULL; }
            left = make_binary(tk->type==T_STAR?OP_MUL:OP_DIV, left, right, tk->line, tk->col);
        } else break;
    }
    return left;
}

static Node* parse_add(Parser* P) {
    Node* left = parse_mul(P);
    if (!left) return NULL;
    for (;;) {
        Token* tk = P_peek(P);
        if (tk->type == T_PLUS || tk->type == T_MINUS) {
            P->pos++;
            Node* right = parse_mul(P);
            if (!right) { node_free(left); return NULL; }
            left = make_binary(tk->type==T_PLUS?OP_PLUS:OP_MINUS, left, right, tk->line, tk->col);
        } else break;
    }
    return left;
}

static Node* parse_rel(Parser* P) {
    Node* left = parse_add(P);
    if (!left) return NULL;
    for (;;) {
        Token* tk = P_peek(P);
        OpType op;
        int match = 1;
        switch (tk->type) {
            case T_LT: op=OP_LT; break;
            case T_LE: op=OP_LE; break;
            case T_GT: op=OP_GT; break;
            case T_GE: op=OP_GE; break;
            default: match=0; break;
        }
        if (!match) break;
        P->pos++;
        Node* right = parse_add(P);
        if (!right) { node_free(left); return NULL; }
        left = make_binary(op, left, right, tk->line, tk->col);
    }
    return left;
}

static Node* parse_eq(Parser* P) {
    Node* left = parse_rel(P);
    if (!left) return NULL;
    for (;;) {
        Token* tk = P_peek(P);
        if (tk->type == T_EQ || tk->type == T_NE) {
            P->pos++;
            Node* right = parse_rel(P);
            if (!right) { node_free(left); return NULL; }
            left = make_binary(tk->type==T_EQ?OP_EQ:OP_NE, left, right, tk->line, tk->col);
        } else break;
    }
    return left;
}

static Node* parse_and(Parser* P) {
    Node* left = parse_eq(P);
    if (!left) return NULL;
    for (;;) {
        Token* tk = P_peek(P);
        if (tk->type == T_AND) {
            P->pos++;
            Node* right = parse_eq(P);
            if (!right) { node_free(left); return NULL; }
            left = make_binary(OP_AND, left, right, tk->line, tk->col);
        } else break;
    }
    return left;
}

static Node* parse_or(Parser* P) {
    Node* left = parse_and(P);
    if (!left) return NULL;
    for (;;) {
        Token* tk = P_peek(P);
        if (tk->type == T_OR) {
            P->pos++;
            Node* right = parse_and(P);
            if (!right) { node_free(left); return NULL; }
            left = make_binary(OP_OR, left, right, tk->line, tk->col);
        } else break;
    }
    return left;
}

static Node* parse_expression(Parser* P) { return parse_or(P); }

static Node* parse_assignment_or_expr_stmt(Parser* P) {
    // lookahead para "IDENT = ..."
    if (P_peek(P)->type == T_IDENTIFIER && P->toks[P->pos+1].type == T_ASSIGN) {
        Token* id = P_consume(P, T_IDENTIFIER, "expected indentifier");
        if (!id) return NULL;
        P_consume(P, T_ASSIGN, "expected '='");
        Node* expr = parse_expression(P);
        if (!expr) return NULL;
        Node* n = node_new(N_ASSIGN, id->line, id->col);
        strncpy(n->value, id->lexeme, sizeof(n->value)-1);
        n->left = expr;
        P_consume(P, T_SEMI, "expected';' after assignment");
        return n;
    } else {
        // expressão solta (ex.: função que não retorna? aqui só avaliamos e descartamos)
        Node* e = parse_expression(P);
        if (!e) return NULL;
        P_consume(P, T_SEMI, "expected';' after expression");
        return e; // como "stmt expr;" (sem efeito prático)
    }
}

static Node* parse_print(Parser* P) {
    Token* kw = P_consume(P, T_KW_PRINT, "expected 'output'");
    if (!kw) return NULL;
    P_consume(P, T_LPAREN, "expected'(' after 'output'");
    Node* first=NULL, *prev=NULL;
    if (P_peek(P)->type != T_RPAREN) {
        for (;;) {
            Node* e = parse_expression(P);
            if (!e) { node_free(first); return NULL; }
            if (!first) first=e; else prev->right=e;
            prev = e;
            if (P_match(P, T_COMMA)) continue;
            break;
        }
    }
    P_consume(P, T_RPAREN, "expected')'");
    P_consume(P, T_SEMI,   "expected ';'  after 'output(...)'");
    Node* n = node_new(N_PRINT, kw->line, kw->col);
    n->extra = first;
    return n;
}

static Node* parse_input(Parser* P) {
    Token* kw = P_consume(P, T_KW_INPUT, "expected'input'");
    if (!kw) return NULL;
    P_consume(P, T_LPAREN, "expected '(' after 'input'");
    Token* id = P_consume(P, T_IDENTIFIER, "expected indentifier ininput(var)");
    if (!id) return NULL;
    P_consume(P, T_RPAREN, "expected ')'");
    P_consume(P, T_SEMI,   "expected after 'input(...)'");
    Node* n = node_new(N_INPUT, kw->line, kw->col);
    strncpy(n->value, id->lexeme, sizeof(n->value)-1);
    return n;
}

static Node* parse_if(Parser* P) {
    Token* kw = P_consume(P, T_KW_IF, "expected 'if'");
    if (!kw) return NULL;
    P_consume(P, T_LPAREN, "expected after '(' após 'if'");
    Node* cond = parse_expression(P);
    if (!cond) return NULL;
    P_consume(P, T_RPAREN, "expected ')'");
    Node* thenB = parse_block(P);
    if (!thenB) { node_free(cond); return NULL; }
    Node* elseB = NULL;
    if (P_match(P, T_KW_ELSE)) {
        elseB = parse_block(P);
        if (!elseB) { node_free(cond); node_free(thenB); return NULL; }
    }
    Node* n = node_new(N_IF, kw->line, kw->col);
    n->left = cond; n->extra = thenB; n->right = elseB;
    return n;
}

static Node* parse_while(Parser* P) {
    Token* kw = P_consume(P, T_KW_WHILE, "expected 'loop'");
    if (!kw) return NULL;
    P_consume(P, T_LPAREN, "expected '(' after 'loop'");
    Node* cond = parse_expression(P);
    if (!cond) return NULL;
    P_consume(P, T_RPAREN, "expected ')'");
    Node* body = parse_block(P);
    if (!body) { node_free(cond); return NULL; }
    Node* n = node_new(N_WHILE, kw->line, kw->col);
    n->left = cond; n->extra = body;
    return n;
}

static Node* parse_statement(Parser* P) {
    Token* tk = P_peek(P);
    switch (tk->type) {
        case T_KW_PRINT: return parse_print(P);
        case T_KW_INPUT: return parse_input(P);
        case T_KW_IF:    return parse_if(P);
        case T_KW_WHILE: return parse_while(P);
        case T_LBRACE:   return parse_block(P);
        default:         return parse_assignment_or_expr_stmt(P);
    }
}

static Node* parse_block(Parser* P) {
    if (!P_match(P, T_LBRACE)) {
        // bloco reduzido a um único statement (permitimos 'stmt' sem chaves)
        return parse_statement(P);
    }
    Node* first=NULL, *prev=NULL;
    while (P_peek(P)->type != T_RBRACE && P_peek(P)->type != T_EOF) {
        Node* s = parse_statement(P);
        if (!s) { node_free(first); return NULL; }
        if (!first) first=s; else prev->right=s;
        prev=s;
    }
    if (!P_consume(P, T_RBRACE, "expected '}' to close block")) { node_free(first); return NULL; }
    Node* blk = node_new(N_BLOCK, P_prev(P)->line, P_prev(P)->col);
    blk->extra = first;
    return blk;
}
#endif
