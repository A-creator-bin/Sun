#ifndef LEXER_H
#define LEXER_H
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

/* =================== Diagnóstico =================== */
typedef enum { ERR_NONE, ERR_LEX, ERR_PARSE, ERR_RUNTIME } ErrKind;

typedef struct {
    ErrKind kind;
    int line;
    int col;
    char msg[1024];
} Error;

static Error g_error = { ERR_NONE, 0, 0, "" };

static void set_error(ErrKind kind, int line, int col, const char* fmt, ...) {
    if (g_error.kind != ERR_NONE) return; // mantém o primeiro erro
    g_error.kind = kind;
    g_error.line = line;
    g_error.col  = col;
    va_list ap; va_start(ap, fmt);
    vsnprintf(g_error.msg, sizeof(g_error.msg), fmt, ap);
    va_end(ap);
}

static void clear_error(void) { g_error.kind = ERR_NONE; g_error.msg[0] = 0; }

static void print_error_and_flush(const char* phase) {
    fprintf(stderr, "[%s error] line %d, col %d: %s\n",
            phase, g_error.line, g_error.col, g_error.msg);
}

/* =================== Tokens =================== */
typedef enum {
    T_EOF = 0,
    T_UNKNOWN,
    T_NUMBER,
    T_STRING,
    T_IDENTIFIER,

    // símbolos
    T_PLUS, T_MINUS, T_STAR, T_SLASH,
    T_ASSIGN,            // =
    T_EQ, T_NE,          // == !=
    T_LT, T_LE, T_GT, T_GE, // < <= > >=
    T_LPAREN, T_RPAREN,  // ( )
    T_LBRACE, T_RBRACE,  // { }
    T_SEMI, T_COMMA,     // ; ,

    // lógicos
    T_AND, T_OR, T_NOT,  // && || !

    // palavras-chave
    T_KW_IF, T_KW_ELSE, T_KW_WHILE, T_KW_PRINT, T_KW_INPUT
} TokenType;

typedef struct {
    TokenType type;
    char lexeme[MAX_TOKEN_LENGTH];
    int line;
    int col;
} Token;

typedef struct {
    Token data[MAX_TOKENS];
    int count;
} TokenVec;

/* =================== Lexer =================== */
typedef struct {
    const char* src;
    int i;
    int line;
    int col;
    TokenVec* out;
} Lexer;

static int lex_peek(Lexer* L) { return L->src[L->i]; }
static int lex_peek2(Lexer* L) { return L->src[L->i+1]; }
static int lex_advance(Lexer* L) {
    int c = L->src[L->i++];
    if (c == '\n') { L->line++; L->col = 1; } else { L->col++; }
    return c;
}
static void emit(Lexer* L, TokenType t, const char* lex, int line, int col) {
    if (L->out->count >= MAX_TOKENS) {
        set_error(ERR_LEX, line, col, "token buffer overflow");
        return;
    }
    Token* tk = &L->out->data[L->out->count++];
    tk->type = t;
    tk->line = line;
    tk->col  = col;
    if (lex) strncpy(tk->lexeme, lex, MAX_TOKEN_LENGTH-1), tk->lexeme[MAX_TOKEN_LENGTH-1]=0;
    else tk->lexeme[0]=0;
}

static int is_ident_start(int c){ return isalpha(c) || c=='_'; }
static int is_ident_part (int c){ return isalnum(c) || c=='_'; }

static int match(Lexer* L, int expected) {
    if (lex_peek(L) == expected) { lex_advance(L); return 1; }
    return 0;
}

static void lex_all(const char* src, TokenVec* out) {
    Lexer L = { src, 0, 1, 1, out };
    out->count = 0;
    clear_error();

    while (lex_peek(&L)) {
        int c = lex_peek(&L);
        if (isspace(c)) { lex_advance(&L); continue; }

        int line = L.line, col = L.col;

        // números
        if (isdigit(c)) {
            char buf[MAX_TOKEN_LENGTH]; int j=0;
            while (isdigit(lex_peek(&L))) {
                if (j < MAX_TOKEN_LENGTH-1) buf[j++] = lex_advance(&L); else lex_advance(&L);
            }
            buf[j]=0;
            emit(&L, T_NUMBER, buf, line, col);
            if (g_error.kind) return;
            continue;
        }

        // identificadores / palavras-chave
        if (is_ident_start(c)) {
            char buf[MAX_TOKEN_LENGTH]; int j=0;
            while (is_ident_part(lex_peek(&L))) {
                if (j < MAX_TOKEN_LENGTH-1) buf[j++] = lex_advance(&L); else lex_advance(&L);
            }
            buf[j]=0;
            TokenType t = T_IDENTIFIER;
            if      (strcmp(buf,"if")==0)    t=T_KW_IF;
            else if (strcmp(buf,"else")==0)  t=T_KW_ELSE;
            else if (strcmp(buf,"loop")==0) t=T_KW_WHILE;
            else if (strcmp(buf,"output")==0) t=T_KW_PRINT;
            else if (strcmp(buf,"input")==0) t=T_KW_INPUT;
            emit(&L, t, buf, line, col);
            if (g_error.kind) return;
            continue;
        }

        // strings
        if (c=='"') {
            lex_advance(&L);
            char buf[512]; int j=0;
            int escaped = 0;
            while (lex_peek(&L)) {
                int ch = lex_advance(&L);
                if (!escaped && ch=='"') break;
                if (!escaped && ch=='\\') { escaped=1; continue; }
                if (escaped) {
                    if      (ch=='n') ch='\n';
                    else if (ch=='t') ch='\t';
                    else if (ch=='r') ch='\r';
                    // caso contrário, mantém o char
                    escaped=0;
                }
                if (j< (int)sizeof(buf)-1) buf[j++]= (char)ch;
            }
            if (L.src[L.i-1] != '"') {
                set_error(ERR_LEX, line, col, "string no closure");
                return;
            }
            buf[j]=0;
            emit(&L, T_STRING, buf, line, col);
            if (g_error.kind) return;
            continue;
        }

        // operadores e pontuação
        switch (c) {
            case '+': lex_advance(&L); emit(&L, T_PLUS, "+", line, col); break;
            case '-': lex_advance(&L); emit(&L, T_MINUS,"-", line, col); break;
            case '*': lex_advance(&L); emit(&L, T_STAR, "*", line, col); break;
            case '/': lex_advance(&L); emit(&L, T_SLASH,"/", line, col); break;
            case '!':
                lex_advance(&L);
                if (match(&L,'=')) emit(&L, T_NE, "!=", line, col);
                else emit(&L, T_NOT, "!", line, col);
                break;
            case '=':
                lex_advance(&L);
                if (match(&L,'=')) emit(&L, T_EQ, "==", line, col);
                else emit(&L, T_ASSIGN, "=", line, col);
                break;
            case '<':
                lex_advance(&L);
                if (match(&L,'=')) emit(&L, T_LE, "<=", line, col);
                else emit(&L, T_LT, "<", line, col);
                break;
            case '>':
                lex_advance(&L);
                if (match(&L,'=')) emit(&L, T_GE, ">=", line, col);
                else emit(&L, T_GT, ">", line, col);
                break;
            case '&':
                lex_advance(&L);
                if (match(&L,'&')) emit(&L, T_AND, "&&", line, col);
                else { set_error(ERR_LEX, line, col, "expected '&' to '&&'"); return; }
                break;
            case '|':
                lex_advance(&L);
                if (match(&L,'|')) emit(&L, T_OR, "||", line, col);
                else { set_error(ERR_LEX, line, col, "expected'|' to'||'"); return; }
                break;
            case '(': lex_advance(&L); emit(&L, T_LPAREN,"(", line, col); break;
            case ')': lex_advance(&L); emit(&L, T_RPAREN,")", line, col); break;
            case '{': lex_advance(&L); emit(&L, T_LBRACE,"{", line, col); break;
            case '}': lex_advance(&L); emit(&L, T_RBRACE,"}", line, col); break;
            case ';': lex_advance(&L); emit(&L, T_SEMI,  ";", line, col); break;
            case ',': lex_advance(&L); emit(&L, T_COMMA, ",", line, col); break;
            default:
                set_error(ERR_LEX, line, col, "unknown caracter'%c'", c);
                return;
        }
        if (g_error.kind) return;
    }
    emit(&L, T_EOF, "", L.line, L.col);
}

#endif
