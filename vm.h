#ifndef VM_H
#define VM_H
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

typedef enum { V_INT, V_STRING } ValType;

typedef struct {
    ValType type;
    int i;
    char s[512];
} Value;

typedef struct { char name[64]; Value val; } Var;
static Var g_vars[MAX_VARS]; static int g_varc=0;

static Var* var_find(const char* name) {
    for (int i=0;i<g_varc;i++) if (strcmp(g_vars[i].name, name)==0) return &g_vars[i];
    return NULL;
}
static Var* var_ensure(const char* name) {
    Var* v = var_find(name);
    if (v) return v;
    if (g_varc>=MAX_VARS) return NULL;
    strncpy(g_vars[g_varc].name, name, sizeof(g_vars[g_varc].name)-1);
    g_vars[g_varc].name[sizeof(g_vars[g_varc].name)-1]=0;
    g_vars[g_varc].val.type=V_INT; g_vars[g_varc].val.i=0; g_vars[g_varc].val.s[0]=0;
    return &g_vars[g_varc++];
}

static int truthy(Value v) {
    if (v.type==V_INT) return v.i!=0;
    return v.s[0]!=0;
}

static void print_value(Value v) {
    if (v.type==V_INT) printf("%d", v.i);
    else printf("%s", v.s);
}

static Value V_int(int x){ Value v; v.type=V_INT; v.i=x; v.s[0]=0; return v; }
static Value V_str(const char* s){ Value v; v.type=V_STRING; v.i=0; strncpy(v.s,s,sizeof(v.s)-1); v.s[sizeof(v.s)-1]=0; return v; }

static Value eval(Node* n); // fwd
static void exec_block(Node* n);

/* Helpers de runtime para tipos */
static Value bin_num_num(Node* n, Value a, Value b, OpType op) {
    if (a.type!=V_INT || b.type!=V_INT) {
        set_error(ERR_RUNTIME, n->line, n->col, "operator aritimatic is not int");
        return V_int(0);
    }
    switch (op) {
        case OP_PLUS:  return V_int(a.i + b.i);
        case OP_MINUS: return V_int(a.i - b.i);
        case OP_MUL:   return V_int(a.i * b.i);
        case OP_DIV:
            if (b.i==0) { set_error(ERR_RUNTIME, n->line, n->col, "division by zero"); return V_int(0); }
            return V_int(a.i / b.i);
        default: return V_int(0);
    }
}

static Value cmp_any(Node* n, Value a, Value b, OpType op) {
    // Comparações: se ambos inteiros, compare numericamente; se ambos strings, lexicográfico; senão, erro.
    if (a.type==V_INT && b.type==V_INT) {
        int A=a.i, B=b.i, r=0;
        switch (op){
            case OP_EQ: r=(A==B); break; case OP_NE: r=(A!=B); break;
            case OP_LT: r=(A< B); break; case OP_LE: r=(A<=B); break;
            case OP_GT: r=(A> B); break; case OP_GE: r=(A>=B); break;
            default: r=0; break;
        }
        return V_int(r);
    }
    if (a.type==V_STRING && b.type==V_STRING) {
        int cmp = strcmp(a.s, b.s), r=0;
        switch (op){
            case OP_EQ: r=(cmp==0); break; case OP_NE: r=(cmp!=0); break;
            case OP_LT: r=(cmp< 0); break; case OP_LE: r=(cmp<=0); break;
            case OP_GT: r=(cmp> 0); break; case OP_GE: r=(cmp>=0); break;
            default: r=0; break;
        }
        return V_int(r);
    }
    set_error(ERR_RUNTIME, n->line, n->col, "incompatibles");
    return V_int(0);
}

static Value add_any(Node* n, Value a, Value b) {
    // Se ambos int -> soma; se qualquer é string -> concatena (coerção simples para int->string)
    if (a.type==V_INT && b.type==V_INT) return V_int(a.i + b.i);
    char buf[1024]; buf[0]=0;
    if (a.type==V_STRING) snprintf(buf, sizeof(buf), "%s", a.s);
    else snprintf(buf, sizeof(buf), "%d", a.i);
    size_t len = strlen(buf);
    if (b.type==V_STRING) snprintf(buf+len, sizeof(buf)-len, "%s", b.s);
    else snprintf(buf+len, sizeof(buf)-len, "%d", b.i);
    return V_str(buf);
}

static Value eval(Node* n) {
    if (!n || g_error.kind) return V_int(0);

    switch (n->type) {
        case N_INT:    return V_int(atoi(n->value));
        case N_STRING: return V_str(n->value);
        case N_VAR: {
            Var* v = var_find(n->value);
            if (!v) { set_error(ERR_RUNTIME, n->line, n->col, "var '%s' not defined", n->value); return V_int(0); }
            return v->val;
        }
        case N_UNARY: {
            Value a = eval(n->left);
            if (g_error.kind) return V_int(0);
            switch (n->op) {
                case OP_NOT:   return V_int(!truthy(a));
                case OP_MINUS: if (a.type!=V_INT){ set_error(ERR_RUNTIME, n->line,n->col,"- unary is not int"); return V_int(0);} return V_int(-a.i);
                case OP_PLUS:  if (a.type!=V_INT){ set_error(ERR_RUNTIME, n->line,n->col,"+ unáry is not int"); return V_int(0);} return V_int(+a.i);
                default: return V_int(0);
            }
        }
        case N_BINARY: {
            // curto-circuito em && e ||
            if (n->op==OP_AND) {
                Value L = eval(n->left); if (g_error.kind) return V_int(0);
                if (!truthy(L)) return V_int(0);
                Value R = eval(n->right); if (g_error.kind) return V_int(0);
                return V_int(truthy(R)!=0);
            }
            if (n->op==OP_OR) {
                Value L = eval(n->left); if (g_error.kind) return V_int(0);
                if (truthy(L)) return V_int(1);
                Value R = eval(n->right); if (g_error.kind) return V_int(0);
                return V_int(truthy(R)!=0);
            }
            // demais binários
            Value L = eval(n->left);  if (g_error.kind) return V_int(0);
            Value R = eval(n->right); if (g_error.kind) return V_int(0);
            switch (n->op) {
                case OP_PLUS:  return add_any(n, L, R);
                case OP_MINUS: case OP_MUL: case OP_DIV: return bin_num_num(n, L, R, n->op);
                case OP_EQ: case OP_NE: case OP_LT: case OP_LE: case OP_GT: case OP_GE:
                    return cmp_any(n, L, R, n->op);
                default: return V_int(0);
            }
        }
        case N_ASSIGN: {
            Value v = eval(n->left);
            if (g_error.kind) return V_int(0);
            Var* slot = var_ensure(n->value);
            if (!slot) { set_error(ERR_RUNTIME, n->line, n->col, "var limit"); return V_int(0); }
            slot->val = v;
            return v;
        }
        case N_PRINT: {
            Node* a = n->extra; int first=1;
            while (a && !g_error.kind) {
                Value v = eval(a);
                if (g_error.kind) break;
                if (!first) printf(" ");
                print_value(v);
                first=0;
                a = a->right;
            }
            if (!g_error.kind) printf("\n");
            return V_int(0);
        }
        case N_INPUT: {
            Var* slot = var_ensure(n->value);
            if (!slot) { set_error(ERR_RUNTIME, n->line, n->col, "var limit"); return V_int(0); }
            char buf[512];
            printf("> "); fflush(stdout);
            if (!fgets(buf, sizeof(buf), stdin)) { set_error(ERR_RUNTIME, n->line, n->col, "error in len"); return V_int(0); }
            buf[strcspn(buf,"\n")]=0;
            slot->val = V_str(buf);
            return slot->val;
        }
        case N_IF: {
            Value c = eval(n->left); if (g_error.kind) return V_int(0);
            if (truthy(c)) exec_block(n->extra);
            else if (n->right) exec_block(n->right);
            return V_int(0);
        }
        case N_WHILE: {
            int guard = 1000000; // evita loop infinito acidental
            while (guard-- > 0) {
                Value c = eval(n->left);
                if (g_error.kind) return V_int(0);
                if (!truthy(c)) break;
                exec_block(n->extra);
                if (g_error.kind) return V_int(0);
            }
            if (guard<=0) set_error(ERR_RUNTIME, n->line, n->col, "While error");
            return V_int(0);
        }
        case N_BLOCK: {
            exec_block(n);
            return V_int(0);
        }
        default: return V_int(0);
    }
}

static void exec_block(Node* stmt) {
    Node* cur = (stmt && stmt->type==N_BLOCK)? stmt->extra : stmt;
    while (cur && !g_error.kind) {
        (void)eval(cur);
        cur = cur->right;
    }
}

#endif

