/* Token-stream dumper: reads fixed-form F77, prints one line per
 * statement (label + token list).  The gate for card assembly and
 * tokenization. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include "f77_shim.h"
#include "f77_contract.h"
static void f77_error(char *msg);
#include "f77_card.h"
#include "f77_lexer_gen.c"

static void f77_error(char *msg) { printf("ERROR: %s\n", msg); }

static char *tname(int t) {
    switch (t) {
    case T_NAME: return "NAME"; case T_ICON: return "ICON";
    case T_RCON: return "RCON"; case T_DCON: return "DCON";
    case T_SCON: return "SCON"; case T_LP: return "("; case T_RP: return ")";
    case T_COMMA: return ","; case T_ASSIGN: return "="; case T_PLUS: return "+";
    case T_MINUS: return "-"; case T_STAR: return "*"; case T_SLASH: return "/";
    case T_POWER: return "**"; case T_CONCAT: return "//"; case T_COLON: return ":";
    case T_EQ: return ".EQ."; case T_NE: return ".NE."; case T_LT: return ".LT.";
    case T_LE: return ".LE."; case T_GT: return ".GT."; case T_GE: return ".GE.";
    case T_AND: return ".AND."; case T_OR: return ".OR."; case T_NOT: return ".NOT.";
    case T_EQV: return ".EQV."; case T_NEQV: return ".NEQV.";
    case T_TRUE: return ".TRUE."; case T_FALSE: return ".FALSE.";
    default: return "EOF";
    }
}

int main(int argc, char **argv) {
    static char src[1 << 20];
    int fd, n, i;
    fd = open(argv[1], 0);
    n = (int)read(fd, src, sizeof(src) - 1);
    if (n < 0) n = 0;
    src[n] = 0;
    close(fd);

    lx_src = src; lx_len = n; lx_pos = 0; lx_line = 1;
    while (f77_next_stmt()) {
        if (lx_stmt_label >= 0) printf("[%d] ", lx_stmt_label); else printf("[ ] ");
        f77_lex_stmt_init();
        for (;;) {
            f77_tok();
            if (lx_t == T_EOF) break;
            if (lx_t == T_NAME) printf("NAME(%s) ", lex_name);
            else if (lx_t == T_ICON) printf("ICON(%d) ", lex_ival);
            else if (lx_t == T_RCON) printf("RCON(%g) ", lex_dval);
            else if (lx_t == T_DCON) printf("DCON(%g) ", lex_dval);
            else if (lx_t == T_SCON) {
                printf("SCON(");
                for (i = 0; i < lex_slen; i++) putchar(lex_strpool[lex_str_off[lex_sidx] + i]);
                printf(") ");
            }
            else printf("%s ", tname(lx_t));
        }
        printf("\n");
    }
    return 0;
}
