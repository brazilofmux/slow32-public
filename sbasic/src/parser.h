#ifndef SBASIC_PARSER_H
#define SBASIC_PARSER_H

#include "ast.h"
#include "lexer.h"
#include "error.h"

typedef struct {
    lexer_t lex;
    error_t error;
    int error_line;
    char error_detail[128];
    /* the names DIM ... AS a TYPE (or a parameter AS one) made records so
     * far: rec.field is a field of one; any other name with a dot is a
     * name (QuickBASIC's, TWO.PI#, SUB summer.day) */
    char records[256][64];
    int nrecords;
    /* COMMON SHARED a() AS type: each such array's type, for its DIM or
     * REDIM later without AS (QBasic types it by the COMMON) */
    struct { char name[64]; int type; char rec[64]; int fixlen; } commons[64];
    int ncommons;
    /* QuickBASIC's DEFtype is the text's: a name with no suffix takes the
     * suffix of the DEFtype in force where it is written (x under DEFINT
     * is x%, and x! another variable) -- the parser writes it so; the
     * names a CONST or an AS gave keep theirs */
    val_type_t deftype[26];
    char (*plain)[64];          /* CONSTs' and AS-declared names */
    int nplain, capplain;
} parser_t;

/* Initialize parser */
void parser_init(parser_t *p, const char *src, int start_line);

/* Parse all statements until EOF. Returns linked list of statements.
   On error, sets p->error and p->error_line, returns partial parse or NULL. */
stmt_t *parser_parse(parser_t *p);

/* Parse a single statement (for REPL one-line input) */
stmt_t *parser_parse_line(parser_t *p);

/* Check if the most recent parse had an error */
int parser_had_error(parser_t *p);

#endif
