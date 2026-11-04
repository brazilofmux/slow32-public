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
