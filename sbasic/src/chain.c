/* chain.c -- a chained program's text to statements: the parser, for
 * eval.c's CHAIN (a program that never chains, compiled, leaves this out). */
#include "chain.h"
#include "program.h"
#include "parser.h"
#include "eval.h"
#include <stdio.h>
#include <stdlib.h>

void eval_chain_go(void);

stmt_t *chain_parse(const char *path) {
    parser_t parser;
    stmt_t *program;
    char *text;
    if (program_load_file(path) < 0 || !(text = program_get_text())) {
        error_print(ERR_FILE_NOT_FOUND, 0);
        return NULL;
    }
    parser_init(&parser, text, 1);
    program = parser_parse(&parser);
    if (parser_had_error(&parser)) {
        if (parser.error_detail[0])
            printf("Error in line %d: %s (%s)\n", parser.error_line,
                   error_message(parser.error), parser.error_detail);
        else
            error_print(parser.error, parser.error_line);
        stmt_free(program);
        return NULL;
    }
    return program;     /* (the text stays: the statements may point into it) */
}

/* compiled code's RUN file$ */
void eval_run_file_go(void) {
    eval_chain_run_file();
    eval_chain_go();
}

/* compiled code's CHAIN, COMMON carried: on in the interpreter, for good */
void eval_chain_go(void) {
    error_t err;
    eval_chain_parser = chain_parse;
    err = eval_chain_run();
    if (err != ERR_NONE && err != ERR_EXIT) error_print(err, 0);
    fflush(stdout);
    exit(0);
}
