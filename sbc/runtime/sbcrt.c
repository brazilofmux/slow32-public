/* The start of a compiled SLOW BASIC program: see sbcrt.h. */
#include "chain.h"
#include "sbcrt.h"
#include "parser.h"
#include <stdio.h>

static int expected_nops;
static main_fn_t compiled_main;

error_t sbcrt_mismatch(void) {
    fprintf(stderr, "s32-sbc: the program's ops differ from its runtime's\n");
    return ERR_INTERNAL;
}

/* the main unit, after checking it is the one the C was written from */
static error_t checked_main(env_t *env, code_t *c) {
    if (c->nops != expected_nops)
        return sbcrt_mismatch();
    return compiled_main(env, c);
}

int sbcrt_main(const char *source, int nops, main_fn_t main_fn) {
    parser_t parser;
    stmt_t *program;
    env_t *env = env_create(NULL);
    error_t err;

    expected_nops = nops;
    compiled_main = main_fn;
    parser_init(&parser, source, 1);
    program = parser_parse(&parser);
    if (parser_had_error(&parser)) {
        if (parser.error_detail[0])
            printf("Error in line %d: %s (%s)\n", parser.error_line,
                   error_message(parser.error), parser.error_detail);
        else
            error_print(parser.error, parser.error_line);
        stmt_free(program);
        env_destroy(env);
        return 0;
    }
    eval_chain_parser = chain_parse;
    err = eval_program_with(env, program, checked_main);
    if (err != ERR_NONE && err != ERR_EXIT)
        error_print(err, 0);
    stmt_free(program);
    env_destroy(env);
    return 0;
}
