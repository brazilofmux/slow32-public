#ifndef COMMAND_H
#define COMMAND_H

#include "dbf.h"
#include "memvar.h"
#include "expr.h"
#include "clause.h"

/* Execute a command line. Returns 1 if QUIT, 0 otherwise. */
int cmd_execute(dbf_t *db, char *line);

/* Close all open work areas */
void cmd_close_all(void);

/* Clause and scope utilities */
struct lexer;
void clause_init(clause_t *c);
int  parse_clauses(struct lexer *l, clause_t *c);
int  scope_bounds(dbf_t *db, const scope_t *s, uint32_t *start, uint32_t *end);
void clause_compile(clause_t *c, memvar_store_t *store);
void clause_free_ast(clause_t *c);

/* Access to shared state (for program.c, screen.c) */
memvar_store_t *cmd_get_memvar_store(void);
expr_ctx_t *cmd_get_expr_ctx(void);
int cmd_get_device(void);     /* 0=SCREEN, 1=PRINT */
int cmd_get_console(void);    /* SET CONSOLE ON/OFF */
int cmd_get_talk(void);       /* SET TALK ON/OFF */
int cmd_get_confirm(void);    /* SET CONFIRM ON/OFF */

#endif
