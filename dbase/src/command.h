#ifndef COMMAND_H
#define COMMAND_H

#include "dbf.h"
#include "memvar.h"
#include "expr.h"

/* Execute a command line. Returns 1 if QUIT, 0 otherwise. */
int cmd_execute(dbf_t *db, char *line);

/* Close all open work areas */
void cmd_close_all(void);

/* Access to shared state (for program.c, screen.c) */
memvar_store_t *cmd_get_memvar_store(void);
expr_ctx_t *cmd_get_expr_ctx(void);
int cmd_get_device(void);     /* 0=SCREEN, 1=PRINT */
int cmd_get_console(void);    /* SET CONSOLE ON/OFF */

#endif
