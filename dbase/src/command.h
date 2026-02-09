#ifndef COMMAND_H
#define COMMAND_H

#include "dbf.h"

/* Execute a command line. Returns 1 if QUIT, 0 otherwise. */
int cmd_execute(dbf_t *db, char *line);

/* Close all open work areas */
void cmd_close_all(void);

#endif
