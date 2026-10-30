#include <stdio.h>
#include <string.h>
#include "dbf.h"
#include "command.h"
#include "gc.h"
#include "program.h"
#include "screen.h"
#include "util.h"

static dbf_t current_db;

int main(void) {
    char line[256];

    gc_set_stack_base(line + sizeof(line));   /* the collector scans the stack up to here */

    printf("SLOW-32 xBase\n");
    printf("xBase interpreter 1.0\n\n");

    dbf_init(&current_db);
    prog_init();

    for (;;) {
        if (!cmd_output_at_line_start())
            printf("\n");   /* ? leaves the line open: the prompt starts its own */
        printf(". ");
        cmd_output_set_line_start(1);
        if (read_line(line, sizeof(line)) < 0)
            break;
        trim_right(line);
        if (line[0] == '\0')
            continue;

        /* Preprocess for && comments and macros (interactive mode) */
        prog_preprocess(line, cmd_get_memvar_store());

        if (cmd_execute(&current_db, line))
            break;
    }

    cmd_close_all();
    screen_shutdown();

    return 0;
}
