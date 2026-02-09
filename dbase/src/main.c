#include <stdio.h>
#include <string.h>
#include "dbf.h"
#include "command.h"
#include "util.h"

static dbf_t current_db;

int main(void) {
    char line[256];

    printf("dBASE III Clone for SLOW-32\n");
    printf("Stage 3B: Work Areas & File Commands\n\n");

    dbf_init(&current_db);

    for (;;) {
        printf(". ");
        if (read_line(line, sizeof(line)) < 0)
            break;
        trim_right(line);
        if (line[0] == '\0')
            continue;
        if (cmd_execute(&current_db, line))
            break;
    }

    cmd_close_all();

    return 0;
}
