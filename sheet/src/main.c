#include "sheet.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <term.h>

static sheet_t g_sheet;

static void trim(char *s) {
    char *p = s;
    char *end;
    while (*p && isspace((unsigned char)*p)) {
        p++;
    }
    if (p != s) {
        memmove(s, p, strlen(p) + 1);
    }
    end = s + strlen(s);
    while (end > s && isspace((unsigned char)end[-1])) {
        end--;
    }
    *end = '\0';
}

static int icmp(const char *a, const char *b) {
    while (*a && *b) {
        int ca = toupper((unsigned char)*a);
        int cb = toupper((unsigned char)*b);
        if (ca != cb) {
            return ca - cb;
        }
        a++;
        b++;
    }
    return (unsigned char)toupper((unsigned char)*a) -
           (unsigned char)toupper((unsigned char)*b);
}

static void print_cell(int col, int row) {
    cell_t *cell = sheet_cell(&g_sheet, col, row);
    char name[8];
    format_cell_name(col, row, name, (int)sizeof(name));
    if (!cell || cell->kind == CELL_EMPTY) {
        printf("%s: (empty)\n", name);
        return;
    }
    if (cell->kind == CELL_LABEL) {
        printf("%s: %s\n", name, cell->raw ? cell->raw : "");
        return;
    }
    if (cell->kind == CELL_FORMULA && cell->err != SERR_OK) {
        printf("%s: %s\n", name, sheet_err_str(cell->err));
        return;
    }
    if (cell->kind == CELL_FORMULA) {
        printf("%s: %g    [%s]\n", name, cell->value,
               cell->raw ? cell->raw : "");
        return;
    }
    printf("%s: %g\n", name, cell->value);
}

static void help(void) {
    printf("A1=10          number\n");
    printf("A2=hello       label\n");
    printf("A3==A1*2       formula (or A3=+A1*2)\n");
    printf("A3             print one cell\n");
    printf("LIST           print used grid\n");
    printf("SAVE file.sht  write text sheet\n");
    printf("LOAD file.sht  read text sheet\n");
    printf("CLEAR          empty the grid\n");
    printf("QUIT           leave\n");
    printf("Functions: @SUM @AVG @MIN @MAX @COUNT  ranges A1:B3\n");
}

static int dispatch(char *line) {
    const char *end;
    int col, row;
    char cmd[16];
    char *args;
    int n = 0;

    trim(line);
    if (!line[0]) {
        return 1;
    }
    if (line[0] == '/') {
        line++;
    }

    if (parse_cell_name(line, &end, &col, &row) && col >= 0) {
        const char *rest = end;
        while (*rest == ' ' || *rest == '\t') {
            rest++;
        }
        if (*rest == '\0') {
            sheet_recalc(&g_sheet);
            print_cell(col, row);
            return 1;
        }
        if (*rest == '=') {
            rest++;
            sheet_set(&g_sheet, col, row, rest);
            sheet_recalc(&g_sheet);
            print_cell(col, row);
            return 1;
        }
    }

    while (line[n] && !isspace((unsigned char)line[n]) && n < 15) {
        cmd[n] = (char)toupper((unsigned char)line[n]);
        n++;
    }
    cmd[n] = '\0';
    args = line + n;
    while (*args && isspace((unsigned char)*args)) {
        args++;
    }

    if (icmp(cmd, "QUIT") == 0 || icmp(cmd, "EXIT") == 0 ||
        icmp(cmd, "Q") == 0) {
        return 0;
    }
    if (icmp(cmd, "HELP") == 0 || icmp(cmd, "?") == 0) {
        help();
        return 1;
    }
    if (icmp(cmd, "LIST") == 0) {
        sheet_recalc(&g_sheet);
        sheet_list(&g_sheet, stdout);
        return 1;
    }
    if (icmp(cmd, "CLEAR") == 0) {
        sheet_clear(&g_sheet);
        printf("cleared\n");
        return 1;
    }
    if (icmp(cmd, "SAVE") == 0) {
        if (!args[0]) {
            printf("SAVE file.sht\n");
            return 1;
        }
        if (sheet_save(&g_sheet, args) != 0) {
            printf("cannot save %s\n", args);
        } else {
            printf("saved %s\n", args);
        }
        return 1;
    }
    if (icmp(cmd, "LOAD") == 0) {
        if (!args[0]) {
            printf("LOAD file.sht\n");
            return 1;
        }
        if (sheet_load(&g_sheet, args) != 0) {
            printf("cannot load %s\n", args);
        } else {
            printf("loaded %s\n", args);
            sheet_list(&g_sheet, stdout);
        }
        return 1;
    }
    printf("?\n");
    return 1;
}

int main(int argc, char **argv) {
    char line[256];
    char filename[256];
    int i;
    int line_mode = 0;

    sheet_init(&g_sheet);
    filename[0] = '\0';

    for (i = 1; i < argc; i++) {
        if (argv[i][0] == '-' && argv[i][1] == '-' &&
            (icmp(argv[i] + 2, "line") == 0 || icmp(argv[i] + 2, "repl") == 0)) {
            line_mode = 1;
            continue;
        }
        if (sheet_load(&g_sheet, argv[i]) != 0) {
            printf("cannot load %s\n", argv[i]);
            return 1;
        }
        strncpy(filename, argv[i], sizeof(filename) - 1);
        filename[sizeof(filename) - 1] = '\0';
        if (line_mode) {
            sheet_list(&g_sheet, stdout);
        }
    }

    if (!line_mode && term_init() == 0) {
        sheet_ui(&g_sheet, filename, (int)sizeof(filename));
        term_cleanup();
        sheet_clear(&g_sheet);
        return 0;
    }

    printf("SLOW-32 Sheet 0.1   (HELP for commands)\n");
    for (;;) {
        printf(">");
        fflush(stdout);
        if (!fgets(line, (int)sizeof(line), stdin)) {
            printf("\n");
            break;
        }
        if (!dispatch(line)) {
            break;
        }
    }
    sheet_clear(&g_sheet);
    return 0;
}
