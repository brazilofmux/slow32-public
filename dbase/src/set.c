#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "set.h"
#include "util.h"

void set_init(set_options_t *opts) {
    opts->talk = 1;
    opts->deleted = 0;    /* OFF = show deleted records */
    opts->exact = 0;      /* OFF = prefix matching */
    opts->heading = 1;
    opts->confirm = 0;
    opts->bell = 1;
    opts->safety = 1;
    opts->console = 1;
    opts->decimals = 2;
    opts->date_format = DATE_AMERICAN;
    opts->device = 0;  /* SCREEN */
}

static int parse_on_off(const char *p) {
    p = skip_ws(p);
    if (str_imatch(p, "ON")) return 1;
    if (str_imatch(p, "OFF")) return 0;
    return -1;
}

static const char *format_name(date_format_t f) {
    switch (f) {
    case DATE_AMERICAN: return "AMERICAN";
    case DATE_ANSI:     return "ANSI";
    case DATE_BRITISH:  return "BRITISH";
    case DATE_FRENCH:   return "FRENCH";
    case DATE_GERMAN:   return "GERMAN";
    case DATE_ITALIAN:  return "ITALIAN";
    case DATE_JAPAN:    return "JAPAN";
    }
    return "AMERICAN";
}

void set_display(const set_options_t *opts) {
    printf("TALK      = %s\n", opts->talk ? "ON" : "OFF");
    printf("DELETED   = %s\n", opts->deleted ? "ON" : "OFF");
    printf("EXACT     = %s\n", opts->exact ? "ON" : "OFF");
    printf("HEADING   = %s\n", opts->heading ? "ON" : "OFF");
    printf("CONFIRM   = %s\n", opts->confirm ? "ON" : "OFF");
    printf("BELL      = %s\n", opts->bell ? "ON" : "OFF");
    printf("SAFETY    = %s\n", opts->safety ? "ON" : "OFF");
    printf("CONSOLE   = %s\n", opts->console ? "ON" : "OFF");
    printf("DECIMALS  = %d\n", opts->decimals);
    printf("DATE      = %s\n", format_name(opts->date_format));
    printf("DEVICE    = %s\n", opts->device ? "PRINT" : "SCREEN");
}

void set_execute(set_options_t *opts, const char *arg) {
    const char *p = skip_ws(arg);
    int val;

    if (*p == '\0') {
        set_display(opts);
        return;
    }

    if (str_imatch(p, "TALK")) {
        val = parse_on_off(p + 4);
        if (val >= 0) opts->talk = val;
        else printf("Syntax: SET TALK ON/OFF\n");
        return;
    }
    if (str_imatch(p, "DELETED")) {
        val = parse_on_off(p + 7);
        if (val >= 0) opts->deleted = val;
        else printf("Syntax: SET DELETED ON/OFF\n");
        return;
    }
    if (str_imatch(p, "EXACT")) {
        val = parse_on_off(p + 5);
        if (val >= 0) opts->exact = val;
        else printf("Syntax: SET EXACT ON/OFF\n");
        return;
    }
    if (str_imatch(p, "HEADING")) {
        val = parse_on_off(p + 7);
        if (val >= 0) opts->heading = val;
        else printf("Syntax: SET HEADING ON/OFF\n");
        return;
    }
    if (str_imatch(p, "CONFIRM")) {
        val = parse_on_off(p + 7);
        if (val >= 0) opts->confirm = val;
        else printf("Syntax: SET CONFIRM ON/OFF\n");
        return;
    }
    if (str_imatch(p, "BELL")) {
        val = parse_on_off(p + 4);
        if (val >= 0) opts->bell = val;
        else printf("Syntax: SET BELL ON/OFF\n");
        return;
    }
    if (str_imatch(p, "SAFETY")) {
        val = parse_on_off(p + 6);
        if (val >= 0) opts->safety = val;
        else printf("Syntax: SET SAFETY ON/OFF\n");
        return;
    }
    if (str_imatch(p, "CONSOLE")) {
        val = parse_on_off(p + 7);
        if (val >= 0) opts->console = val;
        else printf("Syntax: SET CONSOLE ON/OFF\n");
        return;
    }
    if (str_imatch(p, "DECIMALS")) {
        p = skip_ws(p + 8);
        if (str_imatch(p, "TO")) p = skip_ws(p + 2);
        val = atoi(p);
        if (val >= 0 && val <= 18)
            opts->decimals = val;
        else
            printf("DECIMALS must be 0-18.\n");
        return;
    }
    if (str_imatch(p, "DATE")) {
        p = skip_ws(p + 4);
        if (str_imatch(p, "TO")) p = skip_ws(p + 2);
        if (str_imatch(p, "AMERICAN"))  opts->date_format = DATE_AMERICAN;
        else if (str_imatch(p, "ANSI")) opts->date_format = DATE_ANSI;
        else if (str_imatch(p, "BRITISH")) opts->date_format = DATE_BRITISH;
        else if (str_imatch(p, "FRENCH"))  opts->date_format = DATE_FRENCH;
        else if (str_imatch(p, "GERMAN"))  opts->date_format = DATE_GERMAN;
        else if (str_imatch(p, "ITALIAN")) opts->date_format = DATE_ITALIAN;
        else if (str_imatch(p, "JAPAN"))   opts->date_format = DATE_JAPAN;
        else printf("Unknown date format.\n");
        return;
    }

    /* No-op SET options: accepted silently for compatibility */
    if (str_imatch(p, "ECHO")) {
        parse_on_off(p + 4); /* consume ON/OFF, ignore */
        return;
    }
    if (str_imatch(p, "MENU")) {
        /* SET MENU or SET MENUS */
        const char *q = p + 4;
        if (*q == 'S' || *q == 's') q++;
        parse_on_off(q);
        return;
    }
    if (str_imatch(p, "STATUS")) {
        parse_on_off(p + 6);
        return;
    }
    if (str_imatch(p, "SCOREBOARD")) {
        parse_on_off(p + 10);
        return;
    }
    if (str_imatch(p, "ESCAPE")) {
        parse_on_off(p + 6);
        return;
    }
    if (str_imatch(p, "INTENSITY")) {
        parse_on_off(p + 9);
        return;
    }
    if (str_imatch(p, "UNIQUE")) {
        parse_on_off(p + 6);
        return;
    }
    if (str_imatch(p, "FUNCTION")) {
        /* SET FUNCTION <n> TO <string> — just skip */
        return;
    }
    if (str_imatch(p, "PRINT")) {
        parse_on_off(p + 5);
        return;
    }
    if (str_imatch(p, "HELP")) {
        parse_on_off(p + 4);
        return;
    }
    if (str_imatch(p, "CENTURY")) {
        parse_on_off(p + 7);
        return;
    }
    if (str_imatch(p, "PATH")) {
        /* SET PATH TO <path> — skip */
        return;
    }
    if (str_imatch(p, "DEVICE")) {
        /* SET DEVICE TO SCREEN/PRINT */
        p = skip_ws(p + 6);
        if (str_imatch(p, "TO")) p = skip_ws(p + 2);
        if (str_imatch(p, "SCREEN"))
            opts->device = 0;
        else if (str_imatch(p, "PRINT"))
            opts->device = 1;
        return;
    }

    printf("Unknown SET option.\n");
}
