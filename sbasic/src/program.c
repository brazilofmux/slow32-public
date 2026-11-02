/* The program as loaded: see program.h. */
#include "program.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

/* --- Program storage --- */

#define MAX_PROGRAM_LINES 2048

static char **program_lines = NULL;
static int program_count = 0;
static int program_capacity = 0;

void program_clear(void) {
    for (int i = 0; i < program_count; i++)
        free(program_lines[i]);
    program_count = 0;
}

void program_add_line(const char *line) {
    if (program_count >= MAX_PROGRAM_LINES) {
        printf("Program too large\n");
        return;
    }
    if (program_count >= program_capacity) {
        int newcap = program_capacity ? program_capacity * 2 : 64;
        if (newcap > MAX_PROGRAM_LINES) newcap = MAX_PROGRAM_LINES;
        char **newlines = realloc(program_lines, newcap * sizeof(char *));
        if (!newlines) { printf("Out of memory\n"); return; }
        program_lines = newlines;
        program_capacity = newcap;
    }
    program_lines[program_count] = strdup(line);
    if (!program_lines[program_count]) { printf("Out of memory\n"); return; }
    program_count++;
}

/* --- $INCLUDE directive processing --- */

#define MAX_INCLUDE_DEPTH 16

/* Extract filename from $INCLUDE directive. Returns NULL if not an include.
 * Supports:
 *   $INCLUDE: "filename.bas"
 *   $INCLUDE: 'filename.bas'
 *   '$INCLUDE: 'filename.bas'    (QBasic-style, in comment)
 *   REM $INCLUDE: 'filename.bas' (QBasic-style, in REM)
 */
static const char *parse_include(const char *line) {
    const char *p = line;
    while (*p == ' ' || *p == '\t') p++;

    /* Skip leading ' or REM (QBasic puts includes in comments) */
    if (*p == '\'') p++;
    else if (toupper((unsigned char)p[0]) == 'R' &&
             toupper((unsigned char)p[1]) == 'E' &&
             toupper((unsigned char)p[2]) == 'M' &&
             (p[3] == ' ' || p[3] == '\t'))
        p += 4;

    while (*p == ' ' || *p == '\t') p++;

    /* Match $INCLUDE: */
    if (*p != '$') return NULL;
    p++;
    if (!(toupper((unsigned char)p[0]) == 'I' &&
          toupper((unsigned char)p[1]) == 'N' &&
          toupper((unsigned char)p[2]) == 'C' &&
          toupper((unsigned char)p[3]) == 'L' &&
          toupper((unsigned char)p[4]) == 'U' &&
          toupper((unsigned char)p[5]) == 'D' &&
          toupper((unsigned char)p[6]) == 'E'))
        return NULL;
    p += 7;

    /* Optional colon */
    while (*p == ' ' || *p == '\t') p++;
    if (*p == ':') p++;
    while (*p == ' ' || *p == '\t') p++;

    /* Extract filename from quotes or apostrophes */
    static char fname_buf[256];
    char delim = *p;
    if (delim != '"' && delim != '\'') return NULL;
    p++;
    int len = 0;
    while (*p && *p != delim && len < 255)
        fname_buf[len++] = *p++;
    fname_buf[len] = '\0';
    return (len > 0) ? fname_buf : NULL;
}

/* a line of any length, its end (\n, \r\n) dropped; NULL at the end */
static char *read_file_line(FILE *f) {
    int cap = 256, len = 0, ch;
    char *buf = malloc(cap);
    if (!buf) return NULL;
    while ((ch = fgetc(f)) != EOF && ch != '\n') {
        if (len + 1 >= cap) {
            char *nb = realloc(buf, cap * 2);
            if (!nb) break;
            buf = nb;
            cap *= 2;
        }
        buf[len++] = (char)ch;
    }
    if (ch == EOF && len == 0) { free(buf); return NULL; }
    while (len > 0 && buf[len - 1] == '\r') len--;
    buf[len] = '\0';
    return buf;
}

static void include_file(const char *filename, int depth);

void program_add_line_with_includes(const char *line, int depth) {
    const char *fname = parse_include(line);
    if (fname) {
        include_file(fname, depth);
    } else {
        program_add_line(line);
    }
}

static void include_file(const char *filename, int depth) {
    if (depth >= MAX_INCLUDE_DEPTH) {
        printf("$INCLUDE nested too deep: %s\n", filename);
        return;
    }
    FILE *f = fopen(filename, "r");
    if (!f) {
        printf("$INCLUDE file not found: %s\n", filename);
        return;
    }
    char *line;
    while ((line = read_file_line(f)) != NULL) {
        program_add_line_with_includes(line, depth + 1);
        free(line);
    }
    fclose(f);
}

/* Build program text from stored lines */
char *program_get_text(void) {
    int total = 0;
    for (int i = 0; i < program_count; i++)
        total += (int)strlen(program_lines[i]) + 1;
    total++;

    char *text = malloc(total);
    if (!text) return NULL;

    char *p = text;
    for (int i = 0; i < program_count; i++) {
        int len = (int)strlen(program_lines[i]);
        memcpy(p, program_lines[i], len);
        p += len;
        *p++ = '\n';
    }
    *p = '\0';
    return text;
}

int program_load_file(const char *filename) {
    FILE *f = fopen(filename, "r");
    char *line;
    if (!f) return -1;
    program_clear();
    while ((line = read_file_line(f)) != NULL) {
        program_add_line_with_includes(line, 0);
        free(line);
    }
    fclose(f);
    return program_count;
}

int program_line_count(void) { return program_count; }

const char *program_line(int i) { return program_lines[i]; }

void program_delete_lines(int from, int to) {
    int count = to - from + 1;
    for (int i = from - 1; i < to; i++)
        free(program_lines[i]);
    for (int i = to; i < program_count; i++)
        program_lines[i - count] = program_lines[i];
    program_count -= count;
}
