#include "lexer.h"
#include "parser.h"
#include "ast.h"
#include "eval.h"
#include "env.h"
#include "value.h"
#include "error.h"
#include "builtin.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

/* --- Program storage --- */

#define MAX_PROGRAM_LINES 2048
#define MAX_LINE_LEN 256

static char **program_lines = NULL;
static int program_count = 0;
static int program_capacity = 0;

/* Global environment */
static env_t *global_env = NULL;

static void program_clear(void) {
    for (int i = 0; i < program_count; i++)
        free(program_lines[i]);
    program_count = 0;
}

static void program_add_line(const char *line) {
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

/* Build program text from stored lines */
static char *program_get_text(void) {
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

/* --- REPL commands --- */

static void cmd_run(void) {
    if (program_count == 0) {
        printf("No program\n");
        return;
    }

    char *text = program_get_text();
    if (!text) {
        printf("Out of memory\n");
        return;
    }

    /* Reset environment */
    if (global_env) env_destroy(global_env);
    global_env = env_create(NULL);

    /* Parse */
    parser_t parser;
    parser_init(&parser, text, 1);
    stmt_t *program = parser_parse(&parser);

    if (parser_had_error(&parser)) {
        error_print(parser.error, parser.error_line);
        stmt_free(program);
        free(text);
        return;
    }

    /* Execute */
    error_t err = eval_program(global_env, program);
    if (err != ERR_NONE && err != ERR_EXIT)
        error_print(err, 0);

    stmt_free(program);
    free(text);
}

static void cmd_list(void) {
    for (int i = 0; i < program_count; i++)
        printf("%d: %s\n", i + 1, program_lines[i]);
}

static void cmd_new(void) {
    program_clear();
    if (global_env) {
        env_destroy(global_env);
        global_env = env_create(NULL);
    }
}

static void cmd_load(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        printf("File not found: %s\n", filename);
        return;
    }

    program_clear();
    char line[MAX_LINE_LEN];
    while (fgets(line, sizeof(line), f)) {
        /* Strip trailing newline/cr */
        int len = (int)strlen(line);
        while (len > 0 && (line[len - 1] == '\n' || line[len - 1] == '\r'))
            line[--len] = '\0';
        program_add_line(line);
    }
    fclose(f);
    printf("Loaded %d lines from %s\n", program_count, filename);
}

static void cmd_save(const char *filename) {
    FILE *f = fopen(filename, "w");
    if (!f) {
        printf("Cannot write: %s\n", filename);
        return;
    }
    for (int i = 0; i < program_count; i++)
        fprintf(f, "%s\n", program_lines[i]);
    fclose(f);
    printf("Saved %d lines to %s\n", program_count, filename);
}

/* --- Input reading --- */

static int read_line(char *buf, int bufsize) {
    int pos = 0;
    int ch;
    while ((ch = getchar()) != EOF) {
        if (ch == '\n') break;
        if (ch == '\r') continue;
        if (pos < bufsize - 1)
            buf[pos++] = (char)ch;
    }
    buf[pos] = '\0';
    if (ch == EOF && pos == 0)
        return -1; /* EOF */
    return pos;
}

/* Skip leading whitespace and return pointer to first non-space */
static const char *skip_ws(const char *s) {
    while (*s == ' ' || *s == '\t') s++;
    return s;
}

/* Case-insensitive prefix match */
static int match_cmd(const char *input, const char *cmd) {
    while (*cmd) {
        if (toupper((unsigned char)*input) != *cmd)
            return 0;
        input++;
        cmd++;
    }
    /* Must be followed by space, null, or end */
    return (*input == '\0' || *input == ' ' || *input == '\t');
}

/* Get argument after command keyword */
static const char *cmd_arg(const char *input, const char *cmd) {
    input += strlen(cmd);
    return skip_ws(input);
}

/* --- Direct mode execution --- */

static void exec_direct(const char *line) {
    /* Try to parse and execute as a direct statement */
    parser_t parser;
    parser_init(&parser, line, 0);
    stmt_t *s = parser_parse_line(&parser);

    if (parser_had_error(&parser)) {
        error_print(parser.error, 0);
        stmt_free(s);
        return;
    }

    if (s) {
        if (!global_env)
            global_env = env_create(NULL);
        error_t err = eval_stmt(global_env, s);
        if (err != ERR_NONE && err != ERR_EXIT)
            error_print(err, 0);
        stmt_free(s);
    }
}

/* --- Load and run file (for piped input / command-line args) --- */

static int load_and_run_stdin(void) {
    /* Read all of stdin into program */
    char line[MAX_LINE_LEN];
    while (read_line(line, sizeof(line)) >= 0) {
        const char *trimmed = skip_ws(line);

        /* Check for REPL commands in stream */
        if (match_cmd(trimmed, "RUN")) {
            cmd_run();
            continue;
        }
        if (match_cmd(trimmed, "BYE")) {
            return 1;
        }
        if (match_cmd(trimmed, "NEW")) {
            cmd_new();
            continue;
        }
        if (match_cmd(trimmed, "LIST")) {
            cmd_list();
            continue;
        }
        if (match_cmd(trimmed, "LOAD")) {
            cmd_load(cmd_arg(trimmed, "LOAD"));
            continue;
        }
        if (match_cmd(trimmed, "SAVE")) {
            cmd_save(cmd_arg(trimmed, "SAVE"));
            continue;
        }

        /* Otherwise add as program line */
        if (*trimmed != '\0')
            program_add_line(trimmed);
    }
    return 0;
}

static void run_file(const char *filename) {
    cmd_load(filename);
    cmd_run();
}

/* --- Main --- */

int main(int argc, char *argv[]) {
    global_env = env_create(NULL);

    if (argc >= 2) {
        /* Run file */
        run_file(argv[1]);
    } else {
        /* Interactive/pipe mode */
        load_and_run_stdin();
    }

    env_destroy(global_env);
    return 0;
}
