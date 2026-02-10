#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "program.h"
#include "command.h"
#include "func.h"
#include "util.h"

static prog_state_t state;

/* SET PROCEDURE TO file — resident procedure library */
static program_t *procedure_file;

/* ---- Error state ---- */
static struct {
    int code;               /* last error number (0 = none) */
    char message[256];      /* last error message text */
    int lineno;             /* line where error occurred (1-based, 0=interactive) */
    char program[64];       /* program filename, ""=interactive */
    char on_error_proc[64]; /* ON ERROR handler procedure name, ""=none */
    int in_handler;         /* 1 = currently inside error handler */
    int retry;              /* set by RETRY command inside handler */
} error_state;

/* User-defined FUNCTION support */
static int udf_active;          /* >0 when executing a UDF call */
static value_t udf_return_val;  /* return value from FUNCTION */
static int udf_depth;           /* call_depth at UDF entry */
static int prog_udf_call(const char *name, value_t *args, int nargs, value_t *result);

/* IF/ELSE/ENDIF nesting for skip mode */
#define MAX_IF_DEPTH 32
static int if_skip;             /* >0 = skipping lines */
static int if_depth;            /* current IF nesting depth */
static int if_done[MAX_IF_DEPTH]; /* branch already taken at this depth */

/* DO CASE state */
static int case_active;         /* inside DO CASE block */
static int case_done;           /* a CASE branch has been taken */
static int case_skip;           /* skipping until matching CASE/OTHERWISE/ENDCASE */

/* ---- Suspended state for SUSPEND/RESUME ---- */
static struct {
    int if_skip, if_depth;
    int if_done[MAX_IF_DEPTH];
    int case_active, case_done, case_skip;
    int valid;  /* 1 = suspended state exists */
} suspended;

void prog_init(void) {
    memset(&state, 0, sizeof(state));
    memset(&error_state, 0, sizeof(error_state));
    memset(&suspended, 0, sizeof(suspended));
    if_skip = 0;
    if_depth = 0;
    case_active = 0;
    case_done = 0;
    case_skip = 0;
    func_set_udf_callback(prog_udf_call);
}

/* Forward declarations needed by error handling */
static void prog_run(void);

/* Helper: map expression error string to error code */
static int expr_error_code(const char *msg) {
    if (!msg) return ERR_SYNTAX;
    if (strstr(msg, "ivision by zero")) return ERR_DIV_ZERO;
    if (strstr(msg, "ype mismatch")) return ERR_TYPE_MISMATCH;
    return ERR_SYNTAX;
}

/* ---- Error handling ---- */

void prog_error(int code, const char *message) {
    error_state.code = code;
    str_copy(error_state.message, message, sizeof(error_state.message));

    /* Capture line number and program name */
    if (state.running && state.current_prog) {
        error_state.lineno = state.pc + 1;  /* 1-based */
        str_copy(error_state.program, state.current_prog->filename,
                 sizeof(error_state.program));
    } else {
        error_state.lineno = 0;
        error_state.program[0] = '\0';
    }

    /* If ON ERROR handler is set and we're not already in it, invoke it */
    if (error_state.on_error_proc[0] && !error_state.in_handler) {
        error_state.in_handler = 1;
        error_state.retry = 0;
        prog_do(error_state.on_error_proc);
        if (error_state.retry && state.running) {
            /* Decrement PC so the failed line re-executes */
            if (state.pc > 0) state.pc--;
        }
        error_state.in_handler = 0;
        error_state.retry = 0;
        return;
    }

    /* No handler — print error with context */
    if (error_state.lineno > 0 && error_state.program[0]) {
        printf("*** %s in %s line %d\n", message,
               error_state.program, error_state.lineno);
    } else {
        printf("*** %s\n", message);
    }
}

int prog_get_error_code(void) {
    return error_state.code;
}

const char *prog_get_error_message(void) {
    return error_state.message;
}

int prog_get_lineno(void) {
    return error_state.lineno;
}

const char *prog_get_program_name(void) {
    return error_state.program;
}

void prog_on_error(const char *procname) {
    if (procname && procname[0]) {
        str_copy(error_state.on_error_proc, procname,
                 sizeof(error_state.on_error_proc));
    } else {
        error_state.on_error_proc[0] = '\0';
    }
}

void prog_retry(void) {
    if (!error_state.in_handler) {
        printf("*** RETRY not in error handler\n");
        return;
    }
    error_state.retry = 1;
}

/* ---- SUSPEND / RESUME ---- */

void prog_suspend(void) {
    if (!state.running) {
        printf("*** Not in program\n");
        return;
    }
    /* Save skip state */
    suspended.if_skip = if_skip;
    suspended.if_depth = if_depth;
    memcpy(suspended.if_done, if_done, sizeof(if_done));
    suspended.case_active = case_active;
    suspended.case_done = case_done;
    suspended.case_skip = case_skip;
    suspended.valid = 1;

    /* Pause execution — prog_run() exits when running == 0 */
    state.running = 0;
    printf("Suspended.\n");
}

void prog_resume(void) {
    if (!suspended.valid) {
        printf("*** Nothing suspended\n");
        return;
    }
    /* Restore skip state */
    if_skip = suspended.if_skip;
    if_depth = suspended.if_depth;
    memcpy(if_done, suspended.if_done, sizeof(if_done));
    case_active = suspended.case_active;
    case_done = suspended.case_done;
    case_skip = suspended.case_skip;
    suspended.valid = 0;

    /* Continue execution */
    state.running = 1;
    prog_run();
}

int prog_is_running(void) {
    return state.running;
}

memvar_store_t *prog_get_memvar_store(void) {
    return cmd_get_memvar_store();
}

/* ---- Preprocessing: comments, &&, macro substitution ---- */
void prog_preprocess(char *line, memvar_store_t *store) {
    char *p;

    /* Strip && inline comments */
    p = line;
    while (*p) {
        if (*p == '&' && *(p + 1) == '&') {
            *p = '\0';
            trim_right(line);
            break;
        }
        /* Skip string literals */
        if (*p == '"' || *p == '\'') {
            char q = *p++;
            while (*p && *p != q) p++;
            if (*p) p++;
            continue;
        }
        p++;
    }

    /* Macro substitution: &varname -> value of varname */
    if (store) {
        char buf[MAX_LINE_LEN];
        char *src = line;
        char *dst = buf;
        char *end = buf + MAX_LINE_LEN - 1;

        while (*src && dst < end) {
            if (*src == '&' && *(src + 1) != '&' && is_ident_start(*(src + 1))) {
                char name[MEMVAR_NAMELEN];
                value_t val;
                int i = 0;
                src++; /* skip & */
                while (is_ident_char(*src) && i < MEMVAR_NAMELEN - 1)
                    name[i++] = *src++;
                name[i] = '\0';
                /* Optional trailing . delimiter */
                if (*src == '.') src++;

                if (memvar_find(store, name, &val) == 0 && val.type == VAL_CHAR) {
                    int len = strlen(val.str);
                    if (dst + len < end) {
                        memcpy(dst, val.str, len);
                        dst += len;
                    }
                }
                /* If var not found or not string, silently substitute nothing */
            } else {
                *dst++ = *src++;
            }
        }
        *dst = '\0';
        str_copy(line, buf, MAX_LINE_LEN);
    }
}

/* ---- Load program file ---- */
static program_t *prog_load(const char *filename) {
    FILE *fp;
    char path[128];
    char line[MAX_LINE_LEN];
    program_t *prog;
    int capacity = 128;

    str_copy(path, filename, sizeof(path));
    /* Add .PRG extension if not present */
    if (strlen(path) < 4 || str_icmp(path + strlen(path) - 4, ".PRG") != 0) {
        if (strlen(path) + 4 < sizeof(path))
            strcat(path, ".PRG");
    }

    /* Try original case first (Linux), then uppercase (DOS compat) */
    fp = fopen(path, "r");
    if (!fp) {
        str_upper(path);
        fp = fopen(path, "r");
    }
    if (!fp) {
        {
            char buf[128];
            snprintf(buf, sizeof(buf), "File not found: %s", path);
            prog_error(ERR_FILE_NOT_FOUND, buf);
        }
        return NULL;
    }

    prog = (program_t *)malloc(sizeof(program_t));
    if (!prog) {
        fclose(fp);
        return NULL;
    }
    memset(prog, 0, sizeof(program_t));
    str_copy(prog->filename, path, sizeof(prog->filename));
    prog->lines = (char **)malloc(capacity * sizeof(char *));
    if (!prog->lines) {
        free(prog);
        fclose(fp);
        return NULL;
    }

    while (fgets(line, sizeof(line), fp)) {
        trim_right(line);

        /* Line continuation: ; at end */
        while (strlen(line) > 0 && line[strlen(line) - 1] == ';') {
            char next[MAX_LINE_LEN];
            line[strlen(line) - 1] = '\0'; /* remove ; */
            if (!fgets(next, sizeof(next), fp)) break;
            trim_right(next);
            /* Skip leading whitespace of continuation */
            {
                char *np = skip_ws(next);
                if (strlen(line) + strlen(np) < MAX_LINE_LEN - 1)
                    strcat(line, np);
            }
        }

        if (prog->nlines >= capacity) {
            capacity *= 2;
            if (capacity > MAX_PROGRAM_LINES) capacity = MAX_PROGRAM_LINES;
            prog->lines = (char **)realloc(prog->lines, capacity * sizeof(char *));
        }
        if (prog->nlines >= MAX_PROGRAM_LINES) {
            printf("Program too long (max %d lines).\n", MAX_PROGRAM_LINES);
            break;
        }

        prog->lines[prog->nlines] = strdup(line);
        prog->nlines++;
    }

    fclose(fp);
    return prog;
}

static void prog_free(program_t *prog) {
    int i;
    if (!prog) return;
    if (prog->lines) {
        for (i = 0; i < prog->nlines; i++)
            free(prog->lines[i]);
        free(prog->lines);
    }
    free(prog);
}

void prog_set_procedure(const char *filename) {
    if (procedure_file) {
        prog_free(procedure_file);
        procedure_file = NULL;
    }
    if (filename && filename[0]) {
        procedure_file = prog_load(filename);
        if (procedure_file)
            printf("Procedure file: %s\n", procedure_file->filename);
    } else {
        printf("Procedure file released.\n");
    }
}

/* Forward declarations for UDF support */
static int push_frame(program_t *new_prog, int start_line);
static void pop_frame(void);
static void prog_run(void);
static int find_procedure(program_t *prog, const char *name);

/* ---- User-defined function callback (called from expr evaluator) ---- */
static int prog_udf_call(const char *name, value_t *args, int nargs, value_t *result) {
    program_t *target_prog = NULL;
    int func_line = -1;
    /* Save lightweight state (avoid copying 700KB call stack) */
    program_t *saved_prog = state.current_prog;
    int saved_pc = state.pc;
    int saved_call_depth = state.call_depth;
    int saved_loop_depth = state.loop_depth;
    int saved_running = state.running;
    int saved_if_skip = if_skip;
    int saved_if_depth = if_depth;
    int saved_case_active = case_active;
    int saved_case_done = case_done;
    int saved_case_skip = case_skip;
    int saved_udf_active = udf_active;
    int saved_udf_depth = udf_depth;
    value_t saved_udf_return = udf_return_val;

    /* Search current program, then procedure file */
    if (state.running && state.current_prog) {
        func_line = find_procedure(state.current_prog, name);
        if (func_line >= 0)
            target_prog = state.current_prog;
    }
    if (func_line < 0 && procedure_file) {
        func_line = find_procedure(procedure_file, name);
        if (func_line >= 0)
            target_prog = procedure_file;
    }
    if (func_line < 0)
        return -1;  /* not a UDF, let caller report error */

    /* Set up UDF execution */
    udf_return_val = val_nil();
    udf_active = 1;
    if_skip = 0;
    if_depth = 0;
    case_active = 0;
    case_done = 0;
    case_skip = 0;

    /* Push frame for the function (uses existing call stack in-place) */
    if (push_frame(target_prog, func_line + 1) < 0) {
        return -1;
    }
    udf_depth = state.call_depth;

    /* Store args for PARAMETERS */
    {
        call_frame_t *frame = &state.call_stack[state.call_depth - 1];
        int i;
        frame->with_argc = nargs;
        for (i = 0; i < nargs && i < 16; i++)
            frame->with_args[i] = args[i];
    }

    /* Execute the function body — prog_run will stop when RETURN sets running=0 */
    prog_run();

    /* Capture return value */
    *result = udf_return_val;

    /* Restore lightweight state — pop_frame already restored call stack properly */
    state.current_prog = saved_prog;
    state.pc = saved_pc;
    state.call_depth = saved_call_depth;
    state.loop_depth = saved_loop_depth;
    state.running = saved_running;
    if_skip = saved_if_skip;
    if_depth = saved_if_depth;
    case_active = saved_case_active;
    case_done = saved_case_done;
    case_skip = saved_case_skip;
    udf_active = saved_udf_active;
    udf_depth = saved_udf_depth;
    udf_return_val = saved_udf_return;

    return 0;
}

/* ---- Find PROCEDURE or FUNCTION <name> in program ---- */
static int find_procedure(program_t *prog, const char *name) {
    int i;
    for (i = 0; i < prog->nlines; i++) {
        char *p = skip_ws(prog->lines[i]);
        if (str_imatch(p, "PROCEDURE") || str_imatch(p, "FUNCTION")) {
            int kwlen = str_imatch(p, "FUNCTION") ? 8 : 9;
            char *rest = skip_ws(p + kwlen);
            char pname[64];
            int j = 0;
            while (is_ident_char(*rest) && j < 63)
                pname[j++] = *rest++;
            pname[j] = '\0';
            if (str_icmp(pname, name) == 0)
                return i;
        }
    }
    return -1;
}

static int line_is_kw(const char *line, const char *kw) {
    char tok[32];
    int i = 0;
    const char *p = skip_ws(line);
    if (!is_ident_start(*p)) return 0;
    while (is_ident_char(*p) && i < (int)sizeof(tok) - 1)
        tok[i++] = *p++;
    tok[i] = '\0';
    return str_imatch(tok, kw) != 0;
}

static int line_is_do_kw(const char *line, const char *kw) {
    char tok[32];
    int i = 0;
    const char *p = skip_ws(line);
    if (!is_ident_start(*p)) return 0;
    while (is_ident_char(*p) && i < (int)sizeof(tok) - 1)
        tok[i++] = *p++;
    tok[i] = '\0';
    if (!str_imatch(tok, "DO")) return 0;
    p = skip_ws(p);
    if (!is_ident_start(*p)) return 0;
    i = 0;
    while (is_ident_char(*p) && i < (int)sizeof(tok) - 1)
        tok[i++] = *p++;
    tok[i] = '\0';
    return str_imatch(tok, kw) != 0;
}

/* ---- Scan forward for matching control structure ---- */
/* Scan from line `from` for matching ELSE/ENDIF at the same nesting level */
static int scan_if(program_t *prog, int from, int want_else) {
    int depth = 1;
    int i;
    for (i = from + 1; i < prog->nlines; i++) {
        char line[MAX_LINE_LEN];
        char *p;
        str_copy(line, prog->lines[i], MAX_LINE_LEN);
        prog_preprocess(line, cmd_get_memvar_store());
        p = skip_ws(line);
        if (line_is_kw(p, "IF")) {
            depth++;
        } else if (line_is_kw(p, "ENDIF")) {
            depth--;
            if (depth == 0) return i;
        } else if (depth == 1 && want_else && line_is_kw(p, "ELSE")) {
            return i;
        }
    }
    return -1;
}

/* Scan from line `from` for matching ENDDO */
static int scan_enddo(program_t *prog, int from) {
    int depth = 1;
    int i;
    for (i = from + 1; i < prog->nlines; i++) {
        char line[MAX_LINE_LEN];
        char *p;
        str_copy(line, prog->lines[i], MAX_LINE_LEN);
        prog_preprocess(line, cmd_get_memvar_store());
        p = skip_ws(line);
        if (line_is_do_kw(p, "WHILE")) {
            depth++;
        } else if (line_is_kw(p, "ENDDO")) {
            depth--;
            if (depth == 0) return i;
        }
    }
    return -1;
}

/* Scan from line `from` for matching NEXT */
static int scan_next(program_t *prog, int from) {
    int depth = 1;
    int i;
    for (i = from + 1; i < prog->nlines; i++) {
        char line[MAX_LINE_LEN];
        char *p;
        str_copy(line, prog->lines[i], MAX_LINE_LEN);
        prog_preprocess(line, cmd_get_memvar_store());
        p = skip_ws(line);
        if (line_is_kw(p, "FOR"))
            depth++;
        else if (line_is_kw(p, "NEXT")) {
            depth--;
            if (depth == 0) return i;
        }
    }
    return -1;
}

/* Scan for ENDCASE from current position */
static int scan_endcase(program_t *prog, int from) {
    int depth = 1;
    int i;
    for (i = from + 1; i < prog->nlines; i++) {
        char line[MAX_LINE_LEN];
        char *p;
        str_copy(line, prog->lines[i], MAX_LINE_LEN);
        prog_preprocess(line, cmd_get_memvar_store());
        p = skip_ws(line);
        if (line_is_do_kw(p, "CASE"))
            depth++;
        else if (line_is_kw(p, "ENDCASE")) {
            depth--;
            if (depth == 0) return i;
        }
    }
    return -1;
}

/* ---- Save PRIVATE variables ---- */
static void save_private(call_frame_t *frame, const char *name) {
    memvar_store_t *store = cmd_get_memvar_store();
    value_t val;
    int i;

    if (frame->private_count >= 64) return;

    /* Check if already saved */
    for (i = 0; i < frame->private_count; i++) {
        if (str_icmp(frame->private_vars[i], name) == 0)
            return;
    }

    str_copy(frame->private_vars[frame->private_count], name, MEMVAR_NAMELEN);
    str_upper(frame->private_vars[frame->private_count]);

    if (memvar_find(store, name, &val) == 0) {
        frame->saved_vals[frame->private_count] = val;
        frame->saved_valid[frame->private_count] = 1;
    } else {
        frame->saved_valid[frame->private_count] = 0;
    }
    frame->private_count++;
}

/* ---- Restore PRIVATE variables on RETURN ---- */
static void restore_privates(call_frame_t *frame) {
    memvar_store_t *store = cmd_get_memvar_store();
    int i;

    for (i = 0; i < frame->private_count; i++) {
        if (frame->saved_valid[i]) {
            memvar_set(store, frame->private_vars[i], &frame->saved_vals[i]);
        } else {
            memvar_release(store, frame->private_vars[i]);
        }
    }
}

/* ---- Push call frame ---- */
static int push_frame(program_t *new_prog, int start_line) {
    call_frame_t *frame;

    if (state.call_depth >= MAX_CALL_DEPTH) {
        printf("Call stack overflow.\n");
        return -1;
    }

    frame = &state.call_stack[state.call_depth];
    memset(frame, 0, sizeof(call_frame_t));
    frame->caller_prog = state.current_prog;
    frame->caller_line = state.pc;
    frame->prog = new_prog;
    frame->return_line = start_line;
    frame->private_count = 0;

    state.call_depth++;
    state.current_prog = new_prog;
    state.pc = start_line;

    return 0;
}

/* ---- Pop call frame ---- */
static void pop_frame(void) {
    call_frame_t *frame;

    if (state.call_depth <= 0) return;

    state.call_depth--;
    frame = &state.call_stack[state.call_depth];

    /* Restore PRIVATE variables */
    restore_privates(frame);

    if (frame->caller_prog) {
        state.current_prog = frame->caller_prog;
        state.pc = frame->caller_line + 1;
    } else {
        /* Return to interactive mode */
        state.current_prog = NULL;
        state.running = 0;
    }

    /* If the program we returned from was the top-level DO and no caller,
       free it */
    if (state.call_depth == 0 && !frame->caller_prog) {
        prog_free(frame->prog);
    }
}

/* ---- Execute program lines ---- */
static void prog_run(void) {
    state.running = 1;

    while (state.running && state.current_prog && state.pc < state.current_prog->nlines) {
        char line[MAX_LINE_LEN];
        char *p;
        int quit;

        str_copy(line, state.current_prog->lines[state.pc], MAX_LINE_LEN);

        /* Preprocess: comments and macro substitution */
        prog_preprocess(line, cmd_get_memvar_store());

        p = skip_ws(line);

        /* Full-line comment: * or NOTE */
        if (*p == '*' || str_imatch(p, "NOTE")) {
            state.pc++;
            continue;
        }

        /* Empty line after preprocessing */
        if (*p == '\0') {
            state.pc++;
            continue;
        }

        /* Skip PROCEDURE/FUNCTION definitions during normal execution */
        if (line_is_kw(p, "PROCEDURE") || line_is_kw(p, "FUNCTION")) {
            /* Skip to next PROCEDURE/FUNCTION or end of file */
            int i;
            for (i = state.pc + 1; i < state.current_prog->nlines; i++) {
                char line2[MAX_LINE_LEN];
                char *lp;
                str_copy(line2, state.current_prog->lines[i], MAX_LINE_LEN);
                prog_preprocess(line2, cmd_get_memvar_store());
                lp = skip_ws(line2);
                if (line_is_kw(lp, "PROCEDURE") || line_is_kw(lp, "FUNCTION"))
                    break;
            }
            /* If we hit another PROCEDURE, stop there.
               If we hit end of file, we're done. */
            if (i >= state.current_prog->nlines) {
                /* End of program */
                if (state.call_depth > 0)
                    pop_frame();
                else {
                    state.running = 0;
                }
                continue;
            }
            state.pc = i;
            continue;
        }

        /* Handle IF/ELSE/ENDIF/DO CASE/CASE/OTHERWISE/ENDCASE skip mode */
        if (if_skip > 0) {
            /* Counting nesting while skipping */
            if (line_is_kw(p, "IF")) {
                if_skip++;
            } else if (line_is_kw(p, "ENDIF")) {
                if_skip--;
                if (if_skip == 0) {
                    /* We've found matching ENDIF */
                    if (if_depth > 0) if_depth--;
                }
            } else if (if_skip == 1 && line_is_kw(p, "ELSE")) {
                /* At our nesting level, found ELSE */
                if (if_depth > 0 && !if_done[if_depth - 1]) {
                    if_skip = 0; /* start executing ELSE branch */
                    if_done[if_depth - 1] = 1;
                }
            }
            state.pc++;
            continue;
        }

        if (case_skip) {
            /* Inside DO CASE, skipping to next CASE/OTHERWISE/ENDCASE */
            if (line_is_do_kw(p, "CASE")) {
                /* nested DO CASE - skip the whole thing */
                int target = scan_endcase(state.current_prog, state.pc);
                if (target >= 0) {
                    state.pc = target + 1;
                    continue;
                }
            } else if (line_is_kw(p, "CASE")) {
                if (!case_done) {
                    /* Evaluate this CASE condition */
                    char *cond = skip_ws(p + 4);
                    value_t val;
                    expr_ctx_t *ctx = cmd_get_expr_ctx();
                    if (expr_eval_str(ctx, cond, &val) == 0 &&
                        val.type == VAL_LOGIC && val.logic) {
                        case_skip = 0;
                        case_done = 1;
                        state.pc++;
                        continue;
                    }
                }
                state.pc++;
                continue;
            } else if (line_is_kw(p, "OTHERWISE")) {
                if (!case_done) {
                    case_skip = 0;
                    case_done = 1;
                }
                state.pc++;
                continue;
            } else if (line_is_kw(p, "ENDCASE")) {
                case_skip = 0;
                case_active = 0;
                case_done = 0;
                state.pc++;
                continue;
            }
            state.pc++;
            continue;
        }

        /* Normal execution */
        quit = prog_execute_line(line);
        if (quit) {
            state.running = 0;
            return;
        }

        /* If prog_execute_line didn't change PC (control flow commands set it),
           advance to next line. For normal commands, we need to advance. */
        /* Control flow commands will set state.pc themselves, but normal
           commands return and we need to advance. We handle this by noting
           that control flow commands set state.pc, and we only advance here
           if the line was a normal command. Actually, let's just always
           let the control flow code manage PC and advance for non-control. */
    }

    /* End of program - pop frame if needed */
    if (state.running && state.current_prog &&
        state.pc >= state.current_prog->nlines) {
        if (state.call_depth > 0) {
            pop_frame();
            if (state.running && state.current_prog)
                prog_run(); /* continue with caller (recursive tail) */
        } else {
            state.running = 0;
            if (state.current_prog) {
                prog_free(state.current_prog);
                state.current_prog = NULL;
            }
        }
    }
}

/* ---- DO command: DO <file> or DO WHILE <cond> ---- */
void prog_do(const char *arg) {
    const char *p = skip_ws(arg);

    if (str_imatch(p, "WHILE")) {
        /* DO WHILE <condition> */
        char *cond = skip_ws(p + 5);
        value_t val;
        expr_ctx_t *ctx = cmd_get_expr_ctx();

        if (!state.running) {
            printf("DO WHILE not allowed in interactive mode.\n");
            return;
        }

        /* Evaluate condition */
        if (expr_eval_str(ctx, cond, &val) != 0) {
            if (ctx->error) prog_error(expr_error_code(ctx->error), ctx->error);
            state.pc++;
            return;
        }

        if (val.type == VAL_LOGIC && val.logic) {
            /* Push loop entry */
            if (state.loop_depth >= MAX_LOOP_DEPTH) {
                printf("Loop nesting too deep.\n");
                state.pc++;
                return;
            }
            state.loop_stack[state.loop_depth].start_line = state.pc;
            state.loop_stack[state.loop_depth].is_for = 0;
            str_copy(state.loop_stack[state.loop_depth].condition, cond,
                     sizeof(state.loop_stack[state.loop_depth].condition));
            state.loop_depth++;
            state.pc++;
        } else {
            /* Skip to matching ENDDO */
            int target = scan_enddo(state.current_prog, state.pc);
            if (target < 0) {
                printf("ENDDO not found.\n");
                state.pc++;
                return;
            }
            state.pc = target + 1;
        }
        return;
    }

    if (str_imatch(p, "CASE")) {
        /* DO CASE */
        prog_do_case();
        return;
    }

    /* DO <file> or DO <procedure> [WITH arg1, arg2, ...] */
    {
        char name[64];
        int i = 0;
        program_t *prog;
        int proc_line;
        value_t with_args[16];
        int with_argc = 0;

        while (*p && *p != ' ' && *p != '\t' && i < 63)
            name[i++] = *p++;
        name[i] = '\0';

        /* Parse optional WITH clause */
        p = skip_ws(p);
        if (str_imatch(p, "WITH")) {
            expr_ctx_t *ctx = cmd_get_expr_ctx();
            p = skip_ws(p + 4);
            while (*p && with_argc < 16) {
                value_t val;
                if (expr_eval(ctx, &p, &val) != 0) {
                    if (ctx->error) prog_error(expr_error_code(ctx->error), ctx->error);
                    break;
                }
                with_args[with_argc++] = val;
                p = skip_ws(p);
                if (*p == ',') { p++; p = skip_ws(p); }
                else break;
            }
        }

        /* First check if it's a procedure in the current program */
        if (state.running && state.current_prog) {
            proc_line = find_procedure(state.current_prog, name);
            if (proc_line >= 0) {
                /* Call procedure within same program */
                if (push_frame(state.current_prog, proc_line + 1) < 0)
                    return;
                /* Store WITH args in the new frame */
                if (with_argc > 0 && state.call_depth > 0) {
                    call_frame_t *frame = &state.call_stack[state.call_depth - 1];
                    frame->with_argc = with_argc;
                    memcpy(frame->with_args, with_args, with_argc * sizeof(value_t));
                }
                return;
            }
        }

        /* Check SET PROCEDURE TO file */
        if (procedure_file) {
            proc_line = find_procedure(procedure_file, name);
            if (proc_line >= 0) {
                if (push_frame(procedure_file, proc_line + 1) < 0)
                    return;
                if (with_argc > 0 && state.call_depth > 0) {
                    call_frame_t *frame = &state.call_stack[state.call_depth - 1];
                    frame->with_argc = with_argc;
                    memcpy(frame->with_args, with_args, with_argc * sizeof(value_t));
                }
                return;
            }
        }

        /* Load external PRG file */
        prog = prog_load(name);
        if (!prog) return;

        if (state.running) {
            /* Nested DO from within a program */
            if (push_frame(prog, 0) < 0) {
                prog_free(prog);
                return;
            }
            /* Store WITH args in the new frame */
            if (with_argc > 0 && state.call_depth > 0) {
                call_frame_t *frame = &state.call_stack[state.call_depth - 1];
                frame->with_argc = with_argc;
                memcpy(frame->with_args, with_args, with_argc * sizeof(value_t));
            }
        } else {
            /* Interactive DO - push a frame so PARAMETERS can find WITH args */
            state.current_prog = prog;
            state.pc = 0;
            state.call_depth = 0;
            state.loop_depth = 0;
            if_skip = 0;
            if_depth = 0;
            case_active = 0;
            case_done = 0;
            case_skip = 0;
            if (with_argc > 0) {
                /* Push a frame at depth 0 for the interactive DO */
                if (push_frame(prog, 0) == 0) {
                    call_frame_t *frame = &state.call_stack[state.call_depth - 1];
                    frame->with_argc = with_argc;
                    memcpy(frame->with_args, with_args, with_argc * sizeof(value_t));
                    frame->caller_prog = NULL;
                }
            }
            prog_run();
        }
    }
}

/* ---- IF ---- */
void prog_if(const char *arg) {
    value_t val;
    expr_ctx_t *ctx = cmd_get_expr_ctx();

    if (!state.running) {
        printf("IF not allowed in interactive mode.\n");
        return;
    }

    if (expr_eval_str(ctx, arg, &val) != 0) {
        if (ctx->error) prog_error(expr_error_code(ctx->error), ctx->error);
        state.pc++;
        return;
    }

    if_depth++;
    if (if_depth > MAX_IF_DEPTH) {
        printf("IF nesting too deep.\n");
        if_depth--;
        state.pc++;
        return;
    }

    if (val.type == VAL_LOGIC && val.logic) {
        /* Condition true, execute IF block */
        if_done[if_depth - 1] = 1;
        state.pc++;
    } else {
        /* Condition false, skip to ELSE or ENDIF */
        if_done[if_depth - 1] = 0;
        if_skip = 1;
        state.pc++;
    }
}

/* ---- ELSE ---- */
void prog_else(void) {
    if (!state.running) {
        printf("ELSE without IF.\n");
        return;
    }

    /* We were executing the IF-true block, now skip to ENDIF */
    if_skip = 1;
    /* Mark as done so ELSE branch won't also execute */
    if (if_depth > 0)
        if_done[if_depth - 1] = 1;
    state.pc++;
}

/* ---- ENDIF ---- */
void prog_endif(void) {
    if (!state.running) {
        printf("ENDIF without IF.\n");
        return;
    }

    if (if_depth > 0)
        if_depth--;
    state.pc++;
}

/* ---- DO CASE ---- */
void prog_do_case(void) {
    if (!state.running) {
        printf("DO CASE not allowed in interactive mode.\n");
        return;
    }

    case_active = 1;
    case_done = 0;
    case_skip = 1; /* skip until first CASE */
    state.pc++;
}

/* ---- CASE ---- */
void prog_case(const char *arg) {
    if (!state.running || !case_active) {
        printf("CASE without DO CASE.\n");
        return;
    }

    if (case_done) {
        /* A previous CASE was taken, skip to ENDCASE */
        case_skip = 1;
        state.pc++;
        return;
    }

    {
        value_t val;
        expr_ctx_t *ctx = cmd_get_expr_ctx();
        if (expr_eval_str(ctx, arg, &val) != 0) {
            if (ctx->error) prog_error(expr_error_code(ctx->error), ctx->error);
            state.pc++;
            return;
        }

        if (val.type == VAL_LOGIC && val.logic) {
            case_done = 1;
            case_skip = 0;
            state.pc++;
        } else {
            case_skip = 1;
            state.pc++;
        }
    }
}

/* ---- OTHERWISE ---- */
void prog_otherwise(void) {
    if (!state.running || !case_active) {
        printf("OTHERWISE without DO CASE.\n");
        return;
    }

    if (case_done) {
        case_skip = 1;
    } else {
        case_done = 1;
        case_skip = 0;
    }
    state.pc++;
}

/* ---- ENDCASE ---- */
void prog_endcase(void) {
    if (!state.running) {
        printf("ENDCASE without DO CASE.\n");
        return;
    }

    case_active = 0;
    case_done = 0;
    case_skip = 0;
    state.pc++;
}

/* ---- ENDDO ---- */
void prog_enddo(void) {
    expr_ctx_t *ctx;
    value_t val;

    if (!state.running) {
        printf("ENDDO without DO WHILE.\n");
        return;
    }

    if (state.loop_depth <= 0) {
        printf("ENDDO without DO WHILE.\n");
        state.pc++;
        return;
    }

    /* Re-evaluate condition */
    ctx = cmd_get_expr_ctx();
    if (expr_eval_str(ctx, state.loop_stack[state.loop_depth - 1].condition, &val) != 0) {
        if (ctx->error) prog_error(expr_error_code(ctx->error), ctx->error);
        state.loop_depth--;
        state.pc++;
        return;
    }

    if (val.type == VAL_LOGIC && val.logic) {
        /* Loop back to start (line after DO WHILE) */
        state.pc = state.loop_stack[state.loop_depth - 1].start_line + 1;
    } else {
        /* Exit loop */
        state.loop_depth--;
        state.pc++;
    }
}

/* ---- LOOP ---- */
void prog_loop(void) {
    loop_entry_t *loop;

    if (!state.running || state.loop_depth <= 0) {
        printf("LOOP without DO WHILE/FOR.\n");
        return;
    }

    loop = &state.loop_stack[state.loop_depth - 1];

    if (loop->is_for) {
        /* FOR loop: jump to the NEXT line so it handles increment/check */
        int target = scan_next(state.current_prog, loop->start_line);
        if (target >= 0) {
            state.pc = target;
        } else {
            printf("NEXT not found.\n");
            state.pc++;
        }
    } else {
        /* DO WHILE loop: pop and re-evaluate at DO WHILE line */
        state.loop_depth--;
        state.pc = state.loop_stack[state.loop_depth].start_line;
    }
}

/* ---- EXIT (from loop) ---- */
void prog_exit_loop(void) {
    loop_entry_t *loop;
    int target;

    if (!state.running || state.loop_depth <= 0) {
        printf("EXIT without DO WHILE/FOR.\n");
        return;
    }

    loop = &state.loop_stack[state.loop_depth - 1];

    if (loop->is_for) {
        target = scan_next(state.current_prog, loop->start_line);
    } else {
        target = scan_enddo(state.current_prog, loop->start_line);
    }
    state.loop_depth--;

    if (target >= 0) {
        state.pc = target + 1;
    } else {
        printf("%s not found.\n", loop->is_for ? "NEXT" : "ENDDO");
        state.pc++;
    }
}

/* ---- FOR ---- */
void prog_for(const char *arg) {
    const char *p = skip_ws(arg);
    char varname[MEMVAR_NAMELEN];
    value_t start_val, end_val, step_val;
    expr_ctx_t *ctx = cmd_get_expr_ctx();
    memvar_store_t *store = cmd_get_memvar_store();
    int i = 0;

    if (!state.running) {
        printf("FOR not allowed in interactive mode.\n");
        return;
    }

    /* Parse variable name */
    while (is_ident_char(*p) && i < MEMVAR_NAMELEN - 1)
        varname[i++] = *p++;
    varname[i] = '\0';

    p = skip_ws(p);
    if (*p != '=') {
        printf("Syntax error in FOR.\n");
        state.pc++;
        return;
    }
    p++;

    /* Parse start expression */
    p = skip_ws(p);
    if (expr_eval(ctx, &p, &start_val) != 0 || start_val.type != VAL_NUM) {
        printf("FOR requires numeric expressions.\n");
        state.pc++;
        return;
    }

    /* Expect TO */
    p = skip_ws(p);
    if (!str_imatch(p, "TO")) {
        printf("Missing TO in FOR.\n");
        state.pc++;
        return;
    }
    p = skip_ws(p + 2);

    /* Parse end expression */
    if (expr_eval(ctx, &p, &end_val) != 0 || end_val.type != VAL_NUM) {
        printf("FOR requires numeric expressions.\n");
        state.pc++;
        return;
    }

    /* Optional STEP */
    step_val = val_num(1.0);
    p = skip_ws(p);
    if (str_imatch(p, "STEP")) {
        p = skip_ws(p + 4);
        if (expr_eval(ctx, &p, &step_val) != 0 || step_val.type != VAL_NUM) {
            printf("STEP requires numeric expression.\n");
            state.pc++;
            return;
        }
    }

    /* Set loop variable to start value */
    memvar_set(store, varname, &start_val);

    /* Check if loop should execute at all */
    if ((step_val.num > 0 && start_val.num > end_val.num) ||
        (step_val.num < 0 && start_val.num < end_val.num)) {
        int target = scan_next(state.current_prog, state.pc);
        if (target < 0) {
            printf("NEXT not found.\n");
            state.pc++;
            return;
        }
        state.pc = target + 1;
        return;
    }

    /* Push loop entry */
    if (state.loop_depth >= MAX_LOOP_DEPTH) {
        printf("Loop nesting too deep.\n");
        state.pc++;
        return;
    }

    state.loop_stack[state.loop_depth].start_line = state.pc;
    state.loop_stack[state.loop_depth].is_for = 1;
    state.loop_stack[state.loop_depth].condition[0] = '\0';
    str_copy(state.loop_stack[state.loop_depth].varname, varname, MEMVAR_NAMELEN);
    state.loop_stack[state.loop_depth].end_val = end_val.num;
    state.loop_stack[state.loop_depth].step_val = step_val.num;
    state.loop_depth++;
    state.pc++;
}

/* ---- NEXT ---- */
void prog_next(void) {
    loop_entry_t *loop;
    memvar_store_t *store;
    value_t val;
    int done;

    if (!state.running || state.loop_depth <= 0) {
        printf("NEXT without FOR.\n");
        if (state.running) state.pc++;
        return;
    }

    loop = &state.loop_stack[state.loop_depth - 1];
    if (!loop->is_for) {
        printf("NEXT without FOR.\n");
        state.pc++;
        return;
    }

    /* Increment variable */
    store = cmd_get_memvar_store();
    if (memvar_find(store, loop->varname, &val) != 0 || val.type != VAL_NUM) {
        state.loop_depth--;
        state.pc++;
        return;
    }

    val.num += loop->step_val;
    memvar_set(store, loop->varname, &val);

    /* Check termination */
    if (loop->step_val > 0)
        done = (val.num > loop->end_val);
    else if (loop->step_val < 0)
        done = (val.num < loop->end_val);
    else
        done = 0; /* STEP 0 = infinite loop */

    if (done) {
        state.loop_depth--;
        state.pc++;
    } else {
        state.pc = loop->start_line + 1;
    }
}

/* ---- RETURN ---- */
void prog_return(const char *arg) {
    if (!state.running) {
        printf("RETURN not in program.\n");
        return;
    }

    /* RETURN <expr> for FUNCTION — evaluate the return value */
    if (arg && *arg) {
        expr_ctx_t *ctx = cmd_get_expr_ctx();
        value_t val;
        if (expr_eval_str(ctx, arg, &val) == 0) {
            udf_return_val = val;
        }
    }

    if (state.call_depth > 0) {
        /* If this is a UDF call returning to the UDF entry depth, stop */
        if (udf_active && state.call_depth <= udf_depth) {
            pop_frame();
            state.running = 0;
            return;
        }
        pop_frame();
    } else {
        /* Top-level RETURN = end program */
        state.running = 0;
        if (state.current_prog) {
            prog_free(state.current_prog);
            state.current_prog = NULL;
        }
    }
}

/* ---- PROCEDURE ---- */
void prog_procedure(const char *arg) {
    (void)arg;
    /* During normal execution, skip past procedure body to next PROCEDURE or EOF */
    if (!state.running) {
        printf("PROCEDURE not allowed in interactive mode.\n");
        return;
    }
    /* This is handled in prog_run's main loop */
    state.pc++;
}

/* ---- PARAMETERS ---- */
void prog_parameters(const char *arg) {
    const char *p = skip_ws(arg);
    memvar_store_t *store = cmd_get_memvar_store();
    call_frame_t *frame = (state.call_depth > 0) ?
        &state.call_stack[state.call_depth - 1] : NULL;
    char name[MEMVAR_NAMELEN];
    int i, param_idx = 0;

    if (!state.running) {
        printf("PARAMETERS not allowed in interactive mode.\n");
        return;
    }

    while (*p) {
        i = 0;
        while (is_ident_char(*p) && i < MEMVAR_NAMELEN - 1)
            name[i++] = *p++;
        name[i] = '\0';

        if (name[0] != '\0') {
            /* Save current value as PRIVATE */
            if (frame) {
                save_private(frame, name);
            }
            /* Bind from WITH argument if available, otherwise NIL */
            if (frame && param_idx < frame->with_argc) {
                memvar_set(store, name, &frame->with_args[param_idx]);
            } else {
                value_t nil = val_nil();
                memvar_set(store, name, &nil);
            }
            param_idx++;
        }

        p = skip_ws(p);
        if (*p == ',') { p++; p = skip_ws(p); }
        else break;
    }

    state.pc++;
}

/* ---- PRIVATE ---- */
void prog_private(const char *arg) {
    const char *p = skip_ws(arg);
    memvar_store_t *store = cmd_get_memvar_store();
    char name[MEMVAR_NAMELEN];
    int i;

    if (!state.running || state.call_depth <= 0) {
        /* In interactive mode or top level, PRIVATE is ignored */
        return;
    }

    while (*p) {
        i = 0;
        while (is_ident_char(*p) && i < MEMVAR_NAMELEN - 1)
            name[i++] = *p++;
        name[i] = '\0';

        if (name[0] != '\0') {
            save_private(&state.call_stack[state.call_depth - 1], name);
            /* Release the current value - create fresh */
            memvar_release(store, name);
        }

        p = skip_ws(p);
        if (*p == ',') { p++; p = skip_ws(p); }
        else break;
    }

    state.pc++;
}

/* ---- PUBLIC ---- */
void prog_public(const char *arg) {
    const char *p = skip_ws(arg);
    memvar_store_t *store = cmd_get_memvar_store();
    char name[MEMVAR_NAMELEN];
    int i;

    while (*p) {
        value_t existing;
        i = 0;
        while (is_ident_char(*p) && i < MEMVAR_NAMELEN - 1)
            name[i++] = *p++;
        name[i] = '\0';

        if (name[0] != '\0') {
            /* Create variable if it doesn't exist */
            if (memvar_find(store, name, &existing) != 0) {
                value_t v = val_logic(0); /* PUBLIC vars default to .F. */
                memvar_set(store, name, &v);
            }
        }

        p = skip_ws(p);
        if (*p == ',') { p++; p = skip_ws(p); }
        else break;
    }

    if (state.running) state.pc++;
}

/* ---- CANCEL ---- */
void prog_cancel(void) {
    /* Abort all program execution */
    while (state.call_depth > 0) {
        restore_privates(&state.call_stack[state.call_depth - 1]);
        state.call_depth--;
    }
    state.running = 0;
    state.loop_depth = 0;
    if_skip = 0;
    if_depth = 0;
    case_active = 0;
    case_done = 0;
    case_skip = 0;
    if (state.current_prog) {
        prog_free(state.current_prog);
        state.current_prog = NULL;
    }
    printf("*** Cancelled\n");
}

/* ---- Execute one line in program context ---- */
int prog_execute_line(char *line) {
    char *p = skip_ws(line);
    int result;

    /* Check for program-specific commands first */
    if (str_imatch(p, "DO")) {
        prog_do(p + 2);
        return 0;
    }

    if (str_imatch(p, "IF")) {
        prog_if(skip_ws(p + 2));
        return 0;
    }

    if (str_imatch(p, "ELSE")) {
        prog_else();
        return 0;
    }

    if (str_imatch(p, "ENDIF")) {
        prog_endif();
        return 0;
    }

    if (str_imatch(p, "ENDDO")) {
        prog_enddo();
        return 0;
    }

    if (str_imatch(p, "FOR")) {
        prog_for(skip_ws(p + 3));
        return 0;
    }

    if (str_imatch(p, "NEXT")) {
        prog_next();
        return 0;
    }

    if (str_imatch(p, "LOOP")) {
        prog_loop();
        return 0;
    }

    /* EXIT in loop context */
    if (str_imatch(p, "EXIT")) {
        if (state.loop_depth > 0) {
            prog_exit_loop();
            return 0;
        }
        /* Otherwise fall through to cmd_execute (QUIT/EXIT) */
    }

    if (str_imatch(p, "RETURN")) {
        prog_return(skip_ws(p + 6));
        return 0;
    }

    if (str_imatch(p, "FUNCTION")) {
        /* FUNCTION treated same as PROCEDURE during execution — skip body */
        prog_procedure(skip_ws(p + 8));
        return 0;
    }

    if (str_imatch(p, "PROCEDURE")) {
        prog_procedure(skip_ws(p + 9));
        return 0;
    }

    if (str_imatch(p, "PARAMETERS")) {
        prog_parameters(skip_ws(p + 10));
        return 0;
    }

    if (str_imatch(p, "PRIVATE")) {
        prog_private(skip_ws(p + 7));
        return 0;
    }

    if (str_imatch(p, "PUBLIC")) {
        prog_public(skip_ws(p + 6));
        return 0;
    }

    if (str_imatch(p, "CANCEL")) {
        prog_cancel();
        return 0;
    }

    /* ON ERROR DO <proc> / ON ERROR */
    if (str_imatch(p, "ON") && str_imatch(skip_ws(p + 2), "ERROR")) {
        char *rest = skip_ws(skip_ws(p + 2) + 5);
        if (str_imatch(rest, "DO")) {
            rest = skip_ws(rest + 2);
            char procname[64];
            int i = 0;
            while (is_ident_char(*rest) && i < 63)
                procname[i++] = *rest++;
            procname[i] = '\0';
            prog_on_error(procname);
        } else {
            prog_on_error(NULL);
        }
        state.pc++;
        return 0;
    }

    if (str_imatch(p, "RETRY")) {
        prog_retry();
        state.pc++;
        return 0;
    }

    if (str_imatch(p, "SUSPEND")) {
        state.pc++;  /* advance past SUSPEND before pausing */
        prog_suspend();
        return 0;
    }

    if (str_imatch(p, "CASE")) {
        prog_case(skip_ws(p + 4));
        return 0;
    }

    if (str_imatch(p, "OTHERWISE")) {
        prog_otherwise();
        return 0;
    }

    if (str_imatch(p, "ENDCASE")) {
        prog_endcase();
        return 0;
    }

    /* NOTE comment */
    if (str_imatch(p, "NOTE")) {
        state.pc++;
        return 0;
    }

    /* Full-line comment */
    if (*p == '*') {
        state.pc++;
        return 0;
    }

    /* Regular command - delegate to cmd_execute */
    result = cmd_execute(NULL, line);
    state.pc++;
    return result;
}
