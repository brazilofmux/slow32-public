#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "program.h"
#include "command.h"
#include "util.h"

static prog_state_t state;

/* IF/ELSE/ENDIF nesting for skip mode */
#define MAX_IF_DEPTH 32
static int if_skip;             /* >0 = skipping lines */
static int if_depth;            /* current IF nesting depth */
static int if_done[MAX_IF_DEPTH]; /* branch already taken at this depth */

/* DO CASE state */
static int case_active;         /* inside DO CASE block */
static int case_done;           /* a CASE branch has been taken */
static int case_skip;           /* skipping until matching CASE/OTHERWISE/ENDCASE */

void prog_init(void) {
    memset(&state, 0, sizeof(state));
    if_skip = 0;
    if_depth = 0;
    case_active = 0;
    case_done = 0;
    case_skip = 0;
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
    str_upper(path);
    /* Add .PRG extension if not present */
    if (strlen(path) < 4 || str_icmp(path + strlen(path) - 4, ".PRG") != 0) {
        if (strlen(path) + 4 < sizeof(path))
            strcat(path, ".PRG");
    }

    fp = fopen(path, "r");
    if (!fp) {
        printf("File not found: %s\n", path);
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

/* ---- Find PROCEDURE <name> in program ---- */
static int find_procedure(program_t *prog, const char *name) {
    int i;
    for (i = 0; i < prog->nlines; i++) {
        char *p = skip_ws(prog->lines[i]);
        if (str_imatch(p, "PROCEDURE")) {
            char *rest = skip_ws(p + 9);
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

/* ---- Scan forward for matching control structure ---- */
/* Scan from line `from` for matching ELSE/ENDIF at the same nesting level */
static int scan_if(program_t *prog, int from, int want_else) {
    int depth = 1;
    int i;
    for (i = from + 1; i < prog->nlines; i++) {
        char *p = skip_ws(prog->lines[i]);
        if (str_imatch(p, "IF")) {
            depth++;
        } else if (str_imatch(p, "ENDIF")) {
            depth--;
            if (depth == 0) return i;
        } else if (depth == 1 && want_else && str_imatch(p, "ELSE")) {
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
        char *p = skip_ws(prog->lines[i]);
        if (str_imatch(p, "DO") && !str_imatch(skip_ws(p + 2), "CASE")) {
            /* Could be DO WHILE (nesting) or DO <file> - check for WHILE */
            char *rest = skip_ws(p + 2);
            if (str_imatch(rest, "WHILE"))
                depth++;
        } else if (str_imatch(p, "ENDDO")) {
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
        char *p = skip_ws(prog->lines[i]);
        if (str_imatch(p, "DO") && str_imatch(skip_ws(p + 2), "CASE"))
            depth++;
        else if (str_imatch(p, "ENDCASE")) {
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

        /* Skip PROCEDURE definitions during normal execution */
        if (str_imatch(p, "PROCEDURE")) {
            /* Skip to next PROCEDURE or end of file */
            int i;
            for (i = state.pc + 1; i < state.current_prog->nlines; i++) {
                char *lp = skip_ws(state.current_prog->lines[i]);
                if (str_imatch(lp, "PROCEDURE"))
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
            if (str_imatch(p, "IF")) {
                if_skip++;
            } else if (str_imatch(p, "ENDIF")) {
                if_skip--;
                if (if_skip == 0) {
                    /* We've found matching ENDIF */
                    if (if_depth > 0) if_depth--;
                }
            } else if (if_skip == 1 && str_imatch(p, "ELSE")) {
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
            if (str_imatch(p, "DO") && str_imatch(skip_ws(p + 2), "CASE")) {
                /* nested DO CASE - skip the whole thing */
                int target = scan_endcase(state.current_prog, state.pc);
                if (target >= 0) {
                    state.pc = target + 1;
                    continue;
                }
            } else if (str_imatch(p, "CASE")) {
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
            } else if (str_imatch(p, "OTHERWISE")) {
                if (!case_done) {
                    case_skip = 0;
                    case_done = 1;
                }
                state.pc++;
                continue;
            } else if (str_imatch(p, "ENDCASE")) {
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
            if (ctx->error) printf("Error: %s\n", ctx->error);
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

    /* DO <file> or DO <procedure> */
    {
        char name[64];
        int i = 0;
        program_t *prog;
        int proc_line;

        while (*p && *p != ' ' && *p != '\t' && i < 63)
            name[i++] = *p++;
        name[i] = '\0';

        /* First check if it's a procedure in the current program */
        if (state.running && state.current_prog) {
            proc_line = find_procedure(state.current_prog, name);
            if (proc_line >= 0) {
                /* Call procedure within same program */
                if (push_frame(state.current_prog, proc_line + 1) < 0)
                    return;
                /* Don't advance PC since push_frame set it */
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
        } else {
            /* Interactive DO - start execution */
            state.current_prog = prog;
            state.pc = 0;
            state.call_depth = 0;
            state.loop_depth = 0;
            if_skip = 0;
            if_depth = 0;
            case_active = 0;
            case_done = 0;
            case_skip = 0;
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
        if (ctx->error) printf("Error: %s\n", ctx->error);
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
            if (ctx->error) printf("Error: %s\n", ctx->error);
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
        if (ctx->error) printf("Error: %s\n", ctx->error);
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
    if (!state.running || state.loop_depth <= 0) {
        printf("LOOP without DO WHILE.\n");
        return;
    }

    /* Jump back to line after DO WHILE (re-evaluation happens at ENDDO,
       but LOOP goes to start of loop body which is after DO WHILE line) */
    state.pc = state.loop_stack[state.loop_depth - 1].start_line;
    /* Actually, in dBase III, LOOP re-evaluates the condition.
       Let's jump to the DO WHILE line so it gets re-evaluated. */
    /* Wait - the DO WHILE line will push another loop entry. Let's handle
       this by jumping to start_line (the DO WHILE line) and popping
       the current loop entry, so DO WHILE will re-push it. */
    state.loop_depth--;
    state.pc = state.loop_stack[state.loop_depth].start_line;
}

/* ---- EXIT (from loop) ---- */
void prog_exit_loop(void) {
    int target;

    if (!state.running || state.loop_depth <= 0) {
        printf("EXIT without DO WHILE.\n");
        return;
    }

    /* Find ENDDO and jump past it */
    target = scan_enddo(state.current_prog,
                        state.loop_stack[state.loop_depth - 1].start_line);
    state.loop_depth--;

    if (target >= 0) {
        state.pc = target + 1;
    } else {
        printf("ENDDO not found.\n");
        state.pc++;
    }
}

/* ---- RETURN ---- */
void prog_return(void) {
    if (!state.running) {
        printf("RETURN not in program.\n");
        return;
    }

    if (state.call_depth > 0) {
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
    char name[MEMVAR_NAMELEN];
    int i;

    if (!state.running) {
        printf("PARAMETERS not allowed in interactive mode.\n");
        return;
    }

    /* Bind parameter values from caller's WITH clause */
    /* For now, PARAMETERS just declares the variable names as PRIVATE.
       The DO <proc> WITH <args> form would pass values, but basic
       dBase III uses PARAMETERS to name vars that get values from DO WITH.
       For simplicity, just create them as empty PRIVATE vars. */
    while (*p) {
        i = 0;
        while (is_ident_char(*p) && i < MEMVAR_NAMELEN - 1)
            name[i++] = *p++;
        name[i] = '\0';

        if (name[0] != '\0') {
            /* Save as PRIVATE */
            if (state.call_depth > 0) {
                save_private(&state.call_stack[state.call_depth - 1], name);
            }
            /* Create fresh variable with NIL value (unless already set by WITH) */
            {
                value_t existing;
                if (memvar_find(store, name, &existing) != 0) {
                    value_t nil = val_nil();
                    memvar_set(store, name, &nil);
                }
            }
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
        prog_return();
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
