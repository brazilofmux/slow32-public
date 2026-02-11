#ifndef PROGRAM_H
#define PROGRAM_H

#include "expr.h"
#include "memvar.h"
#include "clause.h"

struct lexer;

#define MAX_PROGRAM_LINES 2000
#define MAX_LINE_LEN      256
#define MAX_CALL_DEPTH     32
#define MAX_LOOP_DEPTH     16

/* A loaded program */
typedef struct {
    char **lines;           /* malloc'd array of strdup'd lines */
    int nlines;
    char filename[64];
} program_t;

/* Call stack frame */
typedef struct {
    program_t *prog;
    int return_line;
    int caller_line;
    program_t *caller_prog;
    /* PRIVATE variable scoping */
    char private_vars[64][MEMVAR_NAMELEN];
    int private_count;
    value_t saved_vals[64];
    int saved_valid[64];
    /* DO ... WITH arguments */
    value_t with_args[MAX_FUNC_ARGS];
    int with_argc;
} call_frame_t;

/* Loop stack entry (DO WHILE / ENDDO / FOR / NEXT / SCAN / ENDSCAN) */
typedef struct {
    int start_line;
    int type;               /* 0 = DO WHILE, 1 = FOR, 2 = SCAN */
    char condition[256];    /* DO WHILE condition string */
    struct ast_node *condition_ast;
    /* FOR loop fields */
    char varname[MEMVAR_NAMELEN];
    double end_val;
    double step_val;
    /* SCAN loop fields */
    clause_t scan_clause;
    uint32_t scan_current;
    uint32_t scan_end;
} loop_entry_t;

/* Program execution state */
typedef struct {
    program_t *current_prog;
    int pc;
    call_frame_t call_stack[MAX_CALL_DEPTH];
    int call_depth;
    loop_entry_t loop_stack[MAX_LOOP_DEPTH];
    int loop_depth;
    int running;
} prog_state_t;

/* Initialize program subsystem */
void prog_init(void);

/* Execute a DO command (file or WHILE) */
void prog_do(const char *arg);

/* Program-aware command execution - returns 1 if QUIT */
int prog_execute_line(char *line);

/* Control flow handlers (called from command dispatch) */
void prog_if(const char *arg);
void prog_else(void);
void prog_endif(void);
void prog_do_case(void);
void prog_case(const char *arg);
void prog_otherwise(void);
void prog_endcase(void);
void prog_enddo(void);
void prog_loop(void);
void prog_exit_loop(void);
void prog_scan(const char *arg);
void prog_endscan(void);
void prog_return(const char *arg);
void prog_procedure(struct lexer *l);
void prog_parameters(struct lexer *l);
void prog_private(struct lexer *l);
void prog_public(struct lexer *l);
void prog_for(const char *arg);
void prog_next(void);
void prog_cancel(void);

/* Check if program is running */
int prog_is_running(void);

/* Error handling */
#define ERR_FILE_NOT_FOUND  1
#define ERR_FILE_IO         3
#define ERR_RECORD_RANGE    5
#define ERR_VAR_NOT_FOUND  12
#define ERR_SYNTAX         14
#define ERR_TYPE_MISMATCH  15
#define ERR_UNRECOGNIZED   17
#define ERR_DIV_ZERO       21
#define ERR_NO_DATABASE    36

void prog_error(int code, const char *message);
int prog_get_error_code(void);
const char *prog_get_error_message(void);
int prog_get_lineno(void);
const char *prog_get_program_name(void);

/* ON ERROR DO / RETRY */
void prog_on_error(const char *procname);
void prog_retry(void);

/* SUSPEND / RESUME */
void prog_suspend(void);
void prog_resume(void);

/* SET PROCEDURE TO file */
void prog_set_procedure(const char *filename);

/* Get name of loaded procedure file (NULL if none) */
const char *prog_get_procedure_name(void);

/* Get memvar store (for program.c to access command.c's store) */
memvar_store_t *prog_get_memvar_store(void);

/* Pre-process line: handle comments and macro substitution */
void prog_preprocess(char *line, memvar_store_t *store);

#endif
