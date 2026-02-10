#ifndef PROGRAM_H
#define PROGRAM_H

#include "expr.h"
#include "memvar.h"

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
    value_t with_args[16];
    int with_argc;
} call_frame_t;

/* Loop stack entry (DO WHILE / ENDDO / FOR / NEXT) */
typedef struct {
    int start_line;
    int is_for;             /* 0 = DO WHILE, 1 = FOR */
    char condition[256];    /* DO WHILE condition string */
    /* FOR loop fields */
    char varname[MEMVAR_NAMELEN];
    double end_val;
    double step_val;
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
void prog_return(const char *arg);
void prog_procedure(const char *arg);
void prog_parameters(const char *arg);
void prog_private(const char *arg);
void prog_public(const char *arg);
void prog_for(const char *arg);
void prog_next(void);
void prog_cancel(void);

/* Check if program is running */
int prog_is_running(void);

/* SET PROCEDURE TO file */
void prog_set_procedure(const char *filename);

/* Get memvar store (for program.c to access command.c's store) */
memvar_store_t *prog_get_memvar_store(void);

/* Pre-process line: handle comments and macro substitution */
void prog_preprocess(char *line, memvar_store_t *store);

#endif
