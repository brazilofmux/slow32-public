#ifndef LOWER_H
#define LOWER_H

/* A program unit -- the main program, or one SUB or FUNCTION body --
 * lowered from its statement tree to a flat list of operations with
 * explicit jumps.  A label is an index into the list, so GOTO, GOSUB,
 * RETURN and RESUME reach a statement however deeply it is nested, and
 * a loop's state lives in its control variable and a slot of the
 * unit's activation, not on the C stack: a GOTO may leave a FOR and
 * come back to its NEXT, as line-numbered BASIC does. */

#include "ast.h"
#include "error.h"

typedef enum {
    OP_STMT,     /* s: one simple statement */
    OP_JUMP,     /* to target */
    OP_JFALSE,   /* cond false (true, when invert) -> target, else on */
    OP_FOR,      /* s: push its loop's frame; when it runs no times ->
                    target (past its NEXT in the text) */
    OP_NEXT,     /* s (the FOR it closes in the text, at op slot), invert
                    1 when it names no variable: step the frame of that
                    variable (or the top one) and go back into its body
                    while it runs */
    OP_EXITFOR,  /* pop the top frame -> target (past its FOR's NEXT) */
    OP_SELECT    /* s: to targets[the clause that matches], or the last */
} op_kind_t;

typedef struct {
    op_kind_t kind;
    int line;
    int basic_line;     /* the line number before it in the source (ERL), or 0 */
    stmt_t *s;
    expr_t *cond;
    int invert;
    int target;
    int slot;
    int *targets;
} op_t;

typedef struct {
    char name[64];
    int index;
} code_label_t;

typedef struct code {
    op_t *ops;
    int nops, cap;
    code_label_t *labels;
    int nlabels, lcap;
    int *lhash;         /* labels by name: open, of label index + 1 */
    int lhcap;          /* a power of two, at least twice nlabels */
    int nfor;           /* FOR slots an activation needs */
} code_t;

/* the statement list, following ->next; *one: just the first */
code_t *lower_stmts(stmt_t *list, error_t *err);
code_t *lower_one(stmt_t *s, error_t *err);
void code_free(code_t *c);
int code_find_label(const code_t *c, const char *name);

#endif
