#ifndef GETSYS_H
#define GETSYS_H

/* Clipper's Get system and TBrowse, as objects (expr.h's object_t) with
 * methods reached through the : send operator, and the screen and READ
 * functions a Clipper program leans on.  The @ SAY/GET/READ machinery
 * stays screen.c's; a Get object rides beside each GET it records. */

#include "expr.h"

/* The send operator on any object: an exported variable, or a method of
 * the object's class (a message without parentheses that names a method
 * calls it, as Clipper's does).  -1 with ctx->error when it has neither. */
int object_send(expr_ctx_t *ctx, const value_t *base, const char *name, value_t *args, int nargs,
                int is_call, value_t *result);

/* A Get object for a GET that @ ... GET recorded (its variable by name);
 * appended to the GETLIST array when the program keeps one */
value_t getsys_note_get(int row, int col, const char *varname, const char *picture,
                        const char *valid_expr, const char *when_expr);

/* The functions, for func.c's table */
int fn_getnew(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int getsys_read_through_program(void);   /* READ via a program's ReadModal(), when it has one */
int getsys_has_block(const value_t *obj);          /* the Get reads and writes through a block */
int getsys_varget(const value_t *obj, value_t *out);
int getsys_varput(const value_t *obj, const value_t *v);
int fn_readmodal(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_readvar(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_updated(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_readexit(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_readinsert(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_readkill(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_getactive(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_setpos(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_dispout(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_maxrow(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_maxcol(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_setcolor(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_setcursor(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_scroll(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_dispbox(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_savescreen(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_restscreen(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_setkey(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_outstd(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_outerr(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_noop_nil(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_iscolor(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_tbrowsenew(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_tbrowsedb(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_tbcolumnnew(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_dbgotop(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_dbgobottom(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_dbskip(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
int fn_dbskipper(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);

#endif
