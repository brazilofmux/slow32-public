#ifndef SBASIC_STRICT_H
#define SBASIC_STRICT_H

/* The compilable dialect's checker (docs/plans/basic-compiler.md, "a
 * compiler that decides up front").  The interpreter runs every
 * program it can parse; a compiler that fixes a program's shape before
 * it runs needs that shape to be in the text.  This pass finds what is
 * not: a NEXT with no FOR before it, a FOR no NEXT closes, an array used
 * before its DIM or DIMmed twice, a DEF FN used before it is defined, a
 * label defined twice or not at all -- QuickBASIC's load-time checks,
 * mostly.  Each finding has a code (next-without-for, ...).  It also
 * counts what a compiler can do from the text but pays for or must take
 * care with: GOSUB, ON ERROR, RESUME, ERL, jumps into and out of a FOR,
 * a NEXT closing a FOR from a block inside it.
 *
 *   sbasic -check prog.bas    the findings, and the counts; exit 1 if any
 *   sbasic -strict prog.bas   run it only if there are none
 */

#include "ast.h"

enum {
    STRICT_GOSUB, STRICT_ON_ERROR, STRICT_RESUME, STRICT_RESUME_NEXT,
    STRICT_ERL, STRICT_CLEAR, STRICT_GOTO_OUT_OF_FOR,
    STRICT_GOTO_INTO_FOR, STRICT_NEXT_IN_BLOCK, STRICT_NEXT_SKIPS,
    STRICT_NINFO
};

typedef void (*strict_report_fn)(void *ctx, int line, const char *code, const char *detail);

/* the findings' count; each reported through report (may be NULL), and
 * the counts of what is paid for into info (may be NULL) */
int strict_check(stmt_t *program, strict_report_fn report, void *ctx, int info[STRICT_NINFO]);
const char *strict_info_name(int i);

#endif
