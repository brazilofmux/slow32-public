#ifndef SBASIC_STRICT_H
#define SBASIC_STRICT_H

/* The compilable dialect's checker (docs/plans/basic-compiler.md, "a
 * compiler that decides up front").  The interpreter runs every
 * program it can parse; a compiler that fixes a program's shape before
 * it runs needs that shape to be in the text.  This pass finds what is
 * not -- what QuickBASIC refuses when it loads a program (the rule, user
 * 2026-11-03, so any finding can be checked against QBasic 1.1 and QB64):
 * FOR and NEXT paired in their own block, labels defined once and used
 * defined, an array DIMmed once (again after an ERASE) and not used
 * before its DIM, a DEF FN defined once.  One deliberate difference: a
 * DEF FN used before its definition is fine here (the compiler defines
 * them all first).  Each finding has a code (next-without-for, ...).  It
 * also counts what a compiler can do from the text but pays for or must
 * take care with: GOSUB, ON ERROR, RESUME, ERL, jumps into and out of a
 * FOR, a DEF FN used before it is defined.
 *
 *   sbasic -check prog.bas    the findings, and the counts; exit 1 if any
 *   sbasic -strict prog.bas   run it only if there are none
 */

#include "ast.h"

enum {
    STRICT_GOSUB, STRICT_ON_ERROR, STRICT_RESUME, STRICT_RESUME_NEXT,
    STRICT_ERL, STRICT_CLEAR, STRICT_GOTO_OUT_OF_FOR,
    STRICT_GOTO_INTO_FOR, STRICT_FN_HOISTED,
    STRICT_NINFO
};

typedef void (*strict_report_fn)(void *ctx, int line, const char *code, const char *detail);

/* the findings' count; each reported through report (may be NULL), and
 * the counts of what is paid for into info (may be NULL) */
int strict_check(stmt_t *program, strict_report_fn report, void *ctx, int info[STRICT_NINFO]);
const char *strict_info_name(int i);

#endif
