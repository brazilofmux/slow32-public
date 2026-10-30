#ifndef MEMOEDIT_H
#define MEMOEDIT_H

#include "expr.h"

/* MEMOEDIT([cString], [nTop], [nLeft], [nBottom], [nRight], [lEditMode],
 * [cUserFunction], [nLineLength], [nTabSize], [nTextBufferRow],
 * [nTextBufferColumn], [nWindowRow], [nWindowColumn]) --> cTextBuffer */
int fn_memoedit(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);

#endif
