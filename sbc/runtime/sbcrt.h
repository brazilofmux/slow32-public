/* What a compiled SLOW BASIC program (s32-sbc's C) is linked with: the
 * interpreter's runtime, whose executor runs what the program does not
 * do itself. */
#ifndef SBCRT_H
#define SBCRT_H

#include "eval.h"

/* Parse the program's source (embedded in it), lower it as s32-sbc did,
 * check the main unit has nops ops, and run it with main_fn. */
int sbcrt_main(const char *source, int nops, main_fn_t main_fn);

/* the runtime's ops no longer the ones the C was written from */
error_t sbcrt_mismatch(void);

#endif
