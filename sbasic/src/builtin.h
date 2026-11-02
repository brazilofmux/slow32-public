#ifndef SBASIC_BUILTIN_H
#define SBASIC_BUILTIN_H

#include "value.h"
#include "error.h"

/* Look up a built-in function by name. Returns 1 if found, 0 if not.
   If found, evaluates and stores result in *out. */
error_t builtin_call(const char *name, value_t *args, int nargs, value_t *out);

/* Check if name is a known built-in function */
int builtin_exists(const char *name);

/* CHR$, LEFT$, RIGHT$ and MID$ of their arguments converted (MID$
 * without a count: has_count 0) -- the compiler calls them directly */
error_t sb_chr(int code, value_t *out);
error_t sb_left(sb_string_t *s, int n, value_t *out);
error_t sb_right(sb_string_t *s, int n, value_t *out);
error_t sb_mid(sb_string_t *s, int start, int count, int has_count, value_t *out);

/* TAB(n)'s spaces from PRINT's column (*nl: a new line first), SPC(n)'s */
int sb_tab_spaces(int n, int *nl);
int sb_spc_spaces(int n);

/* RND(n) (has_n 0: RND), a single's value */
double sb_rnd_of(double n, int has_n);

/* Set RNG seed (for RANDOMIZE statement) */
void builtin_randomize(double seed);
void builtin_rnd_reset(void);

#endif
