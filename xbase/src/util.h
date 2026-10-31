#ifndef UTIL_H
#define UTIL_H

#include <stdio.h>

/* Console output, all through one place: whether it stands at the start
 * of a line, so that a message (text that ends a line) begins on a line of
 * its own when ? left one open -- as dBASE III and Clipper show them. */
int  out_printf(const char *fmt, ...);
int  out_at_line_start(void);
void out_set_line_start(int bol);
/* The screen alone, bypassing any route */
void out_console(const char *s);
/* While a console-stream command runs (LIST, DISPLAY), printf goes
 * through route -- the screen, the printer, the alternate file as the
 * SETs say; NULL puts it back on the screen */
void out_set_route(void (*route)(const char *s));
#ifndef UTIL_NO_PRINTF_WRAP
#define printf out_printf
#endif

/* In-place uppercase */
void str_upper(char *s);

/* Case-insensitive strcmp */
int str_icmp(const char *a, const char *b);

/* Case-insensitive prefix match with word boundary */
int str_imatch(const char *input, const char *prefix);

/* Return pointer past leading whitespace */
char *skip_ws(const char *s);

/* Trim trailing whitespace in place */
void trim_right(char *s);

/* Normalize path separators (backslash to forward slash) */
void path_normalize(char *s);

/* Read line from stdin, strip CR/LF. Returns length or -1 on EOF */
int read_line(char *buf, int size);

/* strncpy that always NUL-terminates */
void str_copy(char *dst, const char *src, int n);

/* Case-insensitive strncmp (compares up to n chars) */
int str_nicmp(const char *a, const char *b, int n);

/* Identifier character tests */
int is_ident_start(char c);
int is_ident_char(char c);

/* Case-insensitive wildcard match: '*' and '?' */
int str_like(const char *text, const char *pattern);

#endif
