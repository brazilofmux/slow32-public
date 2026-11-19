#ifndef CODEPAGE_H
#define CODEPAGE_H

#include <stddef.h>
#include <stdio.h>

/* The program's codepage (docs/plans/xbase-utf8.md): what its strings
 * are in.  CP437 by default, as on the PC; the terminal is told the same
 * (screen.c, docs/SPEC.md 8.14.7).
 *
 * Program text is held in the program's codepage, so whatever reads it as
 * data (a prompt, TEXT, an index key's text) needs nothing.  It arrives
 * from outside in one of two: a program file, or a typed or piped line,
 * that is valid UTF-8 is UTF-8, anything else is CP437 (ruling 6). */

#define CP_437  437
#define CP_UTF8 65001

int cp_current(void);

/* Switch the program's codepage (SET CODEPAGE, XBASE_CODEPAGE): the C
 * library's character classes follow (LC_CTYPE C.UTF-8 or C).  The
 * terminal and the loaded program text are the callers' (set.c). */
void cp_set(int cp);

/* A codepage's name, and back: "CP437", "UTF8"; 0 for a name not known */
const char *cp_name(int cp);
int cp_by_name(const char *name);

/* Characters in the program's codepage: in CP437 a byte is one; in UTF-8
 * a code point is (ruling 4).  cp_chars counts them in s[0..n);
 * cp_offset is the byte where character k (0-based) starts, n past the
 * end; cp_decode reads the character at s (n >= 1 bytes there) into *cp
 * and returns its bytes; cp_encode writes one, returning its bytes (at
 * most 4). */
int cp_chars(const char *s, int n);
int cp_offset(const char *s, int n, int k);
int cp_decode(const char *s, int n, unsigned *cp);
int cp_encode(unsigned cp, char *out);

/* CP437 byte b as a character of the program's codepage: itself in
 * CP437, its code point in UTF-8 (the box-drawing set, CHR's table) */
int cp_char_437(unsigned char b);

/* Display columns: a character's (0, 1 or 2 in UTF-8: Unicode's widths,
 * s32utf.h; always 1 in CP437), and a string's s[0..n) */
int cp_width(unsigned cp);
int cp_columns(const char *s, int n);

/* Keys (ruling (a)): a character typed is its code point as a key, those
 * past U+00FF moved up by CP_KEY_HIGH so no key Clipper numbers (Alt-A
 * is 286) means two things; cp_key_char is the character a key stands
 * for, or -1 for one that is no character (an arrow, a function key) */
#define CP_KEY_HIGH 65536
int cp_key(unsigned cp);
int cp_key_char(int key);

/* Program text held in codepage from, as codepage to; malloc'd.  NULL when
 * nothing changes (ASCII, or the same codepage) or memory is short. */
char *cp_transcode(const char *s, int from, int to);

/* s holds bytes past 0x7F, and all of it is well-formed UTF-8 */
int cp_is_utf8(const char *s, size_t n);

/* A whole file's verdict, read through and rewound: 1 UTF-8, 0 not */
int cp_file_is_utf8(FILE *fp);

/* A line of text from outside, in place (size: its buffer), into the
 * program's codepage; from_utf8 says what it was in.  A character the
 * codepage lacks becomes '?'. */
void cp_source_line(char *line, int size, int from_utf8);

/* The same line of a program file into UTF-8 instead, whatever the
 * program's codepage: what xbc compiles from (its literals then reach
 * the program through cp_literal) */
void cp_line_to_utf8(char *line, int size, int from_utf8);

/* A string the compiler wrote as UTF-8 (xbc's literals with bytes past
 * 0x7F), as the program's codepage has it now; the result lasts as long
 * as the program (one conversion per literal and codepage). */
const char *cp_literal(const char *utf8);

#endif
