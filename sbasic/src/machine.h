#ifndef SBASIC_MACHINE_H
#define SBASIC_MACHINE_H
/* machine.c: the PC's devices at their addresses and ports (no CPU) */
#include "error.h"
#include "value.h"
#include "array.h"

extern int def_seg;             /* DEF SEG's segment */
/* DEF SEG alone: the segment of the program's variables */
void pc_def_seg_home(void);

/* PEEK and POKE at DEF SEG's segment, offset (-32768..65535) */
error_t pc_peek(int offset, int *out);
error_t pc_poke(int offset, int v);
/* INP, OUT, WAIT */
error_t pc_inp(int port, int *out);
error_t pc_out(int port, int v);
error_t pc_wait(int port, int and_mask, int xor_mask);
/* a program's start */
void pc_reset(void);

/* --- the program's own storage at an address (VARSEG, VARPTR, SADD) ----
 * Each returns its segment and offset, made the first time it is asked
 * for.  byte: how far into it (a field of a record). */
enum {
    PC_VALUE,                   /* a value_t: the interpreter's variable */
    PC_INT, PC_LONG, PC_FLOAT, PC_DOUBLE,       /* compiled code's own variables */
    PC_STRING, PC_RECORD        /* ... a pointer to the string, to the record */
};
error_t pc_addr_var(int kind, void *p, int fixlen, int byte, int *seg, int *off);
/* element k (arr->data[k]) */
error_t pc_addr_elem(sb_array_t *arr, int k, int byte, int *seg, int *off);
/* SADD: a variable-length string's characters (kind PC_VALUE or PC_STRING;
 * an element is a PC_VALUE); an empty string's are at offset 0 */
error_t pc_addr_chars(int kind, void *p, int *seg, int *off);
/* the bytes before field f of record r */
int pc_field_at(const sb_record_t *r, int f);
/* the storage from lo up to hi is gone (an array, a scope); pc_windows is
 * how many are out, so nothing is looked for when there are none */
extern int pc_windows;
void pc_forget(const void *lo, const void *hi);

/* what a VARPTR$ names: the string at this offset of the variables' segment
 * (X in PLAY and DRAW), 0 if none; a number of its type code, 2 INTEGER or 4
 * SINGLE as = takes them, rounded, 0 for any other */
int pc_string_at(int off, const char **s, int *len);
int pc_var_number(int code, int off, long *out);

/* BSAVE and BLOAD's bytes: the file's name, and an address */
error_t pc_bsave(const char *file, int offset, int length);
error_t pc_bload(const char *file, int has_offset, int offset);

#endif
