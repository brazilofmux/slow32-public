#ifndef GC_H
#define GC_H

/* The collector for what a program makes and lets go of: arrays, code
 * blocks, objects and the heap text of long strings.  Nothing was ever
 * freed before (a running DBU filled a 64 MB heap in minutes).
 *
 * Conservative roots, precise contents.  Every such allocation is
 * registered here.  At a statement boundary (gc_safepoint), when enough
 * has been allocated since the last collection, the data and bss
 * segments and the C stack (registers spilled first) are scanned for
 * words that are registered addresses; what they name is marked, and
 * marking follows the values inside arrays, blocks and objects.  The
 * rest is freed.  No root list to keep: the variable store, the compiled
 * scopes, the interpreter's frames, a built-in's argument array and a
 * compiled statement's temporaries all live in those segments or on
 * that stack.  The one kind of holder neither scan sees is a heap buffer
 * of values that a C frame owns for a moment (the evaluators' argument
 * arrays): those are pushed as roots while they live.
 *
 * Only the SLOW-32 build collects (the segment and stack bounds are the
 * linker's symbols); a host build registers and never frees, as before. */

#include <stddef.h>

enum { GC_ARRAY = 1, GC_BLOCK, GC_OBJECT, GC_TEXT };

void gc_register(void *p, int kind, size_t bytes);   /* a new allocation of that kind */
void gc_pin(void *p);                                /* never collected (a parser's literal) */
void gc_push_roots(void *values, int n);             /* n value_t in a heap buffer a C frame holds */
void gc_pop_roots(void);
void gc_safepoint(void);                             /* a statement boundary */
void gc_collect(void);                               /* now */
void gc_set_stack_base(void *p);                     /* the deepest frame to scan up to */
unsigned long gc_registered(void);                   /* live registered allocations */

#endif
