/* The collector: see gc.h.  The registry is an open-addressing hash set of
 * allocation addresses; marking runs an explicit stack through the values
 * inside arrays, blocks and objects; sweeping rebuilds the set from the
 * survivors. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <setjmp.h>
#include "gc.h"
#include "expr.h"
#include "memvar.h"
#include "ast.h"

typedef struct {
    void *p;
    size_t bytes;
    unsigned char kind, mark, pinned;
} gc_entry_t;

static gc_entry_t *table;
static size_t cap, count;              /* slots, entries */
static size_t bytes_live, bytes_since; /* registered bytes: all live, since the last collection */
static size_t threshold = 4u << 20;    /* collect once this much is new */
static int trace = -1;
static char *stack_base;

/* explicit roots: heap buffers of values a C frame holds for a moment */
#define ROOT_MAX 64
static struct { value_t *v; int n; } roots[ROOT_MAX];
static int nroots;

static size_t hash_ptr(const void *p) {
    uintptr_t x = (uintptr_t)p;
    x ^= x >> 16; x *= 0x45d9f3bu; x ^= x >> 13;
    return (size_t)x;
}

static gc_entry_t *lookup(const void *p) {
    size_t i;
    if (!cap) return NULL;
    i = hash_ptr(p) & (cap - 1);
    for (;;) {
        if (!table[i].p) return NULL;
        if (table[i].p == p) return &table[i];
        i = (i + 1) & (cap - 1);
    }
}

static void insert_raw(gc_entry_t *t, size_t c, const gc_entry_t *e) {
    size_t i = hash_ptr(e->p) & (c - 1);
    while (t[i].p) i = (i + 1) & (c - 1);
    t[i] = *e;
}

static void grow(void) {
    size_t ncap = cap ? cap * 2 : 1024, i;
    gc_entry_t *nt = calloc(ncap, sizeof(gc_entry_t));
    if (!nt) return;   /* out of memory: keep the old table, the set just gets denser */
    for (i = 0; i < cap; i++) if (table[i].p) insert_raw(nt, ncap, &table[i]);
    free(table);
    table = nt;
    cap = ncap;
}

void gc_register(void *p, int kind, size_t bytes) {
    gc_entry_t e;
    if (!p) return;
    if ((count + 1) * 10 > cap * 7) grow();
    if (!cap) return;
    e.p = p; e.bytes = bytes; e.kind = (unsigned char)kind; e.mark = 0; e.pinned = 0;
    insert_raw(table, cap, &e);
    count++;
    bytes_live += bytes;
    bytes_since += bytes;
}

void gc_pin(void *p) {
    gc_entry_t *e = lookup(p);
    if (e) e->pinned = 1;
}

void gc_push_roots(void *values, int n) {
    if (nroots < ROOT_MAX) { roots[nroots].v = (value_t *)values; roots[nroots].n = n; }
    nroots++;
}

void gc_pop_roots(void) {
    if (nroots > 0) nroots--;
}

void gc_set_stack_base(void *p) {
    stack_base = (char *)p;
}

unsigned long gc_registered(void) {
    return (unsigned long)count;
}

/* ---- marking ---- */

static gc_entry_t **mark_stack;
static size_t mark_cap, mark_n;

static void mark_ptr(const void *p) {
    gc_entry_t *e = lookup(p);
    if (!e || e->mark) return;
    e->mark = 1;
    if (e->kind == GC_TEXT) return;   /* a leaf */
    if (mark_n == mark_cap) {
        size_t nc = mark_cap ? mark_cap * 2 : 256;
        gc_entry_t **ns = realloc(mark_stack, nc * sizeof(*ns));
        if (!ns) return;   /* its contents stay unmarked only if memory is gone */
        mark_stack = ns;
        mark_cap = nc;
    }
    mark_stack[mark_n++] = e;
}

static void mark_value(const value_t *v) {
    switch (v->type) {
    case VAL_ARRAY:  mark_ptr(v->array); break;
    case VAL_BLOCK:  mark_ptr(v->block); break;
    case VAL_OBJECT: mark_ptr(v->obj); break;
    case VAL_CHAR:   if (v->ext) mark_ptr(v->ext); break;
    default: break;
    }
}

static void mark_contents(void) {
    while (mark_n > 0) {
        gc_entry_t *e = mark_stack[--mark_n];
        int i;
        switch (e->kind) {
        case GC_ARRAY: {
            array_t *a = (array_t *)e->p;
            for (i = 0; i < a->len; i++) mark_value(&a->el[i]);
            break;
        }
        case GC_BLOCK: {
            block_t *b = (block_t *)e->p;
            for (i = 0; i < b->ncap; i++) mark_value(&b->cap_vals[i]);
            break;
        }
        case GC_OBJECT: {
            object_t *o = (object_t *)e->p;
            for (i = 0; i < o->n; i++) mark_value(&o->vals[i]);
            break;
        }
        default: break;
        }
    }
}

/* Every aligned word of [lo, hi) that is a registered address */
static void scan_range(const char *lo, const char *hi) {
    const char *p = lo + ((uintptr_t)lo & (sizeof(void *) - 1) ? sizeof(void *) - ((uintptr_t)lo & (sizeof(void *) - 1)) : 0);
    for (; p + sizeof(void *) <= hi; p += sizeof(void *)) {
        void *w;
        memcpy(&w, p, sizeof w);
        if (w) mark_ptr(w);
    }
}

static void free_entry(const gc_entry_t *e) {
    switch (e->kind) {
    case GC_ARRAY: {
        array_t *a = (array_t *)e->p;
        free(a->el);
        free(a);
        break;
    }
    case GC_BLOCK: {
        block_t *b = (block_t *)e->p;
        ast_free(b->body);
        free(b);
        break;
    }
    default:
        free(e->p);
        break;
    }
}

#ifdef __slow32__
extern char __data_start[], __bss_end[], __stack_top[];
#endif

static void collect_from(char *stack_here) {
    size_t i, freed = 0, freed_bytes = 0, before = count;
    gc_entry_t *nt;
    int r;
#ifdef __slow32__
    scan_range(__data_start, __bss_end);
    scan_range(stack_here, stack_base ? stack_base : __stack_top);
#else
    (void)stack_here;
    return;   /* a host build does not know its segments: nothing is freed */
#endif
    for (r = 0; r < nroots && r < ROOT_MAX; r++) {
        int k;
        for (k = 0; k < roots[r].n; k++) mark_value(&roots[r].v[k]);
    }
    for (i = 0; i < cap; i++) if (table[i].p && table[i].pinned) mark_ptr(table[i].p);
    mark_contents();
    /* sweep: the survivors into a fresh set */
    nt = calloc(cap, sizeof(gc_entry_t));
    if (!nt) return;
    for (i = 0; i < cap; i++) {
        if (!table[i].p) continue;
        if (table[i].mark) {
            table[i].mark = 0;
            insert_raw(nt, cap, &table[i]);
        } else {
            freed++;
            freed_bytes += table[i].bytes;
            free_entry(&table[i]);
        }
    }
    free(table);
    table = nt;
    count -= freed;
    bytes_live -= freed_bytes;
    bytes_since = 0;
    threshold = bytes_live > (4u << 20) ? bytes_live : (4u << 20);
    if (trace > 0)
        fprintf(stderr, "[gc: %lu of %lu freed, %lu KB live]\n",
                (unsigned long)freed, (unsigned long)before, (unsigned long)(bytes_live >> 10));
}

void gc_collect(void) {
    jmp_buf regs;           /* the callee-saved registers, onto this frame */
    char here;
    if (trace < 0) { const char *e = getenv("XBASE_GC_TRACE"); trace = e && *e && *e != '0'; }
    if (setjmp(regs) == 0) {
        collect_from(&here < (char *)&regs ? &here : (char *)&regs);
    }
}

void gc_safepoint(void) {
    if (bytes_since >= threshold) gc_collect();
}
