/* memcheck.h -- -m: watch what a guest does with its memory.
 *
 * Every output-comparing gate sees a bad read only when it changes an
 * answer, and on SLOW-32 most never do: a stale stack slot holds whatever
 * the last call left, a freed block still holds its bytes, and a load from
 * address 0 quietly returns the first instruction of crt0.  This is the
 * instrument for the reads themselves, below every compiler: it runs the
 * program exactly as the interpreter always does -- the same instructions,
 * the same output -- and keeps a shadow beside it.
 *
 * WHAT IS SHADOWED
 *
 *   Addressability, per byte.  Code, rodata, data and bss are addressable
 *   (and a load through a null pointer is one from code: see mc_load).
 *   The stack is addressable from sp up, and for MC_REDZONE bytes below
 *   it: the machine has no interrupts (docs/SPEC.md 7), so nothing but
 *   the program writes there, and the HIR backends' large-frame prologue
 *   saves lr and fp at sp-4 and sp-8 before it moves sp.  Below that, a
 *   byte is not the program's.  When the
 *   executable names malloc and free, the heap is addressable only inside
 *   a live block, to the byte: the allocator's own headers, a freed block
 *   and the padding after an odd-sized one are not.
 *
 *   Definedness, per bit (Seward & Nethercote's V bits).  Stack bytes
 *   become undefined when sp moves below them, a malloc'd block starts
 *   undefined, and everything else starts defined.  Undefined bits flow
 *   through registers and memory with the values -- copying a structure
 *   with padding is no error.  An error is a USE: a conditional branch, a
 *   memory address, a jump target, or a byte handed to the host (a WRITE
 *   request, a DEBUG character).
 *
 *   Origins, per word: where the undefined value was born (the frame of
 *   the function that lowered sp, or the heap block), so a report says
 *   more than where it was finally used.
 *
 * WHAT IS NOT SEEN
 *
 *   An overrun from one global into the next, or from one local into
 *   another in the same or a caller's frame: both are addressable.  A use
 *   after free once the allocator has handed the block out again.  A heap
 *   the program manages by itself (no malloc symbol: Pascal's RTL, the
 *   Forth kernel) is plain defined memory.  The DBT and slow32-fast do
 *   not have the mode.
 *
 * The allocator is not replaced.  Calls to malloc/calloc/realloc/free/
 * aligned_alloc/posix_memalign are noticed by address, and what happens
 * between the entry of the outermost one and its return is the
 * allocator's own business: nothing is checked there (it reads headers
 * and the footers of neighbours), though definedness still flows.
 *
 * REPORTS go to stderr, or are appended to the file S32_MEMCHECK_LOG
 * names (a run's reports in one piece, when it ends), once for each
 * (kind, pc), with a call chain kept from the JAL/JALR stream.  A clean
 * run prints nothing at all.  S32_MEMCHECK=1 in the environment is -m, so
 * a whole gate can be run under the mode without touching it --
 *
 *     S32_MEMCHECK=1 S32_MEMCHECK_LOG=/abs/log EMU=tools/emulator/slow32 \
 *         fortran/tests/run-tests.sh
 *
 * -- and must pass as it did, since nothing the gate compares changes.
 * The exit status is the program's own unless S32_MEMCHECK_EXIT=N asks
 * for N when there were errors.  S32_MEMCHECK_MAX is how many distinct
 * reports a run prints (100); S32_MEMCHECK_NOHEAP leaves the allocator
 * unwatched.
 */
#ifndef SLOW32_MEMCHECK_H
#define SLOW32_MEMCHECK_H

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include "../../common/mmio_ring_layout.h"

#define MC_REDZONE   8u
#define MC_PAGE      4096u
#define MC_NPAGES    (1u << 20)
#define MC_MMIO_SIZE 0x10000u

/* One guest page of shadow.  A page-table entry that is NULL is a page no
 * byte of which is addressable; one that is &mc_all_defined is a page
 * wholly addressable and defined, shared until something writes to it. */
typedef struct {
    uint8_t  v[MC_PAGE];        /* 1 bits are undefined */
    uint8_t  a[MC_PAGE];        /* 1 = addressable */
    uint32_t o[MC_PAGE / 4];    /* origin of the word's undefined bits */
} mc_page_t;

enum { MC_LIVE = 1, MC_FREED = 2 };
typedef struct {
    uint32_t addr, size;
    uint32_t serial;            /* 0 = empty slot */
    uint32_t alloc_pc, free_pc;
    uint8_t  state;
} mc_block_t;

/* What a register is known to hold beyond its bits: t = x ^ y, and then
 * t & m.  The HIR backends select without a branch -- y ^ ((x ^ y) & m),
 * m all ones or none -- and bit by bit that looks undefined whenever the
 * side NOT chosen is.  With the note, the last XOR is seen for what it is:
 * each result bit is x's or y's as m says, and as defined as that one. */
enum { MC_N_NONE, MC_N_XOR, MC_N_MASKED };
typedef struct { uint8_t kind; uint32_t x, vx, ox, y, vy, oy, m, vm; } mc_note_t;

typedef struct { uint32_t size, pc; } mc_birth_t;     /* by serial: for origins */
typedef struct { uint32_t ret, sp, callee; } mc_call_t;
typedef struct { uint32_t addr; const char *name; uint8_t func; } mc_sym_t;

enum { MC_F_NONE, MC_F_MALLOC, MC_F_CALLOC, MC_F_REALLOC, MC_F_FREE,
       MC_F_ALIGNED, MC_F_PMEMALIGN };

#define MC_CALL_MAX 4096
#define MC_SEEN_SZ  4096

typedef struct {
    int       on;
    FILE     *out;
    const char *log_path;       /* set: `out` is a buffer for it */
    char     *log_buf; size_t log_len;
    const char *prog;
    mc_page_t **pt;
    uint32_t  vr[32];           /* undefined bits of each register */
    uint32_t  orr[32];          /* and where they came from */
    mc_note_t note[32];

    uint32_t  stk_lo, stk_hi;   /* sp lives in [stk_lo, stk_hi] */
    uint32_t  code_limit;
    uint32_t  mmio_base;        /* 0 = none */
    uint8_t  *mmio_v;           /* V bits of what the guest stored there */
    uint32_t *mmio_o;

    /* heap */
    int       heap;             /* the allocator is watched */
    uint32_t  heap_lo, heap_hi;
    uint8_t  *hook;             /* by pc/4: MC_F_* of a function entered there */
    int       in_alloc;         /* MC_F_* of the outermost call in progress */
    uint32_t  al_ret, al_sp, al_a0, al_a1, al_a2, al_pc;
    mc_block_t *blk;  uint32_t blk_cap, blk_n;
    mc_birth_t *birth; uint32_t birth_cap, birth_n;

    /* the call chain */
    mc_call_t call[MC_CALL_MAX];
    int       ncall;

    mc_sym_t *sym; uint32_t nsym;

    /* reports */
    uint64_t  errors;
    uint32_t  distinct, max_shown;
    uint64_t  seen[MC_SEEN_SZ];
    int       exit_code;
} mc_state_t;

static mc_state_t g_mc;
static mc_page_t mc_all_defined;

/* ---------------------------------------------------------------- names */

static int mc_sym_cmp(const void *a, const void *b) {
    const mc_sym_t *x = a, *y = b;
    if (x->addr != y->addr) return x->addr < y->addr ? -1 : 1;
    /* of two names for an address, the function's, and _start before __code_start */
    if (x->func != y->func) return (int)y->func - (int)x->func;
    return (x->name[0] == '_' && x->name[1] == '_') - (y->name[0] == '_' && y->name[1] == '_');
}

/* "name+0x1c" for a code address, or "?" */
static const char *mc_name(uint32_t pc, char *buf, size_t n) {
    uint32_t lo = 0, hi = g_mc.nsym;
    if (!g_mc.nsym || pc < g_mc.sym[0].addr || pc >= g_mc.code_limit) {
        snprintf(buf, n, "?");
        return buf;
    }
    while (hi - lo > 1) {
        uint32_t mid = lo + (hi - lo) / 2;
        if (g_mc.sym[mid].addr <= pc) lo = mid; else hi = mid;
    }
    while (lo > 0 && g_mc.sym[lo - 1].addr == g_mc.sym[lo].addr) lo--;
    if (pc == g_mc.sym[lo].addr) snprintf(buf, n, "%s", g_mc.sym[lo].name);
    else snprintf(buf, n, "%s+0x%x", g_mc.sym[lo].name, pc - g_mc.sym[lo].addr);
    return buf;
}

/* ------------------------------------------------------------- the shadow */

static mc_page_t *mc_page_rw(uint32_t pn) {
    mc_page_t *p = g_mc.pt[pn];
    if (p && p != &mc_all_defined) return p;
    mc_page_t *q = calloc(1, sizeof *q);
    if (!q) { fprintf(stderr, "memcheck: out of memory\n"); exit(1); }
    if (p) memset(q->a, 1, MC_PAGE);
    g_mc.pt[pn] = q;
    return q;
}

static void mc_page_drop(uint32_t pn, mc_page_t *to) {
    mc_page_t *p = g_mc.pt[pn];
    if (p && p != &mc_all_defined) free(p);
    g_mc.pt[pn] = to;
}

/* Make [addr, addr+len) addressable or not, and if addressable either all
 * defined (v = 0) or all undefined (v = 0xFF, born at `origin`). */
static void mc_set_range(uint32_t addr, uint32_t len, int a, uint8_t v, uint32_t origin) {
    while (len) {
        uint32_t pn = addr >> 12, off = addr & (MC_PAGE - 1);
        uint32_t n = MC_PAGE - off;
        if (n > len) n = len;
        if (n == MC_PAGE && !a) mc_page_drop(pn, NULL);
        else if (n == MC_PAGE && !v) mc_page_drop(pn, &mc_all_defined);
        else {
            mc_page_t *p = g_mc.pt[pn];
            if (!(p == NULL && !a) && !(p == &mc_all_defined && a && !v)) {
                p = mc_page_rw(pn);
                memset(p->a + off, a ? 1 : 0, n);
                memset(p->v + off, a ? v : 0, n);
                if (a && v) {
                    uint32_t w;
                    for (w = off >> 2; w <= (off + n - 1) >> 2; w++) p->o[w] = origin;
                }
            }
        }
        addr += n;
        len -= n;
    }
}

/* Only the V bits of a range that stays as addressable as it is */
static void mc_set_defined(uint32_t addr, uint32_t len) {
    while (len) {
        uint32_t pn = addr >> 12, off = addr & (MC_PAGE - 1);
        uint32_t n = MC_PAGE - off;
        mc_page_t *p = g_mc.pt[pn];
        if (n > len) n = len;
        if (p && p != &mc_all_defined) memset(p->v + off, 0, n);
        addr += n;
        len -= n;
    }
}

static inline int mc_in_mmio(uint32_t addr) {
    return g_mc.mmio_base && addr - g_mc.mmio_base < MC_MMIO_SIZE;
}

/* 0 if every byte is addressable, else 1 + the index of the first that is not */
static inline int mc_unaddressable(uint32_t addr, int size) {
    int i;
    for (i = 0; i < size; i++) {
        mc_page_t *p = g_mc.pt[(addr + (uint32_t)i) >> 12];
        if (!p || !p->a[(addr + (uint32_t)i) & (MC_PAGE - 1)]) return i + 1;
    }
    return 0;
}

static inline uint32_t mc_get_v(uint32_t addr, int size, uint32_t *origin) {
    uint32_t v = 0;
    int i;
    for (i = 0; i < size; i++) {
        mc_page_t *p = g_mc.pt[(addr + (uint32_t)i) >> 12];
        uint32_t off = (addr + (uint32_t)i) & (MC_PAGE - 1);
        if (p && p->v[off]) {
            v |= (uint32_t)p->v[off] << (8 * i);
            *origin = p->o[off >> 2];
        }
    }
    return v;
}

static inline void mc_put_v(uint32_t addr, int size, uint32_t v, uint32_t origin) {
    int i;
    for (i = 0; i < size; i++) {
        uint32_t pn = (addr + (uint32_t)i) >> 12;
        uint32_t off = (addr + (uint32_t)i) & (MC_PAGE - 1);
        uint8_t b = (uint8_t)(v >> (8 * i));
        mc_page_t *p = g_mc.pt[pn];
        if (!p) continue;
        if (p == &mc_all_defined) {
            if (!b) continue;
            p = mc_page_rw(pn);
        }
        p->v[off] = b;
        if (b) p->o[off >> 2] = origin;
    }
}

/* ----------------------------------------------------------- heap blocks */

static mc_block_t *mc_blk_slot(uint32_t addr) {
    uint32_t i = (addr * 2654435761u) >> 7;
    for (;;) {
        mc_block_t *b = &g_mc.blk[i & (g_mc.blk_cap - 1)];
        if (!b->serial || b->addr == addr) return b;
        i++;
    }
}

static void mc_blk_grow(void) {
    mc_block_t *old = g_mc.blk;
    uint32_t ocap = g_mc.blk_cap, i;
    g_mc.blk_cap = ocap ? ocap * 2 : 1024;
    g_mc.blk = calloc(g_mc.blk_cap, sizeof *g_mc.blk);
    if (!g_mc.blk) { fprintf(stderr, "memcheck: out of memory\n"); exit(1); }
    for (i = 0; i < ocap; i++)
        if (old[i].serial) *mc_blk_slot(old[i].addr) = old[i];
    free(old);
}

static mc_block_t *mc_blk_find(uint32_t addr) {
    mc_block_t *b;
    if (!g_mc.blk_cap) return NULL;
    b = mc_blk_slot(addr);
    return b->serial ? b : NULL;
}

/* The block a stray address most likely belongs to: the one it is in
 * (the latest, if it is freed ones), else the nearest live one. */
static mc_block_t *mc_blk_near(uint32_t addr) {
    mc_block_t *best = NULL, *in = NULL;
    uint32_t i, bestd = 0;
    for (i = 0; i < g_mc.blk_cap; i++) {
        mc_block_t *b = &g_mc.blk[i];
        if (!b->serial) continue;
        if (addr >= b->addr && addr < b->addr + b->size) {
            if (b->state == MC_LIVE) return b;
            if (!in || b->serial > in->serial) in = b;
        } else if (b->state == MC_LIVE && addr + 64 >= b->addr && addr < b->addr + b->size + 64) {
            uint32_t d = addr < b->addr ? b->addr - addr : addr - (b->addr + b->size);
            if (!best || d < bestd) { best = b; bestd = d; }
        }
    }
    return in ? in : best;
}

static uint32_t mc_new_serial(uint32_t size, uint32_t pc) {
    if (g_mc.birth_n == g_mc.birth_cap) {
        g_mc.birth_cap = g_mc.birth_cap ? g_mc.birth_cap * 2 : 1024;
        g_mc.birth = realloc(g_mc.birth, g_mc.birth_cap * sizeof *g_mc.birth);
        if (!g_mc.birth) { fprintf(stderr, "memcheck: out of memory\n"); exit(1); }
        if (!g_mc.birth_n) g_mc.birth_n = 1;        /* serial 0 is "no block" */
    }
    g_mc.birth[g_mc.birth_n].size = size;
    g_mc.birth[g_mc.birth_n].pc = pc;
    return g_mc.birth_n++;
}

/* --------------------------------------------------------------- reports */

enum { MC_E_BRANCH = 1, MC_E_ADDR, MC_E_JUMP, MC_E_OUT, MC_E_READ, MC_E_WRITE,
       MC_E_NULL, MC_E_FREE, MC_E_REQ, MC_E_HOSTW };

static void mc_trim_calls(uint32_t sp) {
    while (g_mc.ncall > 0 && g_mc.call[g_mc.ncall - 1].sp < sp) g_mc.ncall--;
}

static void mc_print_origin(uint32_t origin) {
    char nb[160];
    if (!origin) return;
    if (origin & 0x80000000u) {
        uint32_t s = origin & 0x7FFFFFFFu;
        if (s < g_mc.birth_n)
            fprintf(g_mc.out, "   the value is from a block of %u bytes allocated at 0x%08X %s\n",
                    g_mc.birth[s].size, g_mc.birth[s].pc,
                    mc_name(g_mc.birth[s].pc, nb, sizeof nb));
    } else {
        fprintf(g_mc.out, "   the value is from the stack frame made at 0x%08X %s\n",
                origin, mc_name(origin, nb, sizeof nb));
    }
}

static void mc_print_block(uint32_t addr) {
    char nb[160];
    mc_block_t *b;
    if (addr >= g_mc.stk_lo && addr <= g_mc.stk_hi + 16) {
        fprintf(g_mc.out, "   0x%08X is on the stack\n", addr);
        return;
    }
    if (!g_mc.heap || addr < g_mc.heap_lo || addr >= g_mc.heap_hi) return;
    b = mc_blk_near(addr);
    if (!b) {
        fprintf(g_mc.out, "   0x%08X is in the heap, in no block that was ever allocated\n", addr);
        return;
    }
    if (addr < b->addr)
        fprintf(g_mc.out, "   0x%08X is %u bytes before", addr, b->addr - addr);
    else if (addr >= b->addr + b->size)
        fprintf(g_mc.out, "   0x%08X is %u bytes after", addr, addr - (b->addr + b->size));
    else
        fprintf(g_mc.out, "   0x%08X is %u bytes inside", addr, addr - b->addr);
    fprintf(g_mc.out, " a block of %u bytes allocated at 0x%08X %s\n",
            b->size, b->alloc_pc, mc_name(b->alloc_pc, nb, sizeof nb));
    if (b->state == MC_FREED)
        fprintf(g_mc.out, "   and freed at 0x%08X %s\n",
                b->free_pc, mc_name(b->free_pc, nb, sizeof nb));
}

/* Returns 1 when the caller should print the rest of the report: the
 * first time this kind of error is seen at this pc. */
static int mc_error(cpu_state_t *cpu, int kind, const char *fmt, ...) {
    uint64_t key = ((uint64_t)kind << 32) | cpu->pc;
    uint32_t h = (uint32_t)((key * 0x9E3779B97F4A7C15ull) >> 40);
    char nb[160];
    va_list ap;
    int i, shown;

    g_mc.errors++;
    for (i = 0; i < MC_SEEN_SZ; i++) {
        uint64_t *s = &g_mc.seen[(h + (uint32_t)i) & (MC_SEEN_SZ - 1)];
        if (*s == key + 1) return 0;
        if (!*s) { *s = key + 1; break; }
    }
    if (i == MC_SEEN_SZ) return 0;
    g_mc.distinct++;
    if (g_mc.distinct > g_mc.max_shown) return 0;

    fprintf(g_mc.out, "memcheck: ");
    va_start(ap, fmt);
    vfprintf(g_mc.out, fmt, ap);
    va_end(ap);
    fprintf(g_mc.out, "\n   at 0x%08X %s\n", cpu->pc, mc_name(cpu->pc, nb, sizeof nb));
    mc_trim_calls(cpu->regs[REG_SP]);
    for (i = g_mc.ncall - 1, shown = 0; i >= 0 && shown < 12; i--, shown++) {
        uint32_t at = g_mc.call[i].ret - 4;
        fprintf(g_mc.out, "   by 0x%08X %s\n", at, mc_name(at, nb, sizeof nb));
    }
    return 1;
}

/* A use of a register with undefined bits.  One report, and the register
 * counts as defined from here on: the first use is the news. */
static void mc_use(cpu_state_t *cpu, int reg, uint32_t mask, int kind, const char *what) {
    if (!(g_mc.vr[reg] & mask)) return;
    if (!g_mc.in_alloc && mc_error(cpu, kind, "%s (r%d)", what, reg))
        mc_print_origin(g_mc.orr[reg]);
    g_mc.vr[reg] = 0;
}

/* ------------------------------------------------------------------ setup */

static void mc_init(void) {
    const char *s;
    g_mc.on = 1;
    g_mc.out = stderr;
    g_mc.max_shown = 100;
    s = getenv("S32_MEMCHECK_LOG");
    if (s && *s) {
        FILE *f = open_memstream(&g_mc.log_buf, &g_mc.log_len);
        if (f) { g_mc.out = f; g_mc.log_path = s; }
    }
    s = getenv("S32_MEMCHECK_MAX");  if (s) g_mc.max_shown = (uint32_t)strtoul(s, NULL, 0);
    s = getenv("S32_MEMCHECK_EXIT"); if (s) g_mc.exit_code = atoi(s);
    g_mc.pt = calloc(MC_NPAGES, sizeof *g_mc.pt);
    g_mc.mmio_v = calloc(MC_MMIO_SIZE, 1);
    g_mc.mmio_o = calloc(MC_MMIO_SIZE / 4, sizeof *g_mc.mmio_o);
    if (!g_mc.pt || !g_mc.mmio_v || !g_mc.mmio_o) {
        fprintf(stderr, "memcheck: out of memory\n");
        exit(1);
    }
    memset(mc_all_defined.a, 1, MC_PAGE);
}

static uint32_t mc_lookup(s32x_symtab_result_t *st, const char *name, uint8_t *hook, int kind) {
    uint32_t a = s32x_symtab_lookup(st, name);
    if (a && hook && a < g_mc.code_limit && !(a & 3)) hook[a >> 2] = (uint8_t)kind;
    return a;
}

/* After the program is loaded: lay the shadow over its regions and find
 * the allocator. */
static void mc_attach(cpu_state_t *cpu, const char *filename, uint32_t stack_base) {
    s32x_load_result_t hdr = load_s32x_header(filename);
    s32x_symtab_result_t st = load_s32x_symtab(filename);
    memory_region_t *r;
    uint32_t i;

    g_mc.prog = realpath(filename, NULL);       /* a log outlives the directory it was run from */
    if (!g_mc.prog) g_mc.prog = filename;
    g_mc.code_limit = cpu->code_limit;
    g_mc.stk_lo = hdr.stack_end;
    g_mc.stk_hi = stack_base;
    g_mc.mmio_base = cpu->mmio.enabled ? cpu->mmio.base : 0;

    for (r = cpu->mm.regions; r; r = r->next) {
        if (g_mc.mmio_base && r->vaddr_start == g_mc.mmio_base) continue;
        mc_set_range(r->vaddr_start, r->vaddr_end - r->vaddr_start, 1, 0, 0);
    }
    if (stack_base - MC_REDZONE > hdr.stack_end)
        mc_set_range(hdr.stack_end, stack_base - MC_REDZONE - hdr.stack_end, 0, 0, 0);

    if (st.success && st.num_symbols) {
        uint32_t hs, he, m, f;
        g_mc.sym = calloc(st.num_symbols, sizeof *g_mc.sym);
        for (i = 0; i < st.num_symbols; i++) {
            s32x_symbol_t *y = &st.symbols[i];
            int ty = y->type & S32O_SYM_TYPE_MASK;
            if (!y->name || !y->name[0] || y->value >= cpu->code_limit) continue;
            if (ty != S32O_SYM_FUNC && ty != S32O_SYM_NOTYPE) continue;
            g_mc.sym[g_mc.nsym].addr = y->value;
            g_mc.sym[g_mc.nsym].name = y->name;     /* the table is kept */
            g_mc.sym[g_mc.nsym].func = ty == S32O_SYM_FUNC;
            g_mc.nsym++;
        }
        qsort(g_mc.sym, g_mc.nsym, sizeof *g_mc.sym, mc_sym_cmp);

        hs = s32x_symtab_lookup(&st, "__heap_start");
        he = s32x_symtab_lookup(&st, "__heap_end");
        m = s32x_symtab_lookup(&st, "malloc");
        f = s32x_symtab_lookup(&st, "free");
        if (hs && he > hs && m && f && !getenv("S32_MEMCHECK_NOHEAP")) {
            g_mc.hook = calloc(cpu->code_limit / 4 + 1, 1);
            g_mc.heap = 1;
            g_mc.heap_lo = hs;
            g_mc.heap_hi = he;
            mc_lookup(&st, "malloc", g_mc.hook, MC_F_MALLOC);
            mc_lookup(&st, "calloc", g_mc.hook, MC_F_CALLOC);
            mc_lookup(&st, "realloc", g_mc.hook, MC_F_REALLOC);
            mc_lookup(&st, "free", g_mc.hook, MC_F_FREE);
            mc_lookup(&st, "aligned_alloc", g_mc.hook, MC_F_ALIGNED);
            mc_lookup(&st, "posix_memalign", g_mc.hook, MC_F_PMEMALIGN);
            mc_set_range(hs, he - hs, 0, 0, 0);
            mc_blk_grow();
        }
    }
}

/* ------------------------------------------------------------ the allocator */

static void mc_block_born(uint32_t addr, uint32_t size, int zeroed) {
    mc_block_t *b;
    uint32_t serial;
    if (!addr) return;
    if (g_mc.blk_n * 2 >= g_mc.blk_cap) mc_blk_grow();
    serial = mc_new_serial(size, g_mc.al_ret - 4);
    b = mc_blk_slot(addr);
    if (!b->serial) g_mc.blk_n++;
    b->addr = addr;
    b->size = size;
    b->serial = serial;
    b->alloc_pc = g_mc.al_ret - 4;
    b->free_pc = 0;
    b->state = MC_LIVE;
    mc_set_range(addr, size, 1, zeroed ? 0 : 0xFF, 0x80000000u | serial);
}

static void mc_block_dies(mc_block_t *b) {
    b->state = MC_FREED;
    b->free_pc = g_mc.al_ret - 4;
    mc_set_range(b->addr, b->size, 0, 0, 0);
}

static void mc_alloc_enter(cpu_state_t *cpu, int kind) {
    g_mc.in_alloc = kind;
    g_mc.al_pc = cpu->pc;
    g_mc.al_ret = cpu->regs[REG_LR];
    g_mc.al_sp = cpu->regs[REG_SP];
    g_mc.al_a0 = cpu->regs[3];
    g_mc.al_a1 = cpu->regs[4];
    g_mc.al_a2 = cpu->regs[5];

    if (kind == MC_F_FREE || (kind == MC_F_REALLOC && g_mc.al_a0)) {
        mc_block_t *b = mc_blk_find(g_mc.al_a0);
        if (!g_mc.al_a0) return;
        if (!b || b->state != MC_LIVE) {
            if (mc_error(cpu, MC_E_FREE, "%s of 0x%08X, which is not a live block",
                         kind == MC_F_FREE ? "free" : "realloc", g_mc.al_a0))
                mc_print_block(g_mc.al_a0);
            g_mc.al_a0 = 0;         /* whatever comes back is a new block */
            return;
        }
        if (kind == MC_F_FREE) mc_block_dies(b);
    }
}

static void mc_alloc_leave(cpu_state_t *cpu) {
    int kind = g_mc.in_alloc;
    uint32_t res = cpu->regs[1];
    g_mc.in_alloc = 0;
    switch (kind) {
    case MC_F_MALLOC:
        mc_block_born(res, g_mc.al_a0, 0);
        break;
    case MC_F_CALLOC:
        mc_block_born(res, g_mc.al_a0 * g_mc.al_a1, 1);
        break;
    case MC_F_ALIGNED:
        mc_block_born(res, g_mc.al_a1, 0);
        break;
    case MC_F_PMEMALIGN:
        if (res == 0) {
            uint32_t p = 0;
            if (mm_read(&cpu->mm, g_mc.al_a0, &p, 4) == 0) mc_block_born(p, g_mc.al_a2, 0);
        }
        break;
    case MC_F_REALLOC: {
        uint32_t oldp = g_mc.al_a0, n = g_mc.al_a1;
        mc_block_t *b = oldp ? mc_blk_find(oldp) : NULL;
        uint32_t osize = b ? b->size : 0;
        if (!res) {
            /* realloc(p, 0) frees; a failed realloc leaves p alone */
            if (b && n == 0) mc_block_dies(b);
            break;
        }
        if (!b) { mc_block_born(res, n, 0); break; }
        if (res == oldp) {
            uint32_t serial = mc_new_serial(n, g_mc.al_ret - 4);
            if (n > osize) mc_set_range(oldp + osize, n - osize, 1, 0xFF, 0x80000000u | serial);
            else if (n < osize) mc_set_range(oldp + n, osize - n, 0, 0, 0);
            b->size = n;
            b->serial = serial;
            b->alloc_pc = g_mc.al_ret - 4;
        } else {
            /* The bytes moved, and what was undefined in them still is;
             * only the tail is newly so. */
            uint32_t keep = n < osize ? n : osize, i;
            uint8_t *v = malloc(keep ? keep : 1);
            uint32_t og = 0;
            for (i = 0; i < keep; i++) v[i] = (uint8_t)mc_get_v(oldp + i, 1, &og);
            mc_block_dies(b);
            mc_block_born(res, n, 0);
            b = mc_blk_find(res);
            for (i = 0; i < keep; i++) mc_put_v(res + i, 1, v[i], og ? og : (0x80000000u | b->serial));
            free(v);
        }
        break;
    }
    default:
        break;
    }
}

/* ------------------------------------------------------- data to the host */

/* At a YIELD, before the host serves the ring: the requests the guest
 * queued, and the bytes of the data buffer a WRITE is sending. */
static void mc_check_requests(cpu_state_t *cpu) {
    mmio_ring_state_t *m = cpu->mmio.state;
    uint32_t head, tail, n = 0;
    if (!m || !cpu->mmio.initialized || g_mc.in_alloc) return;
    head = m->req_head % S32_MMIO_RING_ENTRIES;
    tail = m->req_tail % S32_MMIO_RING_ENTRIES;
    for (; tail != head && n < S32_MMIO_RING_ENTRIES; tail = (tail + 1) % S32_MMIO_RING_ENTRIES, n++) {
        uint32_t d = S32_MMIO_REQ_RING_OFFSET + tail * 16u, w[4], i;
        memcpy(w, &m->req_ring[tail], 16);
        for (i = 0; i < 16; i++) {
            if (g_mc.mmio_v[d + i]) {
                static const char *const fld[4] = { "opcode", "length", "offset", "status" };
                if (mc_error(cpu, MC_E_REQ, "a host request (opcode 0x%02X) with an uninitialised %s",
                             w[0], fld[i / 4]))
                    mc_print_origin(g_mc.mmio_o[(d + i) / 4]);
                memset(g_mc.mmio_v + d, 0, 16);
                break;
            }
        }
        if (w[0] == S32_MMIO_OP_WRITE && w[2] < S32_MMIO_DATA_CAPACITY &&
            w[1] <= S32_MMIO_DATA_CAPACITY - w[2]) {
            uint32_t b = S32_MMIO_DATA_BUFFER_OFFSET + w[2], bad = 0, first = 0;
            for (i = 0; i < w[1]; i++) {
                if (g_mc.mmio_v[b + i]) {
                    if (!bad) first = i;
                    bad++;
                }
            }
            if (bad) {
                if (mc_error(cpu, MC_E_OUT,
                             "%u uninitialised byte%s written to file descriptor %u (first at byte %u of %u)",
                             bad, bad == 1 ? "" : "s", w[3], first, w[1]))
                    mc_print_origin(g_mc.mmio_o[(b + first) / 4]);
                memset(g_mc.mmio_v + b, 0, w[1]);
            }
        }
    }
}

/* The host stored `size` bytes at `addr` on the guest's behalf (a direct
 * read): they are defined now. */
static void mc_host_wrote(cpu_state_t *cpu, uint32_t addr, uint32_t size) {
    uint32_t i;
    int bad = 0;
    if (mc_in_mmio(addr)) return;
    for (i = 0; i < size && !bad; i++)
        if (mc_unaddressable(addr + i, 1)) bad = (int)i + 1;
    if (bad) {
        if (mc_error(cpu, MC_E_HOSTW, "the host was given %u bytes at 0x%08X to fill, and byte %d is not addressable",
                     size, addr, bad - 1))
            mc_print_block(addr + (uint32_t)bad - 1);
    }
    mc_set_defined(addr, size);
}

/* -------------------------------------------------------------- one step */

static inline uint32_t mc_smear(uint32_t v) { return v | (0u - v); }
static inline uint32_t mc_all(uint32_t v) { return v ? 0xFFFFFFFFu : 0; }

static void mc_bad_access(cpu_state_t *cpu, uint32_t addr, int size, int store) {
    if (g_mc.in_alloc) return;
    if (mc_error(cpu, store ? MC_E_WRITE : MC_E_READ, "%s of %d byte%s at 0x%08X, which is not addressable",
                 store ? "store" : "load", size, size == 1 ? "" : "s", addr)) {
        uint32_t sp = cpu->regs[REG_SP];
        if (addr < sp && addr + MC_REDZONE >= g_mc.stk_lo && sp <= g_mc.stk_hi)
            fprintf(g_mc.out, "   0x%08X is %u bytes below the stack pointer\n", addr, sp - addr);
        else
            mc_print_block(addr + (uint32_t)(mc_unaddressable(addr, size) - 1));
    }
}

/* A load through a null pointer does not fault here: code is readable and
 * it starts at 0, so the answer is an instruction of crt0.  It cannot be
 * told by the address alone -- the Forth kernel's threaded code is data
 * in its code segment, read by design -- so it is told by the base: a
 * register (not r0, which is how an absolute address is written) that
 * holds 0, or the first words of memory through any register. */
static uint32_t mc_load(cpu_state_t *cpu, uint32_t addr, int size, uint32_t *origin, int base) {
    int una;
    if (mc_in_mmio(addr)) return 0;
    if (base != 0 && addr < MC_PAGE && (cpu->regs[base] == 0 || addr < 16) && !g_mc.in_alloc)
        mc_error(cpu, MC_E_NULL, "load of %d byte%s at 0x%08X through r%d: a null pointer (the bytes there are code)",
                 size, size == 1 ? "" : "s", addr, base);
    una = mc_unaddressable(addr, size);
    /* An address no region holds faults in the engine; that is its report */
    if (una && mm_find_region(&cpu->mm, addr)) mc_bad_access(cpu, addr, size, 0);
    return una ? 0 : mc_get_v(addr, size, origin);
}

static void mc_store(cpu_state_t *cpu, uint32_t addr, int size, uint32_t v, uint32_t origin) {
    if (mc_in_mmio(addr)) {
        uint32_t off = addr - g_mc.mmio_base;
        int i;
        for (i = 0; i < size && off + (uint32_t)i < MC_MMIO_SIZE; i++) {
            uint8_t b = (uint8_t)(v >> (8 * i));
            g_mc.mmio_v[off + (uint32_t)i] = b;
            if (b) g_mc.mmio_o[(off + (uint32_t)i) / 4] = origin;
        }
        return;
    }
    if (mc_unaddressable(addr, size)) {
        if (mm_find_region(&cpu->mm, addr)) mc_bad_access(cpu, addr, size, 1);
        if (!g_mc.in_alloc) return;
    }
    mc_put_v(addr, size, v, origin);
}

/* Before the instruction executes: the registers still hold its operands. */
static void mc_pre(cpu_state_t *cpu, const instruction_t *in) {
    uint32_t *vr = g_mc.vr, *og = g_mc.orr;
    uint32_t a = cpu->regs[in->rs1], b = cpu->regs[in->rs2];
    uint32_t va = vr[in->rs1], vb = vr[in->rs2];
    uint32_t imm = (uint32_t)in->imm;
    uint32_t v = 0, o;
    int rd = in->rd, op = in->opcode;
    mc_note_t nn;
    nn.kind = MC_N_NONE;

    /* the allocator: where the outermost call began, and where it ends */
    if (g_mc.heap) {
        if (g_mc.in_alloc) {
            if (cpu->pc == g_mc.al_ret && cpu->regs[REG_SP] == g_mc.al_sp) mc_alloc_leave(cpu);
        }
        if (!g_mc.in_alloc && g_mc.hook[cpu->pc >> 2]) mc_alloc_enter(cpu, g_mc.hook[cpu->pc >> 2]);
    }

    o = va ? og[in->rs1] : og[in->rs2];

    switch (op) {
    case OP_ADD: case OP_MUL:
        v = mc_smear(va | vb);
        break;
    case OP_SUB:
        v = in->rs1 == in->rs2 ? 0 : mc_smear(va | vb);
        break;
    case OP_XOR: {
        const mc_note_t *n = &g_mc.note[in->rs1];
        uint32_t y = b, vy = vb;
        if (in->rs1 == in->rs2) { v = 0; break; }
        v = va | vb;
        if (n->kind != MC_N_MASKED) { n = &g_mc.note[in->rs2]; y = a; vy = va; }
        if (n->kind == MC_N_MASKED && v) {
            /* y ^ ((x ^ y) & m), with y either of the two noted */
            uint32_t vsel = 0, vrest = 0, osel = 0, orest = 0, diff = n->x ^ n->y;
            int hit = 1;
            if (y == n->y && vy == n->vy) { vsel = n->vx; osel = n->ox; vrest = n->vy; orest = n->oy; }
            else if (y == n->x && vy == n->vx) { vsel = n->vy; osel = n->oy; vrest = n->vx; orest = n->ox; }
            else hit = 0;
            if (hit) {
                v = (~n->vm & n->m & vsel) | (~n->vm & ~n->m & vrest) | (n->vm & (vsel | vrest | diff));
                o = (v & vsel) ? osel : orest;
                break;
            }
        }
        nn.kind = MC_N_XOR;
        nn.x = a; nn.vx = va; nn.ox = og[in->rs1];
        nn.y = b; nn.vy = vb; nn.oy = og[in->rs2];
        break;
    }
    case OP_OR:         /* a defined 1 on either side settles the bit */
        v = (va | vb) & ~((a & ~va) | (b & ~vb));
        break;
    case OP_AND:        /* and a defined 0 */
        v = (va | vb) & ~((~a & ~va) | (~b & ~vb));
        if (g_mc.note[in->rs1].kind == MC_N_XOR) { nn = g_mc.note[in->rs1]; nn.m = b; nn.vm = vb; nn.kind = MC_N_MASKED; }
        else if (g_mc.note[in->rs2].kind == MC_N_XOR) { nn = g_mc.note[in->rs2]; nn.m = a; nn.vm = va; nn.kind = MC_N_MASKED; }
        break;
    case OP_SLL: v = (vb & 31) ? 0xFFFFFFFFu : va << (b & 31); break;
    case OP_SRL: v = (vb & 31) ? 0xFFFFFFFFu : va >> (b & 31); break;
    case OP_SRA: v = (vb & 31) ? 0xFFFFFFFFu : (uint32_t)((int32_t)va >> (b & 31)); break;
    case OP_SLT: case OP_SGE:       /* against zero only the sign matters */
        if (in->rs2 == 0) v = va >> 31;
        else v = (va | vb) ? 1 : 0;
        break;
    case OP_SGT: case OP_SLE:
        if (in->rs1 == 0) v = vb >> 31;
        else v = (va | vb) ? 1 : 0;
        break;
    case OP_SLTU: case OP_SGEU: case OP_SGTU: case OP_SLEU:
        /* against zero these are (in)equality with it */
        if (in->rs1 == 0 || in->rs2 == 0) v = ((va | vb) && !((a | b) & ~(va | vb))) ? 1 : 0;
        else v = (va | vb) ? 1 : 0;
        break;
    case OP_SEQ: case OP_SNE:       /* a defined bit that differs settles it */
        v = ((va | vb) && !((a ^ b) & ~(va | vb))) ? 1 : 0;
        break;
    case OP_MULH: case OP_MULHU: case OP_DIV: case OP_REM:
        v = mc_all(va | vb);
        break;

    case OP_ADDI: v = mc_smear(va); o = og[in->rs1]; break;
    case OP_ORI:  v = va & ~imm; o = og[in->rs1]; break;
    case OP_ANDI: v = va & imm; o = og[in->rs1]; break;
    case OP_XORI:
        v = va; o = og[in->rs1];
        nn.kind = MC_N_XOR;
        nn.x = a; nn.vx = va; nn.ox = o;
        nn.y = imm; nn.vy = 0; nn.oy = 0;
        break;
    case OP_SLLI: v = va << (imm & 31); o = og[in->rs1]; break;
    case OP_SRLI: v = va >> (imm & 31); o = og[in->rs1]; break;
    case OP_SRAI: v = (uint32_t)((int32_t)va >> (imm & 31)); o = og[in->rs1]; break;
    case OP_SLTI:
        v = imm == 0 ? va >> 31 : (va ? 1 : 0);
        o = og[in->rs1];
        break;
    case OP_SLTIU:
        if (imm == 1) v = (va && !(a & ~va)) ? 1 : 0;
        else v = va ? 1 : 0;
        o = og[in->rs1];
        break;
    case OP_LUI:
        v = 0;
        break;

    case OP_LDB: case OP_LDH: case OP_LDW: case OP_LDBU: case OP_LDHU: {
        int size = (op == OP_LDW) ? 4 : (op == OP_LDH || op == OP_LDHU) ? 2 : 1;
        mc_use(cpu, in->rs1, 0xFFFFFFFFu, MC_E_ADDR, "load through an uninitialised address");
        o = 0;
        v = mc_load(cpu, a + imm, size, &o, in->rs1);
        if (op == OP_LDB) v = (uint32_t)(int32_t)(int8_t)v;
        else if (op == OP_LDH) v = (uint32_t)(int32_t)(int16_t)v;
        break;
    }
    case OP_STB: case OP_STH: case OP_STW: {
        int size = (op == OP_STW) ? 4 : (op == OP_STH) ? 2 : 1;
        mc_use(cpu, in->rs1, 0xFFFFFFFFu, MC_E_ADDR, "store through an uninitialised address");
        mc_store(cpu, a + imm, size, vb, og[in->rs2]);
        return;
    }

    case OP_BEQ: case OP_BNE:
        if ((va | vb) && !((a ^ b) & ~(va | vb))) {
            int r = va ? in->rs1 : in->rs2;
            mc_use(cpu, r, 0xFFFFFFFFu, MC_E_BRANCH, "conditional branch on an uninitialised value");
            vr[in->rs1] = vr[in->rs2] = 0;
        }
        return;
    case OP_BLT: case OP_BGE:
        if (in->rs2 == 0) { va &= 0x80000000u; vb = 0; }
        else if (in->rs1 == 0) { vb &= 0x80000000u; va = 0; }
        /* fall through */
    case OP_BLTU: case OP_BGEU:
        if (va | vb) {
            int r = va ? in->rs1 : in->rs2;
            mc_use(cpu, r, 0xFFFFFFFFu, MC_E_BRANCH, "conditional branch on an uninitialised value");
            vr[in->rs1] = vr[in->rs2] = 0;
        }
        return;

    case OP_JAL:
    case OP_JALR: {
        uint32_t target, sp = cpu->regs[REG_SP];
        if (op == OP_JALR) {
            mc_use(cpu, in->rs1, 0xFFFFFFFFu, MC_E_JUMP, "jump to an uninitialised address");
            target = (a + imm) & ~1u;
        } else {
            target = cpu->pc + imm;
        }
        mc_trim_calls(sp);
        if (rd == REG_LR) {
            if (g_mc.ncall < MC_CALL_MAX) {
                g_mc.call[g_mc.ncall].ret = cpu->pc + 4;
                g_mc.call[g_mc.ncall].sp = sp;
                g_mc.call[g_mc.ncall].callee = target;
                g_mc.ncall++;
            }
        } else if (rd == 0 && g_mc.ncall > 0 && g_mc.call[g_mc.ncall - 1].ret == target) {
            g_mc.ncall--;
        }
        v = 0;
        break;
    }

    case OP_YIELD: case OP_HALT:      /* the two places the host serves the ring */
        mc_check_requests(cpu);
        return;
    case OP_DEBUG:
        mc_use(cpu, in->rs1, 0xFFu, MC_E_OUT, "DEBUG writes an uninitialised character");
        return;
    case OP_ASSERT_EQ:
        mc_use(cpu, in->rs1, 0xFFFFFFFFu, MC_E_BRANCH, "ASSERT_EQ on an uninitialised value");
        mc_use(cpu, in->rs2, 0xFFFFFFFFu, MC_E_BRANCH, "ASSERT_EQ on an uninitialised value");
        return;
    case OP_NOP:
        return;

    /* Floating point: an undefined bit anywhere in makes the whole result so */
    case OP_FADD_S: case OP_FSUB_S: case OP_FMUL_S: case OP_FDIV_S:
        v = mc_all(va | vb);
        break;
    case OP_FSQRT_S: case OP_FCVT_W_S: case OP_FCVT_WU_S: case OP_FCVT_S_W: case OP_FCVT_S_WU:
        v = mc_all(va);
        o = og[in->rs1];
        break;
    case OP_FEQ_S: case OP_FLT_S: case OP_FLE_S:
        v = (va | vb) ? 1 : 0;
        break;
    case OP_FNEG_S: case OP_FABS_S:
        v = va;
        o = og[in->rs1];
        break;

    default:
        if (op >= OP_FADD_D && op <= OP_FCVT_D_LU) {
            /* which operands and results are pairs */
            int s1pair = !(op == OP_FCVT_D_W || op == OP_FCVT_D_WU || op == OP_FCVT_D_S ||
                           op == OP_FCVT_L_S || op == OP_FCVT_LU_S);
            int s2 = (op >= OP_FADD_D && op <= OP_FDIV_D) || (op >= OP_FEQ_D && op <= OP_FLE_D);
            int dpair = !((op >= OP_FEQ_D && op <= OP_FCVT_WU_D) || op == OP_FCVT_S_D ||
                          op == OP_FCVT_S_L || op == OP_FCVT_S_LU);
            uint32_t u = va, vhi;
            /* an odd or out-of-range pair faults in the engine */
            if ((s1pair && in->rs1 >= 31) || (s2 && in->rs2 >= 31) || (dpair && rd >= 31)) return;
            if (s1pair && vr[in->rs1 + 1]) { u |= vr[in->rs1 + 1]; if (!va) o = og[in->rs1 + 1]; }
            if (s2) {
                u |= vb | vr[in->rs2 + 1];
                if (!va && !(s1pair && vr[in->rs1 + 1])) o = vb ? og[in->rs2] : og[in->rs2 + 1];
            }
            if (op == OP_FNEG_D || op == OP_FABS_D) { v = va; vhi = vr[in->rs1 + 1]; }
            else if (op >= OP_FEQ_D && op <= OP_FLE_D) { v = u ? 1 : 0; vhi = 0; }
            else { v = mc_all(u); vhi = v; }
            if (rd) {
                vr[rd] = v; og[rd] = o;
                g_mc.note[rd].kind = MC_N_NONE;
                if (dpair) { vr[rd + 1] = vhi; og[rd + 1] = o; g_mc.note[rd + 1].kind = MC_N_NONE; }
            }
            vr[0] = 0;
            return;
        }
        return;
    }

    if (rd) {
        vr[rd] = v;
        if (v) og[rd] = o;
        if (nn.kind) g_mc.note[rd] = nn; else g_mc.note[rd].kind = MC_N_NONE;
    }
}

/* After it: the stack pointer may have moved. */
static inline void mc_post(cpu_state_t *cpu, uint32_t old_sp) {
    uint32_t sp = cpu->regs[REG_SP];
    if (sp == old_sp) return;
    if (old_sp < g_mc.stk_lo || old_sp > g_mc.stk_hi || sp < g_mc.stk_lo || sp > g_mc.stk_hi) return;
    /* the red zone moves with sp: what was written in it stays as it is */
    if (sp < old_sp) mc_set_range(sp - MC_REDZONE, old_sp - sp, 1, 0xFF, cpu->pc);
    else mc_set_range(old_sp - MC_REDZONE, sp - old_sp, 0, 0, 0);
}

/* At exit: one line if there was anything, none if not. */
static void mc_finish(void) {
    if (g_mc.errors) {
        fprintf(g_mc.out, "memcheck: %llu error%s at %u place%s in %s\n",
                (unsigned long long)g_mc.errors, g_mc.errors == 1 ? "" : "s",
                g_mc.distinct, g_mc.distinct == 1 ? "" : "s", g_mc.prog ? g_mc.prog : "?");
    }
    if (g_mc.log_path) {
        FILE *f;
        fclose(g_mc.out);
        if (g_mc.log_len && (f = fopen(g_mc.log_path, "a")) != NULL) {
            fwrite(g_mc.log_buf, 1, g_mc.log_len, f);
            fclose(f);
        }
        free(g_mc.log_buf);
    }
}

#endif /* SLOW32_MEMCHECK_H */
