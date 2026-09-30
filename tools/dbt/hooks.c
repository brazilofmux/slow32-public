// SLOW-32 DBT: hooks -- native routines the guest opts into
// (docs/dbt-hooks.md, hooks.h).
//
// Each hook is exactly its guest routine for the inputs it accepts, and
// declines the rest; the guest routine is the reference on every engine.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include "hooks.h"

// The tags, checksums of the sources that define the contracts, come from
// the Makefile (cksum of the file).  An empty tag -- the source was not
// there to checksum -- leaves that group unhooked.
#ifndef HK_TAG_BUILTINS
#define HK_TAG_BUILTINS ""
#endif
#ifndef HK_TAG_COBKERN
#define HK_TAG_COBKERN ""
#endif

// libcob's kernels, the same source libcob compiles (docs/dbt-hooks.md)
#if __has_include("../../cobol/libcob/kern.h")
#include "../../cobol/libcob/kern.h"
#define HAVE_COBKERN 1
#else
#define HAVE_COBKERN 0
#endif

#define R(n) (cpu->regs[n])
#define U64(lo, hi) ((uint64_t)R(lo) | ((uint64_t)R(hi) << 32))
#define RET64(v) do { uint64_t v_ = (v); R(1) = (uint32_t)v_; R(2) = (uint32_t)(v_ >> 32); } while (0)

// A guest range as a host pointer, or NULL when a translated access to it
// would fault (out of memory, the MMIO window, a write below the W^X
// limit): the hook then declines and the guest routine faults as it must.
static inline __attribute__((unused)) void *hk_ptr(dbt_cpu_state_t *cpu, uint8_t *mem, uint32_t addr, uint32_t len, int write)
{
    if ((uint64_t)addr + len > cpu->mem_size) return NULL;
    if (cpu->mmio_base && (uint64_t)addr + len > cpu->mmio_base && addr < cpu->mmio_base + 0x10000u) return NULL;
    if (write && cpu->wxorx_enabled) {
        uint32_t lim = cpu->rodata_limit ? cpu->rodata_limit : cpu->code_limit;
        if (addr < lim) return NULL;
    }
    return mem + addr;
}

// ---- runtime/builtins.c: the 64-bit division routines ------------------
// Arguments in r3:r4 and r5:r6, the result in r1:r2, low word first.  A
// zero divisor declines: the guest's own answer stands.  The signed ones
// follow builtins.c's formula in unsigned arithmetic, so INT64_MIN / -1
// is what the guest computes (INT64_MIN), not a host trap.

static int hk_udivdi3(dbt_cpu_state_t *cpu, uint8_t *mem)
{
    (void)mem;
    uint64_t n = U64(3, 4), d = U64(5, 6);
    if (!d) return HK_DECLINE;
    RET64(n / d);
    return HK_DONE;
}

static int hk_umoddi3(dbt_cpu_state_t *cpu, uint8_t *mem)
{
    (void)mem;
    uint64_t n = U64(3, 4), d = U64(5, 6);
    if (!d) return HK_DECLINE;
    RET64(n % d);
    return HK_DONE;
}

static int hk_divdi3(dbt_cpu_state_t *cpu, uint8_t *mem)
{
    (void)mem;
    uint64_t n = U64(3, 4), d = U64(5, 6);
    if (!d) return HK_DECLINE;
    int nneg = (int64_t)n < 0, dneg = (int64_t)d < 0;
    uint64_t un = nneg ? 0 - n : n, ud = dneg ? 0 - d : d;
    uint64_t q = un / ud;
    RET64(nneg != dneg ? 0 - q : q);
    return HK_DONE;
}

static int hk_moddi3(dbt_cpu_state_t *cpu, uint8_t *mem)
{
    (void)mem;
    uint64_t n = U64(3, 4), d = U64(5, 6);
    if (!d) return HK_DECLINE;
    int nneg = (int64_t)n < 0;
    uint64_t un = nneg ? 0 - n : n, ud = (int64_t)d < 0 ? 0 - d : d;
    uint64_t r = un % ud;
    RET64(nneg ? 0 - r : r);
    return HK_DONE;
}

// ---- cobol/libcob/kern.h: numeric fetch and store ---------------------

#if HAVE_COBKERN
// the descriptor at guest address a, copied out (it need not be aligned)
static int hk_desc(dbt_cpu_state_t *cpu, uint8_t *mem, uint32_t a, cob_kdesc *d)
{
    const void *g = hk_ptr(cpu, mem, a, sizeof *d, 0);
    if (!g) return 0;
    memcpy(d, g, sizeof *d);
    return 1;
}

// long long cob_get_num(const void *p, const cob_desc *d)
static int hk_cob_get_num(dbt_cpu_state_t *cpu, uint8_t *mem)
{
    cob_kdesc d;
    if (!hk_desc(cpu, mem, R(4), &d) || !cob_k_get_ok(&d) || !d.size) return HK_DECLINE;
    const unsigned char *p = hk_ptr(cpu, mem, R(3), d.size, 0);
    if (!p) return HK_DECLINE;
    RET64((uint64_t)cob_k_get_num(p, &d));
    return HK_DONE;
}

// int cob_put_num_x(void *p, const cob_desc *d, long long v, int vscale, int opts)
static int hk_cob_put_num_x(dbt_cpu_state_t *cpu, uint8_t *mem)
{
    cob_kdesc d;
    if (!hk_desc(cpu, mem, R(4), &d) || !cob_k_put_ok(&d) || !d.size) return HK_DECLINE;
    int eff = d.digits;
    if (d.pic) {                        // the PICTURE's P symbols hold no digit
        for (uint32_t a = d.pic; ; a++) {
            const char *c = hk_ptr(cpu, mem, a, 1, 0);
            if (!c || a - d.pic > 256) return HK_DECLINE;
            if (!*c) break;
            if (*c == 'P') eff--;
        }
    }
    unsigned char *p = hk_ptr(cpu, mem, R(3), d.size, 1);
    if (!p) return HK_DECLINE;
    R(1) = (uint32_t)cob_k_put_num(p, &d, eff, (long long)U64(5, 6), (int)R(7), (int)R(8));
    return HK_DONE;
}

// The item's PICTURE as a host string, its P symbols, and the bytes the
// editor walks (CR/DB two, V S P none); NULL when it is not all there or
// longer than 38 symbols (the kernels' digit buffers hold 40).
static const char *hk_pic(dbt_cpu_state_t *cpu, uint8_t *mem, uint32_t a, int *np, uint32_t *width)
{
    *np = 0; *width = 0;
    if (!a) return NULL;
    for (uint32_t i = 0; ; i++) {
        const char *c = hk_ptr(cpu, mem, a + i, 1, 0);
        if (!c || i > 38) return NULL;
        if (!*c) break;
        if (*c == 'P') ++*np;
        *width += (*c == 'C' || *c == 'D') ? 2 : (*c == 'V' || *c == 'S' || *c == 'P') ? 0 : 1;
    }
    return (const char *)mem + a;
}

// int cob_put_edited(void *p, const cob_desc *d, long long v, int vscale, int opts, int locale)
static int hk_cob_put_edited(dbt_cpu_state_t *cpu, uint8_t *mem)
{
    cob_kdesc d;
    int np; uint32_t width;
    if (!hk_desc(cpu, mem, R(4), &d) || !cob_k_ed_ok(&d)) return HK_DECLINE;
    const char *pic = hk_pic(cpu, mem, d.pic, &np, &width);
    if (!pic) return HK_DECLINE;
    unsigned char *p = hk_ptr(cpu, mem, R(3), width > d.size ? width : d.size, 1);
    if (!p) return HK_DECLINE;
    R(1) = (uint32_t)cob_k_put_edited(p, &d, pic, d.digits - np, (long long)U64(5, 6), (int)R(7), (int)R(8), (int)R(9));
    return HK_DONE;
}

// long long cob_get_edited(const void *p, const cob_desc *d, int locale)
static int hk_cob_get_edited(dbt_cpu_state_t *cpu, uint8_t *mem)
{
    cob_kdesc d;
    int np; uint32_t width;
    if (!hk_desc(cpu, mem, R(4), &d) || !cob_k_ed_ok(&d)) return HK_DECLINE;
    const char *pic = hk_pic(cpu, mem, d.pic, &np, &width);
    if (!pic) return HK_DECLINE;
    const unsigned char *p = hk_ptr(cpu, mem, R(3), width > d.size ? width : d.size, 0);
    if (!p) return HK_DECLINE;
    RET64((uint64_t)cob_k_get_edited(p, &d, pic, (int)R(5)));
    return HK_DONE;
}
#endif

// ---- the definitions --------------------------------------------------

typedef struct {
    const char *name;       // the routine: __s32hk_<name>_<tag>
    const char *tag;
    int (*fn)(dbt_cpu_state_t *cpu, uint8_t *mem);
} hook_def_t;

static const hook_def_t hook_defs[] = {
    { "__udivdi3", HK_TAG_BUILTINS, hk_udivdi3 },
    { "__umoddi3", HK_TAG_BUILTINS, hk_umoddi3 },
    { "__divdi3",  HK_TAG_BUILTINS, hk_divdi3 },
    { "__moddi3",  HK_TAG_BUILTINS, hk_moddi3 },
#if HAVE_COBKERN
    { "cob_get_num",   HK_TAG_COBKERN, hk_cob_get_num },
    { "cob_put_num_x", HK_TAG_COBKERN, hk_cob_put_num_x },
    { "cob_get_edited", HK_TAG_COBKERN, hk_cob_get_edited },
    { "cob_put_edited", HK_TAG_COBKERN, hk_cob_put_edited },
#endif
};
#define NDEFS ((int)(sizeof hook_defs / sizeof hook_defs[0]))

static uint64_t hook_calls[NDEFS], hook_declines[NDEFS];

int dbt_hook_call(dbt_cpu_state_t *cpu, uint8_t *mem, uint32_t idx)
{
    const dbt_hook_t *h = &cpu->hooks[idx];
    hook_calls[h->def]++;
    int r = hook_defs[h->def].fn(cpu, mem);
    if (r != HK_DONE) hook_declines[h->def]++;
    return r;
}

int dbt_hook_at(const dbt_cpu_state_t *cpu, uint32_t guest_pc)
{
    if (guest_pc < cpu->hook_lo || guest_pc > cpu->hook_hi) return -1;
    for (int i = 0; i < cpu->num_hooks; i++)
        if (cpu->hooks[i].guest_addr == guest_pc) return i;
    return -1;
}

// S32_HOOKS=name,name: only those (bisecting a suspect hook)
static int hook_wanted(const char *name)
{
    const char *only = getenv("S32_HOOKS");
    if (!only) return 1;
    size_t n = strlen(name);
    for (const char *p = only; *p; ) {
        const char *e = strchr(p, ',');
        size_t len = e ? (size_t)(e - p) : strlen(p);
        if (len == n && !strncmp(p, name, n)) return 1;
        if (!e) break;
        p = e + 1;
    }
    return 0;
}

void dbt_hooks_register(dbt_cpu_state_t *cpu,
                        uint32_t (*lookup)(void *user, const char *name),
                        void *user)
{
    cpu->num_hooks = 0;
    cpu->hook_lo = UINT32_MAX;
    cpu->hook_hi = 0;
    for (int d = 0; d < NDEFS && cpu->num_hooks < MAX_HOOKS; d++) {
        const hook_def_t *def = &hook_defs[d];
        if (!def->tag[0] || !hook_wanted(def->name)) continue;
        char sym[160];
        snprintf(sym, sizeof sym, "__s32hk_%s_%s", def->name, def->tag);
        uint32_t addr = lookup(user, sym);
        if (!addr || (addr & 3) || (uint64_t)addr + 4 > cpu->code_limit) continue;
        // the thunk must be `jal r0, impl`; anything else, and the guest runs
        uint32_t raw;
        memcpy(&raw, cpu->mem_base + addr, 4);
        if ((raw & 0x7F) != 0x40 || ((raw >> 7) & 0x1F) != 0) continue;
        uint32_t imm = (((raw >> 31) & 1) << 20) | (((raw >> 12) & 0xFF) << 12) |
                       (((raw >> 20) & 1) << 11) | (((raw >> 21) & 0x3FF) << 1);
        if (imm & 0x100000) imm |= 0xFFE00000;
        dbt_hook_t *h = &cpu->hooks[cpu->num_hooks++];
        h->guest_addr = addr;
        h->decline_pc = addr + imm;
        h->def = (uint16_t)d;
        if (addr < cpu->hook_lo) cpu->hook_lo = addr;
        if (addr > cpu->hook_hi) cpu->hook_hi = addr;
    }
}

void dbt_hooks_print_stats(const dbt_cpu_state_t *cpu)
{
    if (!cpu->num_hooks) return;
    fprintf(stderr, "Hooks: %d\n", cpu->num_hooks);
    for (int i = 0; i < cpu->num_hooks; i++) {
        int d = cpu->hooks[i].def;
        fprintf(stderr, "  %-24s 0x%08X  calls %" PRIu64 "  declined %" PRIu64 "\n",
                hook_defs[d].name, cpu->hooks[i].guest_addr, hook_calls[d], hook_declines[d]);
    }
}
