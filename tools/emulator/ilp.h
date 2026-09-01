/* ilp.h -- dynamic ILP-limit analysis for SLOW-32 traces.
 *
 * Answers one question: if you threw unlimited hardware at a SLOW-32
 * program, how many instructions per cycle could it retire?  This is the
 * classic limit study (Wall 1991, Lam & Wilson 1992) applied to this ISA,
 * and it is the cheap experiment that has to come before any spatial /
 * dataflow hardware effort: if the answer is 4, a fabric sized for 45
 * sits idle.
 *
 * Enabled with -I.  S32_ILP_SKIP / S32_ILP_COUNT bound the analysis
 * window (defaults: skip 0, count = everything).  S32_ILP_LAT=real uses
 * a latency table instead of unit latency.
 *
 * MODELS (all assume PERFECT branch prediction -- control dependences
 * are ignored entirely, which is what makes it a limit study):
 *
 *   1. dataflow    unbounded width and window, memory perfectly
 *                  disambiguated (register dependences only).  The
 *                  theoretical ceiling.
 *   2. +true mem   adds real memory dependences: a load waits for the
 *                  last store to ITS address, a store waits for the last
 *                  access to it (RAW/WAW/WAR through memory, oracle
 *                  addresses, no false aliasing).
 *   3. +1 port     model 2 with memory operations serialized one per
 *                  cycle -- SLOW-32's defining "single-ported memory"
 *                  constraint.  Bounded by 1/memory-op-fraction.
 *   4. in-order W  in-order issue, W instructions per cycle, true memory
 *                  dependences.  The honest model for a statically
 *                  scheduled spatial machine, swept over W.
 *
 * Model 1 is out-of-order with an infinite window; 4 is in-order.  Real
 * spatial designs land between them.
 */
#ifndef SLOW32_ILP_H
#define SLOW32_ILP_H

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* The in-order width model runs ONE width per invocation, selected by
 * S32_ILP_W.  Each width needs its own memory shadow (a narrow, slow
 * width writes later timestamps, which a wider one would then inherit
 * as bogus dependences -- this was a real bug, caught because every
 * width reported exactly 1.00 IPC), and eight shadows do not fit in a
 * sensible amount of RAM.  Separate processes also parallelise. */

/* Memory shadow: word-address -> last store / last access cycle.
 * Direct-mapped with a tag; a miss evicts and is counted.  Eviction
 * forgets a dependence, so a high eviction count would bias IPC UP --
 * hence it is reported. */
#define ILP_MEM_BITS 23
#define ILP_MEM_SZ   (1u << ILP_MEM_BITS)
#define ILP_MEM_MASK (ILP_MEM_SZ - 1)

typedef struct {
    uint32_t tag;
    uint32_t st;      /* cycle the last store to this word completed */
    uint32_t ld;      /* cycle the last load of this word completed */
    uint8_t  valid;
} ilp_mem_ent_t;

typedef struct {
    int      enabled;
    uint64_t skip, count, analyzed;
    int      real_lat;

    /* model 1: pure dataflow (register deps only) */
    uint32_t r_df[32];
    uint32_t cyc_df;

    /* model 2: + true memory dependences */
    uint32_t r_mem[32];
    uint32_t cyc_mem;

    /* model 3: + single memory port */
    uint32_t r_1p[32];
    uint32_t cyc_1p;
    uint32_t memport_free;

    /* model 4: in-order, W-wide (one width per run; 0 = disabled) */
    int      width;
    uint32_t r_w[32];
    uint32_t cyc_w;
    uint32_t slots_w;

    ilp_mem_ent_t *mem;     /* for model 2 */
    ilp_mem_ent_t *mem1p;   /* for model 3 (separate timeline) */
    ilp_mem_ent_t *memw;    /* for model 4 (its own timeline) */
    uint64_t evictions;

    /* instruction mix */
    uint64_t n_load, n_store, n_branch, n_jump, n_alu, n_mul, n_div, n_fp, n_other;
} ilp_state_t;

static ilp_state_t g_ilp;

static void ilp_init(void) {
    const char *s;
    memset(&g_ilp, 0, sizeof(g_ilp));
    g_ilp.enabled = 1;
    g_ilp.count = (uint64_t)-1;
    s = getenv("S32_ILP_SKIP");  if (s) g_ilp.skip  = strtoull(s, NULL, 0);
    s = getenv("S32_ILP_COUNT"); if (s) g_ilp.count = strtoull(s, NULL, 0);
    s = getenv("S32_ILP_LAT");   if (s && strcmp(s, "real") == 0) g_ilp.real_lat = 1;
    s = getenv("S32_ILP_W");     if (s) g_ilp.width = atoi(s);
    g_ilp.mem   = calloc(ILP_MEM_SZ, sizeof(ilp_mem_ent_t));
    g_ilp.mem1p = calloc(ILP_MEM_SZ, sizeof(ilp_mem_ent_t));
    g_ilp.memw  = calloc(ILP_MEM_SZ, sizeof(ilp_mem_ent_t));
    if (!g_ilp.mem || !g_ilp.mem1p || !g_ilp.memw) {
        fprintf(stderr, "ilp: out of memory\n");
        exit(1);
    }
}

/* Latency of an instruction's result, in cycles. */
static int ilp_latency(int op) {
    if (!g_ilp.real_lat) return 1;
    switch (op) {
        case OP_LDB: case OP_LDH: case OP_LDW: case OP_LDBU: case OP_LDHU: return 2;
        case OP_MUL: case OP_MULH: case OP_MULHU: return 3;
        case OP_DIV: case OP_REM: return 16;
        case OP_FADD_S: case OP_FSUB_S: case OP_FADD_D: case OP_FSUB_D: return 3;
        case OP_FMUL_S: case OP_FMUL_D: return 4;
        case OP_FDIV_S: case OP_FDIV_D: return 12;
        case OP_FSQRT_S: case OP_FSQRT_D: return 12;
        default: return 1;
    }
}

static int ilp_is_load(int op) {
    return op == OP_LDB || op == OP_LDH || op == OP_LDW ||
           op == OP_LDBU || op == OP_LDHU;
}
static int ilp_is_store(int op) {
    return op == OP_STB || op == OP_STH || op == OP_STW;
}
/* f64 ops address register PAIRS; these read/write rd..rd+1. */
static int ilp_d_dst_pair(int op) {
    switch (op) {
        case OP_FADD_D: case OP_FSUB_D: case OP_FMUL_D: case OP_FDIV_D:
        case OP_FSQRT_D: case OP_FNEG_D: case OP_FABS_D:
        case OP_FCVT_D_W: case OP_FCVT_D_WU: case OP_FCVT_D_S:
        case OP_FCVT_L_S: case OP_FCVT_LU_S: case OP_FCVT_L_D:
        case OP_FCVT_LU_D: case OP_FCVT_D_L: case OP_FCVT_D_LU:
            return 1;
        default: return 0;
    }
}
static int ilp_d_src_pair(int op) {
    switch (op) {
        case OP_FADD_D: case OP_FSUB_D: case OP_FMUL_D: case OP_FDIV_D:
        case OP_FSQRT_D: case OP_FNEG_D: case OP_FABS_D:
        case OP_FEQ_D: case OP_FLT_D: case OP_FLE_D:
        case OP_FCVT_W_D: case OP_FCVT_WU_D: case OP_FCVT_S_D:
        case OP_FCVT_S_L: case OP_FCVT_S_LU: case OP_FCVT_L_D:
        case OP_FCVT_LU_D: case OP_FCVT_D_L: case OP_FCVT_D_LU:
            return 1;
        default: return 0;
    }
}

static ilp_mem_ent_t *ilp_mem_lookup(ilp_mem_ent_t *tbl, uint32_t waddr, int count_evict) {
    ilp_mem_ent_t *e = &tbl[waddr & ILP_MEM_MASK];
    if (!e->valid || e->tag != waddr) {
        if (e->valid && count_evict) g_ilp.evictions++;
        e->valid = 1;
        e->tag = waddr;
        e->st = 0;
        e->ld = 0;
    }
    return e;
}

static uint32_t ilp_u32max(uint32_t a, uint32_t b) { return a > b ? a : b; }

/* Called once per retired instruction, BEFORE it executes (so cpu->regs
 * still holds the values the effective address is computed from). */
static void ilp_record(const instruction_t *in, const uint32_t *regs, uint64_t icount) {
    int op, fmt, lat, i, w;
    int srcs[4], nsrc = 0;
    int dsts[2], ndst = 0;
    int is_ld, is_st, is_mem;
    uint32_t addr = 0, waddr = 0;

    if (icount < g_ilp.skip) return;
    if (g_ilp.analyzed >= g_ilp.count) return;
    g_ilp.analyzed++;

    op = in->opcode;
    fmt = in->format;
    lat = ilp_latency(op);
    is_ld = ilp_is_load(op);
    is_st = ilp_is_store(op);
    is_mem = is_ld || is_st;

    /* --- operand sets ------------------------------------------------ */
    switch (fmt) {
        case FMT_R: srcs[nsrc++] = in->rs1; srcs[nsrc++] = in->rs2; dsts[ndst++] = in->rd; break;
        case FMT_I: srcs[nsrc++] = in->rs1; dsts[ndst++] = in->rd; break;
        case FMT_S: srcs[nsrc++] = in->rs1; srcs[nsrc++] = in->rs2; break;
        case FMT_B: srcs[nsrc++] = in->rs1; srcs[nsrc++] = in->rs2; break;
        case FMT_U: dsts[ndst++] = in->rd; break;
        case FMT_J: if (op == OP_JALR) srcs[nsrc++] = in->rs1; dsts[ndst++] = in->rd; break;
        default: break;
    }
    if (op == OP_DEBUG) { nsrc = 0; srcs[nsrc++] = in->rs1; ndst = 0; }
    if (op == OP_NOP || op == OP_YIELD || op == OP_HALT) { nsrc = 0; ndst = 0; }

    /* f64 pair semantics: widen to the odd half. */
    if (ilp_d_src_pair(op)) {
        int n0 = nsrc;
        for (i = 0; i < n0 && nsrc < 4; i++)
            if (srcs[i] != 0) srcs[nsrc++] = srcs[i] + 1;
    }
    if (ilp_d_dst_pair(op) && ndst == 1 && dsts[0] != 0) dsts[ndst++] = dsts[0] + 1;

    /* --- mix --------------------------------------------------------- */
    if (is_ld) g_ilp.n_load++;
    else if (is_st) g_ilp.n_store++;
    else if (fmt == FMT_B) g_ilp.n_branch++;
    else if (op == OP_JAL || op == OP_JALR) g_ilp.n_jump++;
    else if (op == OP_MUL || op == OP_MULH || op == OP_MULHU) g_ilp.n_mul++;
    else if (op == OP_DIV || op == OP_REM) g_ilp.n_div++;
    else if (op >= OP_FADD_S && op <= OP_FCVT_D_LU) g_ilp.n_fp++;
    else if (fmt == FMT_R || fmt == FMT_I || fmt == FMT_U) g_ilp.n_alu++;
    else g_ilp.n_other++;

    if (is_mem) {
        addr = regs[in->rs1] + (uint32_t)in->imm;
        waddr = addr >> 2;
    }

    /* --- model 1: pure dataflow, memory perfectly disambiguated ------- */
    {
        uint32_t t = 0;
        for (i = 0; i < nsrc; i++) if (srcs[i]) t = ilp_u32max(t, g_ilp.r_df[srcs[i]]);
        for (i = 0; i < ndst; i++) if (dsts[i]) g_ilp.r_df[dsts[i]] = t + lat;
        g_ilp.cyc_df = ilp_u32max(g_ilp.cyc_df, t + lat);
    }

    /* --- model 2: + true memory dependences -------------------------- */
    {
        uint32_t t = 0;
        for (i = 0; i < nsrc; i++) if (srcs[i]) t = ilp_u32max(t, g_ilp.r_mem[srcs[i]]);
        if (is_mem) {
            ilp_mem_ent_t *e = ilp_mem_lookup(g_ilp.mem, waddr, 1);
            if (is_ld) {
                t = ilp_u32max(t, e->st);              /* RAW */
                e->ld = ilp_u32max(e->ld, t + lat);
            } else {
                t = ilp_u32max(t, ilp_u32max(e->st, e->ld));  /* WAW / WAR */
                e->st = t + lat;
            }
        }
        for (i = 0; i < ndst; i++) if (dsts[i]) g_ilp.r_mem[dsts[i]] = t + lat;
        g_ilp.cyc_mem = ilp_u32max(g_ilp.cyc_mem, t + lat);
    }

    /* --- model 3: + one memory port ---------------------------------- */
    {
        uint32_t t = 0;
        for (i = 0; i < nsrc; i++) if (srcs[i]) t = ilp_u32max(t, g_ilp.r_1p[srcs[i]]);
        if (is_mem) {
            ilp_mem_ent_t *e = ilp_mem_lookup(g_ilp.mem1p, waddr, 0);
            if (is_ld) t = ilp_u32max(t, e->st);
            else       t = ilp_u32max(t, ilp_u32max(e->st, e->ld));
            t = ilp_u32max(t, g_ilp.memport_free);     /* the single port */
            g_ilp.memport_free = t + 1;
            if (is_ld) e->ld = ilp_u32max(e->ld, t + lat);
            else       e->st = t + lat;
        }
        for (i = 0; i < ndst; i++) if (dsts[i]) g_ilp.r_1p[dsts[i]] = t + lat;
        g_ilp.cyc_1p = ilp_u32max(g_ilp.cyc_1p, t + lat);
    }

    /* --- model 4: in-order, W-wide, true memory deps ----------------- */
    if (g_ilp.width > 0) {
        uint32_t t = 0;
        for (i = 0; i < nsrc; i++) if (srcs[i]) t = ilp_u32max(t, g_ilp.r_w[srcs[i]]);
        if (is_mem) {
            ilp_mem_ent_t *e = ilp_mem_lookup(g_ilp.memw, waddr, 0);
            if (is_ld) t = ilp_u32max(t, e->st);
            else       t = ilp_u32max(t, ilp_u32max(e->st, e->ld));
        }
        /* in-order issue: never earlier than the current issue cycle */
        if (t > g_ilp.cyc_w) { g_ilp.cyc_w = t; g_ilp.slots_w = 0; }
        if (g_ilp.slots_w >= (uint32_t)g_ilp.width) {
            g_ilp.cyc_w++;
            g_ilp.slots_w = 0;
        }
        g_ilp.slots_w++;
        t = g_ilp.cyc_w;
        if (is_mem) {
            ilp_mem_ent_t *e = ilp_mem_lookup(g_ilp.memw, waddr, 0);
            if (is_ld) e->ld = ilp_u32max(e->ld, t + lat);
            else       e->st = t + lat;
        }
        for (i = 0; i < ndst; i++) if (dsts[i]) g_ilp.r_w[dsts[i]] = t + lat;
    }
}

static void ilp_report(void) {
    uint64_t n = g_ilp.analyzed;
    uint64_t mem = g_ilp.n_load + g_ilp.n_store;
    int w;
    if (!n) { fprintf(stderr, "ilp: no instructions analyzed\n"); return; }

    printf("\n=== ILP limit study ===\n");
    printf("instructions analyzed : %llu\n", (unsigned long long)n);
    printf("latency model         : %s\n", g_ilp.real_lat ? "realistic" : "unit");
    printf("memory shadow evictions: %llu (%.4f%% of mem ops)\n",
           (unsigned long long)g_ilp.evictions,
           mem ? 100.0 * (double)g_ilp.evictions / (double)mem : 0.0);
    printf("\nmix: load %.1f%%  store %.1f%%  branch %.1f%%  jump %.1f%%  "
           "alu %.1f%%  mul %.1f%%  div %.1f%%  fp %.1f%%  other %.1f%%\n",
           100.0*g_ilp.n_load/n, 100.0*g_ilp.n_store/n, 100.0*g_ilp.n_branch/n,
           100.0*g_ilp.n_jump/n, 100.0*g_ilp.n_alu/n, 100.0*g_ilp.n_mul/n,
           100.0*g_ilp.n_div/n, 100.0*g_ilp.n_fp/n, 100.0*g_ilp.n_other/n);
    printf("memory ops            : %.1f%%  -> single-port ceiling %.2f IPC\n",
           100.0*(double)mem/(double)n, mem ? (double)n/(double)mem : 0.0);

    printf("\nunbounded width + window, perfect branch prediction:\n");
    printf("  1. dataflow (perfect memory)   : %8.2f IPC   (%llu cycles)\n",
           (double)n / (double)g_ilp.cyc_df, (unsigned long long)g_ilp.cyc_df);
    printf("  2. + true memory dependences   : %8.2f IPC   (%llu cycles)\n",
           (double)n / (double)g_ilp.cyc_mem, (unsigned long long)g_ilp.cyc_mem);
    printf("  3. + single memory port        : %8.2f IPC   (%llu cycles)\n",
           (double)n / (double)g_ilp.cyc_1p, (unsigned long long)g_ilp.cyc_1p);

    if (g_ilp.width > 0) {
        uint32_t c = g_ilp.cyc_w + 1;
        printf("\nin-order, W=%d issue, true memory dependences:\n", g_ilp.width);
        printf("  W=%-4d : %6.2f IPC   (%.1f%% of dataflow limit)\n",
               g_ilp.width, (double)n / (double)c,
               100.0 * ((double)n / (double)c) / ((double)n / (double)g_ilp.cyc_df));
    }
    printf("\n");
}

#endif /* SLOW32_ILP_H */
