// SLOW-32 DBT5 — Stage 5 only experimental dynamic binary translator
//
// Clean-room tree.  This binary and everything under tools/dbt5/ is
// intentionally isolated from the production Stages 1-4 implementation
// in ../dbt/.  The goal is to allow Stage 5 (full SSA + BURG + RA +
// native codegen) to evolve without the gravity well of the mature
// per-instruction translator helpers.
//
// On this host (AArch64) native emission is not yet present.  The
// lifter + SSA + MIR + BURG + RA pipeline can run for analysis and
// diagnostics.  Execution falls back to the shadow interpreter.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <sys/mman.h>   // for the experimental A64 execution test path

#include "cpu_state.h"
#include "block_cache.h"
#include "shadow_interp.h"
#include "slow32_inst.h"

#include "pre/lift/stage5_lift.h"
#include "pre/ssa/stage5_ssa.h"
#include "pre/mir/stage5_mir.h"

// Target-specific includes (post-MIR split)
#if defined(__aarch64__)
#include "target/a64/codegen/stage5_codegen_a64.h"
#elif defined(__x86_64__)
#include "target/x64/codegen/stage5_codegen_x64.h"
#endif

// s32x loader lives in the emulator tree (self-contained header).
// The header contains several static helper functions that dbt5.c does not
// use (C6 cleanup). We suppress the resulting -Wunused-function warnings
// locally so we don't have to touch the shared header.
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
#include "../emulator/s32x_loader.h"
#pragma GCC diagnostic pop

// Bridge for the experimental A64 emitter (temporary until the experimental
// block is restructured to receive ctx/cache as a proper parameter).
static block_cache_t *g_a64_experimental_cache = NULL;

#if defined(__aarch64__)
// Atomic launcher for an emitted A64 block.
//
// All register setup (x20=&cpu, x21=mem_base) AND the call to the JIT block
// happen inside a single __asm__ volatile so the C compiler cannot reorder,
// spill across, or otherwise touch x20/x21 between setup and call. This is
// what the previous open-coded inline-asm preamble in the driver got wrong:
// without a unified block + clobber list, GCC was free to save/restore x20
// across the surrounding shadow_init / shadow_snapshot calls, undoing the
// `mov x20, &cpu` before the JIT got to use it.
//
// Guest live-in registers are NOT loaded here; the JIT block's prologue
// already emits `ldr wN, [x20, #gpr*4]` for every live-in slot. All this
// trampoline needs to guarantee is that x20 and x21 hold &cpu and mem_base
// when control transfers into the emitted code.
__attribute__((noinline))
static void dbt5_launch_a64(void (*fn)(void), void *cpu_ptr, void *mem_base)
{
    __asm__ volatile (
        // IMPORTANT: stage all three operand values into known-safe scratch
        // registers BEFORE we touch x20/x21. The "r" constraint lets GCC
        // pick any register for each input — and on AArch64 it happily
        // picks callee-saved x19/x20/x21 because they survive intervening
        // C code. If we then do `mov x21, %[mem]` while GCC has put `fn`
        // in x21, we overwrite fn's register before we BLR to it (and end
        // up branching to mem_base, which segfaults). Staging into x9/x10/x11
        // (listed below as clobbers so GCC won't pick them for the operands)
        // breaks that dependency.
        "mov x9,  %[fn]\n"
        "mov x10, %[cpu]\n"
        "mov x11, %[mem]\n"

        // Frame: save FP/LR (BLR clobbers LR) and the two callee-saved regs
        // we are about to repurpose as the JIT's ABI bridge.
        "stp x29, x30, [sp, #-16]!\n"
        "stp x20, x21, [sp, #-16]!\n"
        "mov x29, sp\n"

        // Hand the JIT its two ABI bridge registers.
        "mov x20, x10\n"
        "mov x21, x11\n"

        // Transfer to the emitted code. The block writes cpu->pc and
        // cpu->exit_reason via x20 before its own ret.
        "blr x9\n"

        // Restore everything we touched. SP returns balanced.
        "ldp x20, x21, [sp], #16\n"
        "ldp x29, x30, [sp], #16\n"
        :
        : [fn]"r"(fn), [cpu]"r"(cpu_ptr), [mem]"r"(mem_base)
        : "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7",
          "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15",
          "x16", "x17",
          // NOTE: x18 deliberately omitted — Apple Silicon reserves it as the
          // platform register and clobber-listing it is an error.
          "memory", "cc"
    );
}
#endif

// Memory size for the clean-room driver.
// Many self-hosted tools (s32-as, sbasic, etc.) request 256 MiB.
#define DBT5_MEM_SIZE (256 * 1024 * 1024)

// Simple write callback for the loader into our flat guest memory buffer
static int dbt5_write_mem(void *user_data, uint32_t addr, const void *data, uint32_t size) {
    uint8_t *mem = (uint8_t *)user_data;
    if (addr + size > DBT5_MEM_SIZE) return -1;
    memcpy(mem + addr, data, size);
    return 0;
}

static void print_banner(void) {
    fprintf(stderr,
            "SLOW-32 Stage 5 DBT (clean room) — experimental\n"
            "  No Stage 1-4 translator code is present in this binary.\n"
            "  Lifter/SSA/BURG/RA pipeline is active for analysis.\n"
            "  Native AArch64 codegen is TBD (x86 path exists on Intel hosts).\n\n");
}

static void usage(const char *prog) {
    fprintf(stderr,
            "Usage: %s <program.s32x> [--lift-only] [--verbose]\n"
            "       %s --help\n",
            prog, prog);
}

// Run the complete Stage 5 analysis pipeline on a lifted region.
// This is the core of what the clean-room driver should do today:
//   lift → SSA overlay → MIR → BURG lower → LIR opt → RA
// We stop before native emission (A64 codegen TBD).
static void run_full_stage5_analysis(const stage5_lift_region_t *region,
                                     const char *tag,
                                     bool verbose)
{
    if (!region || !region->ir_count) {
        fprintf(stderr, "  [%s] empty region, skipping pipeline\n", tag);
        return;
    }

    stage5_ssa_overlay_t ssa;
    if (!stage5_ssa_build_overlay(region, &ssa)) {
        fprintf(stderr, "  [%s] SSA overlay FAILED\n", tag);
        return;
    }

    stage5_mir_t mir;
    stage5_mir_build(region, &ssa, &mir);

    stage5_lir_t lir;
    if (!stage5_burg_lower(&mir, &ssa, &lir)) {
        fprintf(stderr, "  [%s] BURG lower FAILED\n", tag);
        return;
    }

    stage5_lir_optimize(&lir, &ssa);

    stage5_ra_plan_t ra;
    if (!stage5_ra_build_plan_lir(&lir, &ssa, &ra, region)) {
        fprintf(stderr, "  [%s] RA FAILED\n", tag);
        return;
    }

    // Success — full pipeline completed in the clean room
    fprintf(stderr,
            "  [%s] FULL PIPELINE OK  MIR=%u  LIR=%u  RA_intervals=%u  spills=%u\n",
            tag, mir.node_count, lir.node_count, ra.interval_count, ra.spilled_count);

    // Basic SSA stats (always shown, cheap and useful)
    fprintf(stderr, "    SSA: values=%u", region->value_count);
    if (verbose) {
        // Count constants from the overlay
        uint32_t const_count = 0;
        uint32_t total_uses = 0;
        for (uint16_t v = 0; v < ssa.value_count; v++) {
            if (ssa.value_is_const[v]) const_count++;
            total_uses += ssa.value_use_count[v];
        }
        double avg_uses = ssa.value_count ? (double)total_uses / ssa.value_count : 0.0;
        fprintf(stderr, "  consts=%u  avg_uses=%.1f", const_count, avg_uses);
    }
    fprintf(stderr, "\n");

    if (verbose) {
        // MIR dump (architecture-neutral layer)
        uint32_t mir_dump = mir.node_count < 12 ? mir.node_count : 12;
        fprintf(stderr, "    MIR dump (first %u of %u):\n", mir_dump, mir.node_count);
        for (uint32_t i = 0; i < mir_dump; i++) {
            const mir_node_t *m = &mir.nodes[i];
            uint8_t dst_g = (m->dst_v < STAGE5_SSA_MAX_VALUES) ? ssa.value_to_reg[m->dst_v] : 0;
            fprintf(stderr, "      [%02u] %-8s  dst=v%-3u(g%-2u)  src=[v%-3u,v%-3u]  imm=%d  gpc=0x%08X\n",
                    i, mir_op_name(m->op),
                    m->dst_v, dst_g, m->src_v[0], m->src_v[1], m->imm, m->guest_pc);
        }
        if (mir.node_count > mir_dump)
            fprintf(stderr, "      ... (%u more MIR nodes)\n", mir.node_count - mir_dump);

        // Rich LIR dump
        uint32_t max_lir = lir.node_count < 12 ? lir.node_count : 12;
        fprintf(stderr, "    LIR dump (first %u of %u):\n", max_lir, lir.node_count);
        for (uint32_t i = 0; i < max_lir; i++) {
            const lir_node_t *n = &lir.nodes[i];
            uint8_t dst_gpr = (n->dst_v < STAGE5_SSA_MAX_VALUES) ? ssa.value_to_reg[n->dst_v] : 0;
            fprintf(stderr,
                    "      [%02u] %-12s  dst=v%-3u(g%-2u)  src=[v%-3u,v%-3u]  imm=%-6d  gpc=0x%08X\n",
                    i, lir_op_name(n->op),
                    n->dst_v, dst_gpr,
                    n->src_v[0], n->src_v[1],
                    n->imm, n->guest_pc);
        }
        if (lir.node_count > max_lir)
            fprintf(stderr, "      ... (%u more LIR nodes)\n", lir.node_count - max_lir);

        // RA assignment summary
        if (ra.interval_count > 0) {
#if defined(__aarch64__)
            int host_slot_budget = STAGE5_A64_MAX_HOST_SLOTS;  // 14
#else
            int host_slot_budget = 8;
#endif
            fprintf(stderr, "    RA assignments (%d-slot pool, %u intervals, %u spills):\n",
                    host_slot_budget, ra.interval_count, ra.spilled_count);

            int shown = 0;
            for (uint32_t i = 0; i < ra.interval_count && shown < 10; i++) {
                const stage5_ra_interval_t *iv = &ra.intervals[i];
                if (iv->value_id == 0) continue;
                uint8_t gpr = (iv->value_id < STAGE5_SSA_MAX_VALUES) ? ssa.value_to_reg[iv->value_id] : 0;
                if (iv->spilled) {
                    fprintf(stderr, "      val=%-4u (g%-2u)  -> SPILLED   [live %u..%u]\n",
                            iv->value_id, gpr, iv->start_idx, iv->end_idx);
                } else {
                    fprintf(stderr, "      val=%-4u (g%-2u)  -> slot %d    [live %u..%u]\n",
                            iv->value_id, gpr, iv->assigned_slot, iv->start_idx, iv->end_idx);
                }
                shown++;
            }
            if (ra.interval_count > shown)
                fprintf(stderr, "      ... (%u more intervals)\n", ra.interval_count - shown);

            // Pressure curve derived from the RA live intervals (best possible view we have)
            if (lir.node_count > 1) {
                uint32_t max_p = 0;
                uint32_t pressure_points[32] = {0}; // sample up to 32 points
                int samples = (lir.node_count > 32) ? 32 : lir.node_count;

                for (int s = 0; s < samples; s++) {
                    uint32_t step = (s * (lir.node_count-1)) / (samples-1);
                    uint32_t p = 0;
                    for (uint32_t iv = 0; iv < ra.interval_count; iv++) {
                        const stage5_ra_interval_t *r = &ra.intervals[iv];
                        if (r->value_id != 0 && !r->spilled &&
                            r->start_idx <= step && step <= r->end_idx) p++;
                    }
                    if (p > max_p) max_p = p;
                    if (s < 32) pressure_points[s] = p;
                }

#if defined(__aarch64__)
                int host_slot_budget = STAGE5_A64_MAX_HOST_SLOTS;  // 14
#else
                int host_slot_budget = 8;
#endif
                fprintf(stderr, "    Register pressure (from RA live ranges, %d-slot pool): max=%u\n",
                        host_slot_budget, max_p);
                if (max_p > (uint32_t)host_slot_budget) {
                    fprintf(stderr, "      WARNING: peak pressure %u > %d host slots — real spills likely\n",
                            max_p, host_slot_budget);
                }

                // Simple ASCII sparkline
                fprintf(stderr, "      pressure: ");
                for (int s = 0; s < samples && s < 24; s++) {
                    uint32_t p = pressure_points[s];
                    if (p == 0) fprintf(stderr, "_");
                    else if (p <= 3) fprintf(stderr, ".");
                    else if (p <= 6) fprintf(stderr, "o");
                    else fprintf(stderr, "*");
                }
                fprintf(stderr, "\n");
            }
        }

        // Guest register flow / pressure (from the lifter's analysis)
        if (region->reg_flow_valid) {
            fprintf(stderr, "    Guest register flow (use/def counts, live across edges):\n");
            // Show top 8 by use_count
            int order[32];
            for (int r = 0; r < 32; r++) order[r] = r;
            // Simple bubble sort by use_count
            for (int i = 0; i < 32; i++) {
                for (int j = i+1; j < 32; j++) {
                    if (region->reg_flow[order[j]].use_count > region->reg_flow[order[i]].use_count) {
                        int t = order[i]; order[i] = order[j]; order[j] = t;
                    }
                }
            }
            int printed = 0;
            for (int k = 0; k < 32 && printed < 8; k++) {
                int r = order[k];
                const stage5_reg_flow_t *f = &region->reg_flow[r];
                if (f->use_count == 0 && f->def_count == 0) continue;
                fprintf(stderr, "      r%-2d  uses=%-3u  defs=%-3u  live_blks=%u  across_edge=%s\n",
                        r, f->use_count, f->def_count, f->live_blocks,
                        f->live_across_edge ? "yes" : "no");
                printed++;
            }
            if (region->cfg_spill_likely) {
                fprintf(stderr, "    Note: cfg_max_live=%u  → spill_likely=true (high guest reg pressure)\n",
                        region->cfg_max_live);
            }
        }

        if (verbose) {
            // Quick "would we want to own this natively?" summary
            int viability = 100;
            const char *reasons[4] = {0};
            int rcount = 0;

            if (ra.spilled_count > 0) { viability -= 30; reasons[rcount++] = "spills present"; }
            if (region->cfg_spill_likely) { viability -= 25; reasons[rcount++] = "high register pressure"; }
            if (region->side_exit_count > 2) { viability -= 15; reasons[rcount++] = "many side exits"; }
            if (region->cfg_block_count > 8) { viability -= 10; reasons[rcount++] = "complex CFG"; }

            if (viability < 0) viability = 0;

            fprintf(stderr, "    Viability for native ownership: %d/100", viability);
            if (rcount > 0) {
                fprintf(stderr, " (");
                for (int i = 0; i < rcount; i++) {
                    if (i > 0) fprintf(stderr, ", ");
                    fprintf(stderr, "%s", reasons[i]);
                }
                fprintf(stderr, ")");
            }
            fprintf(stderr, "\n");
        }
    }

        // Per-CFG-block breakdown (when the lifter built CFG)
        if (region->cfg_valid && region->cfg_block_count > 1) {
            fprintf(stderr, "    CFG blocks (%u):\n", region->cfg_block_count);
            for (uint32_t b = 0; b < region->cfg_block_count && b < 6; b++) {
                const stage5_cfg_block_t *blk = &region->cfg_blocks[b];
                int live_in = __builtin_popcount(blk->live_in_mask);
                int live_out = __builtin_popcount(blk->live_out_mask);
                fprintf(stderr, "      blk%u  insts=%-2u  live_in=%-2d  live_out=%-2d  term=%d  succ=%u\n",
                        b, blk->inst_count, live_in, live_out, blk->term_kind, blk->succ_count);
            }
            if (region->cfg_block_count > 6)
                fprintf(stderr, "      ... (%u more blocks)\n", region->cfg_block_count - 6);
        }

    (void)stage5_burg_select; // silence unused warning for now (scaffolding)
}

int main(int argc, char **argv) {
    print_banner();

    const char *filename = NULL;
    bool lift_only = false;
    bool verbose = false;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
            usage(argv[0]);
            return 0;
        } else if (strcmp(argv[i], "--lift-only") == 0) {
            lift_only = true;
        } else if (strcmp(argv[i], "--verbose") == 0 || strcmp(argv[i], "-v") == 0) {
            verbose = true;
        } else if (argv[i][0] != '-') {
            if (!filename) filename = argv[i];
        }
    }

    if (!filename) {
        usage(argv[0]);
        return 1;
    }

    // Allocate guest memory and CPU state
    uint8_t *mem = (uint8_t *)calloc(1, DBT5_MEM_SIZE);
    if (!mem) {
        fprintf(stderr, "dbt5: out of memory for guest RAM\n");
        return 1;
    }

    dbt_cpu_state_t cpu;
    memset(&cpu, 0, sizeof(cpu));
    cpu.mem_base = mem;
    cpu.mem_size = DBT5_MEM_SIZE;
    cpu.code_limit = 0x200000;   // same conservative limit as production

    // Load the .s32x image using the real loader (proper sections + layout)
    s32x_loader_config_t cfg = {
        .write_cb = dbt5_write_mem,
        .user_data = mem,
        .mem_size = DBT5_MEM_SIZE,
        .verbose = 0
    };

    s32x_load_result_t load_res = load_s32x_file(filename, &cfg);
    if (!load_res.success) {
        fprintf(stderr, "dbt5: load failed: %s\n", load_res.error_msg);
        free(mem);
        return 1;
    }

    cpu.pc = load_res.entry_point;
    cpu.code_limit = load_res.code_limit;

    fprintf(stderr, "dbt5: loaded %s  entry=0x%08X  code_limit=0x%08X  mem_size=%u\n",
            filename, load_res.entry_point, load_res.code_limit, load_res.mem_size);

    // ========================================================================
    // Run the FULL Stage 5 pipeline in the clean room (the point of this tree)
    // ========================================================================

#if defined(__aarch64__)
    // Activation for the experimental A64 pilot path (clean-room driver).
    // Setting S5_EMIT_A64 enables:
    //   - Clean emission via stage5_codegen_a64
    //   - Real block cache installation (cache_init + cache_alloc_block + cache_insert)
    //   - Optional direct execution of the emitted code for testing/validation
    if (getenv("S5_EMIT_A64")) {
        static block_cache_t experimental_cache = {0};
        if (!cache_init(&experimental_cache)) {
            fprintf(stderr, "  [A64-CACHE] cache_init failed\n");
            g_a64_experimental_cache = NULL;
        } else {
            g_a64_experimental_cache = &experimental_cache;
            fprintf(stderr, "  [A64-CACHE] experimental cache ready (pool=%u blocks, code_buf=%u)\n",
                    experimental_cache.block_pool_size, experimental_cache.code_buffer_size);
        }
    }
#endif
    stage5_lift_region_t region;
    stage5_lift_region_init(&region, load_res.entry_point);

    bool lifted = stage5_lift_superblock(&region, cpu.mem_base, cpu.code_limit,
                                         STAGE5_LIFT_BUDGET);
    if (lifted) {
        fprintf(stderr,
                "Stage5 lift: OK  insts=%u  ir=%u  cfg_blks=%u  side=%u\n",
                region.guest_inst_count, region.ir_count,
                region.cfg_block_count, region.side_exit_count);

        // This is the key call — full independent Stage 5 analysis
        run_full_stage5_analysis(&region, "entry", verbose);

        // ------------------------------------------------------------------
        // Experimental A64 emitter path (pilot in clean-room driver).
        //
        // Setting S5_EMIT_A64 enables the clean independent backend to:
        //   - Emit using stage5_codegen_a64
        //   - Wire the result into the real block cache
        //   - (When not using --lift-only) attempt direct execution for validation
        //
        // The global bridge g_a64_experimental_cache is populated automatically.
        // ------------------------------------------------------------------
#if defined(__aarch64__)
        if (getenv("S5_EMIT_A64")) {
            static uint8_t a64_code[64 * 1024];
            stage5_cg_a64_ctx_t a64_cg;
            stage5_cg_a64_init(&a64_cg, a64_code, sizeof(a64_code));

            // D3: reuse the region the lifter already produced for the entry point.
            // No need to go back to guest memory and re-decode. We just build a
            // fresh SSA overlay over the existing IR (cheap) and feed the same
            // region to the emitter. This eliminates the temporary re-lift that
            // the original scaffolding comment promised we would remove.
            stage5_ssa_overlay_t ssa_for_emit;
            if (stage5_ssa_build_overlay(&region, &ssa_for_emit))
            {
                stage5_lir_t tmp_lir;
                stage5_mir_t tmp_mir;
                stage5_ra_plan_t tmp_ra;

                stage5_mir_build(&region, &ssa_for_emit, &tmp_mir);
                if (stage5_burg_lower(&tmp_mir, &ssa_for_emit, &tmp_lir)) {
                    stage5_lir_optimize(&tmp_lir, &ssa_for_emit);
                    stage5_ra_build_plan_lir(&tmp_lir, &ssa_for_emit, &tmp_ra, &region);

                    bool emitted = stage5_codegen_a64(&a64_cg, &region, &ssa_for_emit, &tmp_lir, &tmp_ra);
                    if (emitted) {
                        fprintf(stderr, "  [A64-EMIT] clean emitter succeeded (%zu bytes)\n",
                                emit_offset(&a64_cg.emit));
                    } else {
                        fprintf(stderr, "  [A64-EMIT] codegen bailed — this region will run under shadow interpreter\n");
                    }

                    if (emitted && a64_cg.exit_count > 0) {
                        fprintf(stderr, "           recorded %u exit(s), first target=0x%08X\n",
                                a64_cg.exit_count, a64_cg.exits[0].target_pc);
                    }

                    // ------------------------------------------------------------------
                    // Real wiring: create translated_block_t and insert the cleanly
                    // emitted A64 code into the block cache.
                    // ------------------------------------------------------------------
                    if (emitted) {
                        size_t code_len = a64_cg.emit.offset;

                        // Use portable JIT mmap flags + write-protect bracketing (fixes B2 on Apple Silicon)
                        void *exec_code = mmap(NULL, code_len + 4096,
                                               PROT_READ | PROT_WRITE | PROT_EXEC,
                                               DBT_JIT_MMAP_FLAGS, -1, 0);
                        if (exec_code != MAP_FAILED) {
                            dbt_jit_writable_begin();
                            memcpy(exec_code, a64_code, code_len);
                            dbt_clear_icache(exec_code, (char *)exec_code + code_len);
                            dbt_jit_writable_end();

                            if (g_a64_experimental_cache) {
                                translated_block_t *blk = cache_alloc_block(g_a64_experimental_cache, load_res.entry_point);
                                if (blk) {
                                    blk->host_code  = exec_code;
                                    blk->host_size  = code_len;
                                    blk->guest_size = region.guest_inst_count * 4;
                                    blk->exit_count = a64_cg.exit_count;

                                    for (uint32_t e = 0; e < a64_cg.exit_count && e < MAX_BLOCK_EXITS; e++) {
                                        blk->exits[e].target_pc = a64_cg.exits[e].target_pc;
                                    }

                                    cache_insert(g_a64_experimental_cache, blk);

                                    fprintf(stderr, "  [A64-WIRE] installed clean-emitted block into cache (%zu bytes)\n", code_len);
                                } else {
                                    munmap(exec_code, code_len + 4096);
                                }
                            } else {
                                fprintf(stderr, "  [A64-WIRE] wiring skipped (no cache set via g_a64_experimental_cache)\n");
                                munmap(exec_code, code_len + 4096);
                            }
                        }
                    }

                    // ------------------------------------------------------------------
                    // Experimental: actually try to execute the emitted A64 code
                    // (very early, gated by S5_EMIT_A64=exec)
                    // ------------------------------------------------------------------
                    // Execute the emitted code immediately when the experimental flag is set.
                    // (We can later make this conditional on =exec if someone wants wiring-only.)
                    if (emitted && getenv("S5_EMIT_A64")) {
                        // Make a copy in an executable mapping (portable JIT version)
                        void *exec_mem = mmap(NULL, 64*1024, PROT_READ | PROT_WRITE | PROT_EXEC,
                                              DBT_JIT_MMAP_FLAGS, -1, 0);
                        if (exec_mem == MAP_FAILED) {
                            perror("mmap exec");
                        } else {
                            dbt_jit_writable_begin();
                            memcpy(exec_mem, a64_code, a64_cg.emit.offset);
                            dbt_clear_icache(exec_mem, (char *)exec_mem + a64_cg.emit.offset);
                            dbt_jit_writable_end();

                            cpu.pc = load_res.entry_point;

                            typedef void (*block_fn_t)(void);
                            block_fn_t fn = (block_fn_t)exec_mem;

                            fprintf(stderr, "  [A64-EXEC] jumping to emitted code @ %p (len=%zu)...\n",
                                    exec_mem, a64_cg.emit.offset);

                            // Snapshot pre-emit CPU state for B3 validation. This must
                            // happen BEFORE the launcher so that the snapshot reflects
                            // what the shadow interpreter will start from.
                            shadow_state_t sh;
                            shadow_init(&sh, &cpu);
                            shadow_snapshot(&sh, &cpu);

                            // Atomic launch — see dbt5_launch_a64() above.
                            dbt5_launch_a64(fn, &cpu, cpu.mem_base);

                            uint32_t emitted_pc = cpu.pc;
                            uint32_t emitted_exit = cpu.exit_reason;

                            fprintf(stderr, "  [A64-EXEC] returned. pc=0x%08X exit_reason=%u\n",
                                    emitted_pc, emitted_exit);

                            // Real shadow execution (B3) — run from the pre-snapshot.
                            // Bound shadow by the region's pc range: the JIT owns
                            // exactly the guest code in [start_pc, end_pc). When
                            // shadow leaves that range it has reached the same
                            // "exited the region" boundary the JIT did, and the
                            // states should agree. Stepping further would have
                            // shadow execute callees the JIT never owned (e.g.
                            // memset from crt0) and produce a spurious mismatch.
                            uint32_t shadow_steps = 0;
                            const uint32_t MAX_TEST_STEPS = 10000;
                            while (shadow_steps < MAX_TEST_STEPS) {
                                if (sh.pc < region.start_pc || sh.pc >= region.end_pc) break;
                                if (sh.pc >= cpu.code_limit) break;
                                bool ended = shadow_step_one(&sh);
                                shadow_steps++;
                                if (ended) break;
                            }

                            uint32_t shadow_pc = sh.pc;
                            uint32_t shadow_exit = (sh.hit_halt || sh.hit_yield || sh.hit_debug) ? 0x7F : 2;

                            fprintf(stderr, "  [SHADOW-EXEC] returned. pc=0x%08X exit_reason=%u (steps=%u)\n",
                                    shadow_pc, shadow_exit, shadow_steps);

                            // Richer validation (polished B3)
                            bool pc_ok   = (emitted_pc == shadow_pc);
                            bool exit_ok = (emitted_exit == shadow_exit);
                            bool sp_ok   = (cpu.regs[29] == sh.regs[29]);   // r29 = sp

                            if (pc_ok && exit_ok && sp_ok) {
                                fprintf(stderr, "  [VALIDATION] PASS: emitted and shadow results match (pc, exit, sp)\n");
                            } else {
                                fprintf(stderr, "  [VALIDATION] FAIL: mismatch!\n");
                                if (!pc_ok)   fprintf(stderr, "                 pc mismatch: emitted=0x%08X shadow=0x%08X\n", emitted_pc, shadow_pc);
                                if (!exit_ok) fprintf(stderr, "                 exit mismatch: emitted=%u shadow=%u\n", emitted_exit, shadow_exit);
                                if (!sp_ok)   fprintf(stderr, "                 sp mismatch: emitted=%u shadow=%u\n", cpu.regs[29], sh.regs[29]);
                                fprintf(stderr, "                 emitted: pc=0x%08X exit=%u sp=%u\n", emitted_pc, emitted_exit, cpu.regs[29]);
                                fprintf(stderr, "                 shadow : pc=0x%08X exit=%u sp=%u\n", shadow_pc, shadow_exit, sh.regs[29]);
                            }

                            munmap(exec_mem, 64*1024);
                        }
                    }
                }
            }
        }
#elif defined(__x86_64__)
        if (getenv("S5_EMIT_X64")) {
            static uint8_t x64_code[64 * 1024];
            stage5_cg_x64_ctx_t x64_cg = {0};
            x64_cg.cpu = &cpu;
            emit_init(&x64_cg.emit, x64_code, sizeof(x64_code));
            // No block_cache wired up at the dbt5 driver level yet; the
            // emitter tolerates cache == NULL (RET-based exits instead of
            // chained JMPs).  This is enough for a compile-and-emit smoke.

            bool emitted = stage5_codegen_x64(&x64_cg, &region, region.start_pc);
            fprintf(stderr, "  [X64-EMIT] clean emitter %s (%zu bytes emitted)\n",
                    emitted ? "succeeded" : "failed", emit_offset(&x64_cg.emit));
        }
#endif

        // === Additional regions from the CFG the lifter discovered ===
        if (region.cfg_valid && region.cfg_block_count > 1 && verbose) {
            fprintf(stderr, "\n  --- Additional CFG blocks analyzed ---\n");
            int extra = 0;
            for (uint32_t b = 1; b < region.cfg_block_count && extra < 3; b++) {
                uint32_t pc = region.cfg_blocks[b].start_pc;
                if (pc == 0 || pc == region.start_pc) continue;

                stage5_lift_region_t extra_region;
                // D3: slice from the already-lifted parent instead of re-decoding
                // guest memory. This is the core of the "no more re-lifting per
                // CFG block" fix.
                if (stage5_lift_extract_cfg_block_region(&region, b, &extra_region)) {
                    char tag[32];
                    snprintf(tag, sizeof(tag), "blk%u", b);
                    run_full_stage5_analysis(&extra_region, tag, true);
                    extra++;
                } else {
                    // Fallback for blocks that are successors outside the
                    // original lifted superblock (rare in the current pilot).
                    stage5_lift_region_init(&extra_region, pc);
                    if (stage5_lift_superblock(&extra_region, cpu.mem_base, cpu.code_limit, STAGE5_LIFT_BUDGET)) {
                        char tag[32];
                        snprintf(tag, sizeof(tag), "blk%u", b);
                        run_full_stage5_analysis(&extra_region, tag, true);
                        extra++;
                    }
                }
            }
            if (extra == 0) {
                fprintf(stderr, "  (no additional liftable blocks in this region)\n");
            }
        }
    } else {
        fprintf(stderr, "Stage5 lift: FAILED  reason=%d at pc=0x%08X\n",
                region.reason, region.start_pc);
    }

    if (lift_only) {
        fprintf(stderr, "dbt5: --lift-only, exiting after full pipeline analysis.\n");
        free(mem);
        return 0;
    }

    // Real execution still falls back to shadow interpreter for now
    // (until we grow native A64 emission inside this clean tree).
    fprintf(stderr, "dbt5: falling back to shadow interpreter (no native Stage 5 emission on this host yet)\n");

    shadow_state_t sh;
    shadow_init(&sh, &cpu);

    const uint32_t MAX_STEPS = 10000000;
    uint32_t steps = 0;
    while (steps < MAX_STEPS) {
        if (cpu.pc >= cpu.code_limit) {
            fprintf(stderr, "dbt5: pc 0x%08X hit code limit, stopping\n", cpu.pc);
            break;
        }
        uint32_t raw = *(uint32_t *)(cpu.mem_base + cpu.pc);
        decoded_inst_t inst = decode_instruction(raw);

        if (inst.opcode == OP_HALT || inst.opcode == OP_YIELD || inst.opcode == OP_DEBUG) {
            fprintf(stderr, "dbt5: guest terminal op 0x%02X at 0x%08X after %u steps\n",
                    inst.opcode, cpu.pc, steps);
            break;
        }

        cpu.pc += 4;
        steps++;
        if (steps % 2000000 == 0)
            fprintf(stderr, "dbt5: ... %u steps (pc=0x%08X)\n", steps, cpu.pc);
    }

    fprintf(stderr, "dbt5: finished (shadow path, %u steps)\n", steps);
    free(mem);
    return 0;
}
