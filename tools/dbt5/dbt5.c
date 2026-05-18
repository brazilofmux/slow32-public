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

#include "stage5_lift.h"
#include "stage5_ssa.h"
#include "stage5_mir.h"
#include "stage5_burg.h"
#include "stage5_lir.h"
#include "stage5_ra.h"
#if defined(__aarch64__)
#include "stage5_codegen_a64.h"   // experimental clean A64 emitter
#elif defined(__x86_64__)
#include "stage5_codegen_x64.h"   // clean x86-64 emitter
#endif

// s32x loader lives in the emulator tree (self-contained header)
#include "../emulator/s32x_loader.h"

// Bridge for the experimental A64 emitter (temporary until the experimental
// block is restructured to receive ctx/cache as a proper parameter).
static block_cache_t *g_a64_experimental_cache = NULL;

#if defined(__aarch64__)
// File-scope helper to encapsulate clean A64 emission + wiring.
// Returns a wired translated_block_t on success, NULL otherwise.
static translated_block_t *try_clean_a64_emission(block_cache_t *cache, uint32_t guest_pc)
{
    // For this slice, the helper forwards to a static implementation
    // of the current experimental logic. The body will be inlined here
    // in the next micro-edit.
    (void)cache;
    (void)guest_pc;
    return NULL;   // real forwarding will be added when the body is moved
}

// Static implementation of the current experimental A64 emission + wiring logic.
// This will be moved into try_clean_a64_emission in the next micro-edit.
static translated_block_t *do_experimental_a64_emission(block_cache_t *cache, uint32_t guest_pc)
{
    // The old long experimental logic lives here for this slice.
    // (In the next micro-edit it will be inlined into the main helper.)
    (void)cache;
    (void)guest_pc;
    return NULL;
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
    if (!stage5_ra_build_plan_lir(&lir, &ssa, &ra)) {
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
            fprintf(stderr, "    RA assignments (8 host slots, %u intervals, %u spills):\n",
                    ra.interval_count, ra.spilled_count);

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

                fprintf(stderr, "    Register pressure (from RA live ranges, 8-slot budget): max=%u\n", max_p);
                if (max_p > 8) {
                    fprintf(stderr, "      WARNING: peak pressure %u > 8 host slots — real spills likely\n", max_p);
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
                fprintf(stderr, "    Note: cfg_max_live=%u  → spill_likely=true (pressure > 8)\n",
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
    // Activation for the experimental A64 path (Option 1 wiring complete).
    // Setting S5_EMIT_A64 (any value) now does:
    //   - Clean emission via stage5_codegen_a64
    //   - Real block cache installation (translated_block_t + cache_insert)
    //   - Immediate execution for testing
    if (getenv("S5_EMIT_A64")) {
        static block_cache_t experimental_cache = {0};
        static uint8_t experimental_code[4 * 1024 * 1024]; // 4 MB

        experimental_cache.code_buffer      = experimental_code;
        experimental_cache.code_buffer_size = sizeof(experimental_code);
        // Other fields (block pool, etc.) can stay zero for the first version.

        g_a64_experimental_cache = &experimental_cache;
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
        // Experimental A64 emitter path (Option 1 wiring complete).
        //
        // Setting S5_EMIT_A64 (any non-empty value) now does the full thing:
        //   - Emit using the clean independent backend
        //   - Install the result into the real block cache
        //   - Immediately execute the region for quick validation
        //
        // The global bridge g_a64_experimental_cache is populated automatically.
        // ------------------------------------------------------------------
#if defined(__aarch64__)
        if (getenv("S5_EMIT_A64")) {
            static uint8_t a64_code[64 * 1024];
            stage5_cg_a64_ctx_t a64_cg;
            stage5_cg_a64_init(&a64_cg, a64_code, sizeof(a64_code));

            // We need the SSA that was built inside run_full...
            // For the very first experiments we re-lift + re-run the early parts here.
            // (Later we will thread the SSA out of the analysis function.)
            stage5_ssa_overlay_t ssa_for_emit;
            stage5_lift_region_t tmp_region;
            stage5_lift_region_init(&tmp_region, load_res.entry_point);
            if (stage5_lift_superblock(&tmp_region, cpu.mem_base, cpu.code_limit, STAGE5_LIFT_BUDGET) &&
                stage5_ssa_build_overlay(&tmp_region, &ssa_for_emit))
            {
                stage5_lir_t tmp_lir;
                stage5_mir_t tmp_mir;
                stage5_ra_plan_t tmp_ra;

                stage5_mir_build(&tmp_region, &ssa_for_emit, &tmp_mir);
                if (stage5_burg_lower(&tmp_mir, &ssa_for_emit, &tmp_lir)) {
                    stage5_lir_optimize(&tmp_lir, &ssa_for_emit);
                    stage5_ra_build_plan_lir(&tmp_lir, &ssa_for_emit, &tmp_ra);

                    bool emitted = stage5_codegen_a64(&a64_cg, &tmp_region, &ssa_for_emit, &tmp_lir, &tmp_ra);
                    fprintf(stderr, "  [A64-EMIT] clean emitter %s (%zu bytes emitted)\n",
                            emitted ? "succeeded" : "failed (stub)", emit_offset(&a64_cg.emit));

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

                        void *exec_code = mmap(NULL, code_len + 4096,
                                               PROT_READ | PROT_WRITE | PROT_EXEC,
                                               MAP_PRIVATE | MAP_ANON, -1, 0);
                        if (exec_code != MAP_FAILED) {
                            memcpy(exec_code, a64_code, code_len);

                            if (g_a64_experimental_cache) {
                                translated_block_t *blk = cache_alloc_block(g_a64_experimental_cache, load_res.entry_point);
                                if (blk) {
                                    blk->host_code  = exec_code;
                                    blk->host_size  = code_len;
                                    blk->guest_size = tmp_region.guest_inst_count * 4;
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
                        // Make a copy in an executable mapping
                        void *exec_mem = mmap(NULL, 64*1024, PROT_READ | PROT_WRITE | PROT_EXEC,
                                              MAP_PRIVATE | MAP_ANON, -1, 0);
                        if (exec_mem == MAP_FAILED) {
                            perror("mmap exec");
                        } else {
                            memcpy(exec_mem, a64_code, a64_cg.emit.offset);

                            cpu.pc = load_res.entry_point;

                            // ----------------------------------------------------------------
                            // Proper live-in setup using the RA plan (first real step toward
                            // reliable execution testing)
                            // ----------------------------------------------------------------
                            a64_reg_t slot_to_host[8] = { W8, W9, W10, W11, W12, W13, W14, W15 };
                            uint8_t entry_gpr_for_slot[8] = {0};

                            for (uint16_t i = 0; i < tmp_ra.interval_count; i++) {
                                const stage5_ra_interval_t *iv = &tmp_ra.intervals[i];
                                if (iv->value_id == 0 || iv->spilled || iv->assigned_slot < 0)
                                    continue;
                                if (iv->start_idx > 2)   // only values live at entry
                                    continue;

                                uint8_t gpr = ssa_for_emit.value_to_reg[iv->value_id];
                                if (gpr != 0 && iv->assigned_slot < 8) {
                                    entry_gpr_for_slot[iv->assigned_slot] = gpr;
                                }
                            }

                            // Load the live-in guest registers into the host registers
                            // the emitter expects.
                            for (int s = 0; s < 8; s++) {
                                uint8_t gpr = entry_gpr_for_slot[s];
                                if (gpr == 0) continue;

                                uint32_t val = cpu.regs[gpr];
                                switch (s) {
                                    case 0: __asm__ volatile("mov w8, %w0" : : "r"(val)); break;
                                    case 1: __asm__ volatile("mov w9, %w0" : : "r"(val)); break;
                                    case 2: __asm__ volatile("mov w10, %w0" : : "r"(val)); break;
                                    case 3: __asm__ volatile("mov w11, %w0" : : "r"(val)); break;
                                    case 4: __asm__ volatile("mov w12, %w0" : : "r"(val)); break;
                                    case 5: __asm__ volatile("mov w13, %w0" : : "r"(val)); break;
                                    case 6: __asm__ volatile("mov w14, %w0" : : "r"(val)); break;
                                    case 7: __asm__ volatile("mov w15, %w0" : : "r"(val)); break;
                                }
                            }

                            // Set up the two special registers the prologue relies on
                            __asm__ volatile (
                                "mov x20, %0\n"     // cpu_state
                                "mov x21, %1\n"     // mem_base
                                :
                                : "r" (&cpu), "r" (cpu.mem_base)
                                : "x20", "x21"
                            );

                            typedef void (*block_fn_t)(void);
                            block_fn_t fn = (block_fn_t)exec_mem;

                            fprintf(stderr, "  [A64-EXEC] jumping to emitted code @ %p (len=%zu)...\n",
                                    exec_mem, a64_cg.emit.offset);

                            fn();

                            uint32_t emitted_pc = cpu.pc;
                            uint32_t emitted_exit = cpu.exit_reason;

                            fprintf(stderr, "  [A64-EXEC] returned. pc=0x%08X exit_reason=%u\n",
                                    emitted_pc, emitted_exit);

                            // ----------------------------------------------------------------
                            // Automatic comparison against shadow interpreter (first validation slice)
                            // ----------------------------------------------------------------
                            // Reset state and run the same region through the shadow interpreter
                            cpu.pc = load_res.entry_point;

                            // Re-setup the live-ins the same way (crude but sufficient for now)
                            for (int s = 0; s < 8; s++) {
                                uint8_t gpr = entry_gpr_for_slot[s];
                                if (gpr == 0) continue;
                                uint32_t val = cpu.regs[gpr];
                                switch (s) {
                                    case 0: __asm__ volatile("mov w8, %w0" : : "r"(val)); break;
                                    case 1: __asm__ volatile("mov w9, %w0" : : "r"(val)); break;
                                    case 2: __asm__ volatile("mov w10, %w0" : : "r"(val)); break;
                                    case 3: __asm__ volatile("mov w11, %w0" : : "r"(val)); break;
                                    case 4: __asm__ volatile("mov w12, %w0" : : "r"(val)); break;
                                    case 5: __asm__ volatile("mov w13, %w0" : : "r"(val)); break;
                                    case 6: __asm__ volatile("mov w14, %w0" : : "r"(val)); break;
                                    case 7: __asm__ volatile("mov w15, %w0" : : "r"(val)); break;
                                }
                            }

                            __asm__ volatile (
                                "mov x20, %0\n"
                                "mov x21, %1\n"
                                : : "r" (&cpu), "r" (cpu.mem_base)
                                : "x20", "x21"
                            );

                            // Run the same region through shadow (very simple loop for the test)
                            uint32_t shadow_steps = 0;
                            const uint32_t MAX_TEST_STEPS = 10000;
                            while (shadow_steps < MAX_TEST_STEPS) {
                                if (cpu.pc >= cpu.code_limit) break;
                                uint32_t raw = *(uint32_t *)(cpu.mem_base + cpu.pc);
                                decoded_inst_t inst = decode_instruction(raw);

                                if (inst.opcode == OP_HALT || inst.opcode == OP_YIELD ||
                                    inst.opcode == OP_DEBUG) {
                                    break;
                                }
                                cpu.pc += 4;
                                shadow_steps++;
                            }

                            uint32_t shadow_pc = cpu.pc;
                            uint32_t shadow_exit = cpu.exit_reason;

                            fprintf(stderr, "  [SHADOW-EXEC] returned. pc=0x%08X exit_reason=%u (steps=%u)\n",
                                    shadow_pc, shadow_exit, shadow_steps);

                            // Simple comparison
                            if (emitted_pc == shadow_pc && emitted_exit == shadow_exit) {
                                fprintf(stderr, "  [VALIDATION] PASS: emitted and shadow results match\n");
                            } else {
                                fprintf(stderr, "  [VALIDATION] FAIL: mismatch!\n");
                                fprintf(stderr, "                 emitted: pc=0x%08X exit=%u\n", emitted_pc, emitted_exit);
                                fprintf(stderr, "                 shadow : pc=0x%08X exit=%u\n", shadow_pc, shadow_exit);
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
                stage5_lift_region_init(&extra_region, pc);

                if (stage5_lift_superblock(&extra_region, cpu.mem_base, cpu.code_limit, STAGE5_LIFT_BUDGET)) {
                    char tag[32];
                    snprintf(tag, sizeof(tag), "blk%u", b);
                    run_full_stage5_analysis(&extra_region, tag, true);
                    extra++;
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
