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

// s32x loader lives in the emulator tree (self-contained header)
#include "../emulator/s32x_loader.h"

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
