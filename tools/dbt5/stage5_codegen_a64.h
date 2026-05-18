// SLOW-32 DBT5 — Clean AArch64 Native Codegen
//
// This is the intended home for the *independent* AArch64 Stage 5 backend.
//
// RULES (strict — this is the whole point of the dbt5 fork)
// -----------------------------------------------------------
// - This code may ONLY use raw primitives from emit_a64.h / emit_a64.c.
// - It may NEVER include or call anything from ../dbt/translate*.h or
//   ../dbt/translate*.c (no translate_ctx_t, no reg_alloc, no
//   translate_add / translate_ldw, no old fusion, no old exit machinery).
// - It receives data only from the clean pipeline:
//     - stage5_lift_region_t
//     - stage5_lir_t (after BURG + optimize)
//     - stage5_ra_plan_t (the real register allocation decisions)
// - All exits, side exits, chaining, and prologue/epilogue logic must be
//   implemented here using only the 8 host slots the RA gave us.
//
// CURRENT STATE
// -------------
// This is the very first skeleton.  The context and entry point are defined,
// but the actual emitter is still empty.  The first real owning work will
// live in stage5_codegen_a64.c.
//
// The long-term goal is that on AArch64, `slow32-dbt5` can own regions
// natively using this path, without ever having seen the mature Stage 4
// translator that lives in the production tree.

#ifndef DBT5_STAGE5_CODEGEN_A64_H
#define DBT5_STAGE5_CODEGEN_A64_H

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

#include "stage5_lift.h"
#include "stage5_lir.h"
#include "stage5_ra.h"
#include "emit_a64.h"   // raw emission only

// ============================================================================
// Thin AArch64 codegen context
//
// This is deliberately *not* translate_ctx_t.  It only contains what a
// real Stage 5 emitter on AArch64 needs.
// ============================================================================

#define STAGE5_A64_MAX_HOST_SLOTS  8
#define STAGE5_A64_MAX_EXITS       16

typedef struct {
    // The 8 host registers the RA decided to use for this region.
    // Value 0 in the slot means "not used / reload from memory".
    a64_reg_t host_regs[STAGE5_A64_MAX_HOST_SLOTS];

    // Which guest register (if any) is currently live in each host slot.
    // This is maintained by the emitter during code generation.
    uint8_t   guest_in_slot[STAGE5_A64_MAX_HOST_SLOTS];

    // Whether the value in the slot is dirty relative to guest memory.
    bool      slot_dirty[STAGE5_A64_MAX_HOST_SLOTS];

    // Code emission buffer (we write A64 instructions here)
    emit_ctx_t emit;

    // Exit / side-exit patching information (populated by the emitter)
    uint32_t exit_count;
    struct {
        uint32_t target_pc;
        size_t   patch_offset;   // offset in the emitted code of the branch
        bool     is_side_exit;
    } exits[STAGE5_A64_MAX_EXITS];

    // Bookkeeping
    uint32_t guest_pc;           // current guest PC being emitted
    bool     side_exits_enabled;
} stage5_cg_a64_ctx_t;

// ============================================================================
// Entry point for AArch64 native emission
//
// On success: the region has been emitted into cg->emit using only the
//             RA plan and raw A64 instructions.  Returns true.
// On failure: returns false (caller should not try to use the emitted code).
// ============================================================================

bool stage5_codegen_a64(stage5_cg_a64_ctx_t *cg,
                        const stage5_lift_region_t *region,
                        const stage5_lir_t *lir,
                        const stage5_ra_plan_t *ra_plan);

// Telemetry (stubs for now)
extern uint32_t stage5_codegen_a64_attempted;
extern uint32_t stage5_codegen_a64_success;

#endif // DBT5_STAGE5_CODEGEN_A64_H
