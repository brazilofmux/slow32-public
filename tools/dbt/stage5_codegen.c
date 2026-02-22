// SLOW-32 DBT: Stage 5 Native Codegen
// Region-level register allocation + direct x86-64 emission
//
// Emits x86-64 for lifted IR prefix instructions (ALU, shifts, compares,
// MUL, MULH, MULHU, DIV, REM, loads, stores) using the pre-assigned host
// registers from ctx->reg_alloc[] (set up by reg_alloc_prescan()).
// Emits native terminals for JAL, HALT, DEBUG, YIELD.
// Delegates FP to translate_fp_r_type().

#include "stage5_codegen.h"
#include "stage5_burg.h"
#include "stage5_ssa.h"
#include "block_cache.h"
#include "cpu_state.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

// ============================================================================
// Telemetry counters
// ============================================================================
uint32_t stage5_codegen_attempted;
uint32_t stage5_codegen_success;
uint32_t stage5_codegen_fallback;
uint32_t stage5_codegen_fallback_unsupported_op;
uint32_t stage5_codegen_fallback_preflight;
uint32_t stage5_codegen_fallback_preflight_fused_branch;
uint32_t stage5_codegen_fallback_preflight_missing_terminal;
uint32_t stage5_codegen_fallback_preflight_bad_terminal_index;
uint32_t stage5_codegen_fallback_preflight_bad_fuse_index;
uint32_t stage5_codegen_fallback_preflight_opcode;
uint32_t stage5_codegen_fallback_preflight_opcode_hist[128];
uint32_t stage5_codegen_fallback_preflight_side_exit;
uint32_t stage5_codegen_fallback_preflight_side_exit_cmpdep_mem;
uint32_t stage5_codegen_fallback_preflight_side_exit_cmpdep_mem_load;
uint32_t stage5_codegen_fallback_preflight_side_exit_cmpdep_mem_store;
uint32_t stage5_codegen_fallback_preflight_side_exit_cmpdep_mem_store_stb;
uint32_t stage5_codegen_fallback_preflight_side_exit_cmpdep_mem_store_stb_beq;
uint32_t stage5_codegen_fallback_preflight_side_exit_cmpdep_mem_store_sth;
uint32_t stage5_codegen_fallback_preflight_side_exit_cmpdep_mem_store_stw;
uint32_t stage5_codegen_fallback_preflight_terminal;
uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix;
uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_opcode_hist[128];
uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_reason_noncanonical_term;
uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_reason_noncanonical_term_opcode_hist[128];
uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_reason_noncanonical_cmp_opcode_hist[128];
uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_reason_nonadjacent;
uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_reason_rd_mismatch;
uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_reason_rd_mismatch_opcode_hist[128];
uint32_t stage5_codegen_branch_cmp_mix_passthrough_rd_mismatch_xor;
uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_reason_predicate_policy;
uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_reason_predicate_policy_pattern_hist[64];
uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_reason_opcode;
uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_reason_multi_cmp;
uint32_t stage5_codegen_fallback_side_exit;
uint32_t stage5_codegen_fallback_emit_node;
uint32_t stage5_codegen_fallback_terminal;
uint32_t stage5_codegen_fallback_terminal_opcode_hist[128];
uint64_t stage5_codegen_guest_insts;
uint64_t stage5_codegen_host_bytes;
uint32_t stage5_codegen_regs_allocated_hist[REG_ALLOC_SLOTS + 1];
uint32_t stage5_codegen_boolpair_native_attempted;
uint32_t stage5_codegen_boolpair_native_success;
uint32_t stage5_codegen_boolpair_native_fallback;
uint32_t stage5_codegen_predicate_native_terminal_success;
uint32_t stage5_codegen_predicate_native_terminal_fallback;
uint32_t stage5_codegen_predicate_native_terminal_opcode_hist[128];
uint64_t stage5_codegen_predicate_native_guest_insts;
uint64_t stage5_codegen_predicate_native_host_bytes;
uint32_t stage5_codegen_fused_addi_mem;
uint32_t stage5_codegen_evictions;
uint64_t stage5_codegen_native_loads;
uint64_t stage5_codegen_native_stores;

static bool cg_env_flag_enabled(const char *name, bool default_enabled) {
    const char *v = getenv(name);
    if (!v || v[0] == '\0') {
        return default_enabled;
    }
    return strcmp(v, "0") != 0;
}

static bool cg_cmp_rr_enabled(void) {
    static bool inited = false;
    static bool enabled = true;
    if (!inited) {
        enabled = cg_env_flag_enabled("SLOW32_DBT_STAGE5_CODEGEN_CMP_RR", true);
        inited = true;
    }
    return enabled;
}

static bool cg_cmp_ri_enabled(void) {
    static bool inited = false;
    static bool enabled = true;
    if (!inited) {
        enabled = cg_env_flag_enabled("SLOW32_DBT_STAGE5_CODEGEN_CMP_RI", true);
        inited = true;
    }
    return enabled;
}

static bool cg_fused_branch_enabled(void) {
    static bool inited = false;
    static bool enabled = true;
    if (!inited) {
        enabled = cg_env_flag_enabled("SLOW32_DBT_STAGE5_CODEGEN_FUSED_BRANCH", true);
        inited = true;
    }
    return enabled;
}

static bool cg_branch_term_enabled(void) {
    static bool inited = false;
    static bool enabled = true;
    if (!inited) {
        enabled = cg_env_flag_enabled("SLOW32_DBT_STAGE5_CODEGEN_BRANCH_TERM", true);
        inited = true;
    }
    return enabled;
}

static bool cg_branch_cmp_mix_enabled(void) {
    static bool inited = false;
    static bool enabled = true;
    if (!inited) {
        enabled = cg_env_flag_enabled("SLOW32_DBT_STAGE5_CODEGEN_BRANCH_CMP_MIX", true);
        inited = true;
    }
    return enabled;
}

static bool cg_branch_cmp_mix_slt_enabled(void) {
    static bool inited = false;
    static bool enabled = true;
    if (!inited) {
        enabled = cg_env_flag_enabled("SLOW32_DBT_STAGE5_CODEGEN_BRANCH_CMP_MIX_SLT", true);
        inited = true;
    }
    return enabled;
}

static bool cg_branch_cmp_mix_slti_enabled(void) {
    static bool inited = false;
    static bool enabled = true;
    if (!inited) {
        enabled = cg_env_flag_enabled("SLOW32_DBT_STAGE5_CODEGEN_BRANCH_CMP_MIX_SLTI", true);
        inited = true;
    }
    return enabled;
}

static bool cg_branch_cmp_mix_sltiu_enabled(void) {
    static bool inited = false;
    static bool enabled = true;
    if (!inited) {
        enabled = cg_env_flag_enabled("SLOW32_DBT_STAGE5_CODEGEN_BRANCH_CMP_MIX_SLTIU", true);
        inited = true;
    }
    return enabled;
}

static bool cg_branch_cmp_mix_unsigned_rr_enabled(void) {
    static bool inited = false;
    static bool enabled = true;
    if (!inited) {
        enabled = cg_env_flag_enabled("SLOW32_DBT_STAGE5_CODEGEN_BRANCH_CMP_MIX_UNSIGNED_RR", true);
        inited = true;
    }
    return enabled;
}

static bool cg_side_exit_enabled(void) {
    static bool inited = false;
    static bool enabled = false;
    if (!inited) {
        const char *v = getenv("SLOW32_DBT_STAGE5_CODEGEN_SIDE_EXIT");
        enabled = (v && v[0] != '\0' && strcmp(v, "0") != 0);
        inited = true;
    }
    return enabled;
}

static bool cg_boolpair_native_enabled(void) {
    static bool inited = false;
    static bool enabled = true;
    if (!inited) {
        enabled = cg_env_flag_enabled("SLOW32_DBT_STAGE5_CODEGEN_BOOLPAIR_NATIVE", true);
        inited = true;
    }
    return enabled;
}

static bool cg_predicate_native_enabled(void) {
    static bool inited = false;
    static bool enabled = true;
    if (!inited) {
        enabled = cg_env_flag_enabled("SLOW32_DBT_STAGE5_CODEGEN_PREDICATE_NATIVE", true);
        inited = true;
    }
    return enabled;
}

static bool cg_allow_cmpdep_side_exit(void) {
    static bool inited = false;
    static bool enabled = true;
    if (!inited) {
        enabled = cg_env_flag_enabled("SLOW32_DBT_STAGE5_CODEGEN_ALLOW_CMPDEP_SIDE_EXIT", true);
        inited = true;
    }
    return enabled;
}

static bool cg_allow_cmpdep_side_exit_loads(void) {
    static bool inited = false;
    static bool enabled = false;
    if (!inited) {
        const char *v = getenv("SLOW32_DBT_STAGE5_CODEGEN_ALLOW_CMPDEP_SIDE_EXIT_LOADS");
        // Default-on once cmpdep side-exit ownership is enabled: loads are
        // currently parity-clean, stores are still guarded separately.
        if (!v || v[0] == '\0') {
            enabled = true;
        } else {
            enabled = (strcmp(v, "0") != 0);
        }
        inited = true;
    }
    return enabled;
}

static bool cg_allow_cmpdep_side_exit_stores(void) {
    static bool inited = false;
    static bool has_override = false;
    static bool enabled = false;
    if (!inited) {
        const char *v = getenv("SLOW32_DBT_STAGE5_CODEGEN_ALLOW_CMPDEP_SIDE_EXIT_STORES");
        has_override = (v && v[0] != '\0');
        enabled = (has_override && strcmp(v, "0") != 0);
        inited = true;
    }
    return enabled;
}

static bool cg_allow_cmpdep_side_exit_stores_has_override(void) {
    static bool inited = false;
    static bool has_override = false;
    if (!inited) {
        const char *v = getenv("SLOW32_DBT_STAGE5_CODEGEN_ALLOW_CMPDEP_SIDE_EXIT_STORES");
        has_override = (v && v[0] != '\0');
        inited = true;
    }
    return has_override;
}

static bool cg_allow_cmpdep_side_exit_store_stb(void) {
    static bool inited = false;
    static bool enabled = false;
    if (!inited) {
        const char *v = getenv("SLOW32_DBT_STAGE5_CODEGEN_ALLOW_CMPDEP_SIDE_EXIT_STORE_STB");
        if (!v || v[0] == '\0') {
            enabled = true;
        } else {
            enabled = (strcmp(v, "0") != 0);
        }
        inited = true;
    }
    return enabled;
}

static bool cg_allow_cmpdep_side_exit_store_sth(void) {
    static bool inited = false;
    static bool enabled = false;
    if (!inited) {
        const char *v = getenv("SLOW32_DBT_STAGE5_CODEGEN_ALLOW_CMPDEP_SIDE_EXIT_STORE_STH");
        if (!v || v[0] == '\0') {
            enabled = true;
        } else {
            enabled = (strcmp(v, "0") != 0);
        }
        inited = true;
    }
    return enabled;
}

static bool cg_allow_cmpdep_side_exit_store_stw(void) {
    static bool inited = false;
    static bool enabled = false;
    if (!inited) {
        const char *v = getenv("SLOW32_DBT_STAGE5_CODEGEN_ALLOW_CMPDEP_SIDE_EXIT_STORE_STW");
        if (!v || v[0] == '\0') {
            enabled = true;
        } else {
            enabled = (strcmp(v, "0") != 0);
        }
        inited = true;
    }
    return enabled;
}

static bool cg_boolpair_trace_enabled(void) {
    static bool inited = false;
    static bool enabled = false;
    if (!inited) {
        const char *v = getenv("SLOW32_DBT_STAGE5_CODEGEN_BOOLPAIR_TRACE");
        enabled = (v && v[0] != '\0' && strcmp(v, "0") != 0);
        inited = true;
    }
    return enabled;
}

static bool cg_boolpair_skip_pc_enabled(uint32_t guest_pc) {
    static bool inited = false;
    static bool has_pc = false;
    static uint32_t skip_pc = 0;
    if (!inited) {
        const char *v = getenv("SLOW32_DBT_STAGE5_CODEGEN_BOOLPAIR_SKIP_PC");
        if (v && v[0] != '\0') {
            char *end = NULL;
            unsigned long p = strtoul(v, &end, 0);
            if (end != v && *end == '\0') {
                has_pc = true;
                skip_pc = (uint32_t)p;
            }
        }
        inited = true;
    }
    return has_pc && guest_pc == skip_pc;
}

static bool cg_codegen_trace_enabled(void) {
    static bool inited = false;
    static bool enabled = false;
    if (!inited) {
        const char *v = getenv("SLOW32_DBT_STAGE5_CODEGEN_TRACE");
        enabled = (v && v[0] != '\0' && strcmp(v, "0") != 0);
        inited = true;
    }
    return enabled;
}

static bool cg_regalloc_trace_enabled(void) {
    static bool inited = false;
    static bool enabled = false;
    if (!inited) {
        const char *v = getenv("SLOW32_DBT_STAGE5_CODEGEN_TRACE_REGALLOC");
        enabled = (v && v[0] != '\0' && strcmp(v, "0") != 0);
        inited = true;
    }
    return enabled;
}

static bool cg_regalloc_trace_pc_match(uint32_t guest_pc) {
    static bool inited = false;
    static bool has_pc = false;
    static uint32_t trace_pc = 0;
    if (!inited) {
        const char *v = getenv("SLOW32_DBT_STAGE5_CODEGEN_TRACE_REGALLOC_PC");
        if (v && v[0] != '\0') {
            trace_pc = (uint32_t)strtoul(v, NULL, 0);
            has_pc = true;
        }
        inited = true;
    }
    return !has_pc || guest_pc == trace_pc;
}

static void cg_trace_regalloc(translate_ctx_t *ctx, uint32_t guest_pc) {
    if (!cg_regalloc_trace_enabled()) return;
    if (!cg_regalloc_trace_pc_match(guest_pc)) return;
    fprintf(stderr, "[stage5-regalloc] pc=0x%08X slots:", guest_pc);
    for (int i = 0; i < REG_ALLOC_SLOTS; i++) {
        if (!ctx->reg_alloc[i].allocated) continue;
        fprintf(stderr, " s%d=g%u(d=%u)", i, ctx->reg_alloc[i].guest_reg,
                ctx->reg_alloc[i].dirty ? 1u : 0u);
    }
    fprintf(stderr, " map:");
    for (int g = 1; g < 32; g++) {
        int8_t s = ctx->reg_alloc_map[g];
        if (s >= 0) fprintf(stderr, " g%u->s%d", g, s);
    }
    fprintf(stderr, "\n");
}

static bool cg_codegen_skip_pc_enabled(uint32_t guest_pc) {
    static bool inited = false;
    static char spec[256];
    static bool has_spec = false;
    if (!inited) {
        const char *v = getenv("SLOW32_DBT_STAGE5_CODEGEN_SKIP_PC");
        if (v && v[0] != '\0') {
            size_t n = strlen(v);
            if (n >= sizeof(spec)) n = sizeof(spec) - 1;
            memcpy(spec, v, n);
            spec[n] = '\0';
            has_spec = true;
        }
        inited = true;
    }
    if (!has_spec) return false;
    const char *p = spec;
    while (*p) {
        while (*p == ' ' || *p == '\t' || *p == ',') p++;
        if (!*p) break;
        char *end = NULL;
        unsigned long x = strtoul(p, &end, 0);
        if (end != p) {
            if ((uint32_t)x == guest_pc) return true;
            p = end;
            continue;
        }
        while (*p && *p != ',') p++;
    }
    return false;
}

static void cg_trace_codegen_region(uint32_t guest_pc,
                                    int emitted_pattern,
                                    const stage5_lift_region_t *region) {
    if (!cg_codegen_trace_enabled()) return;
    fprintf(stderr, "[stage5-codegen] pc=0x%08X pattern=%s ginst=%u ir=%u side_exits=%u\n",
            guest_pc, stage5_burg_pattern_str((stage5_burg_pattern_t)emitted_pattern),
            region ? region->guest_inst_count : 0,
            region ? region->ir_count : 0,
            region ? region->side_exit_count : 0);
}

static void cg_trace_boolpair_region(const stage5_lift_region_t *region,
                                     uint32_t guest_pc,
                                     int terminal_idx) {
    if (!cg_boolpair_trace_enabled() || !region) return;
    fprintf(stderr, "[stage5-boolpair] pc=0x%08X ginst=%u ir=%u term=%d\n",
            guest_pc, region->guest_inst_count, region->ir_count, terminal_idx);
    int lo = terminal_idx - 6;
    if (lo < 0) lo = 0;
    int hi = terminal_idx;
    if (hi >= (int)region->ir_count) hi = (int)region->ir_count - 1;
    for (int i = lo; i <= hi; i++) {
        const stage5_ir_node_t *n = &region->ir[i];
        fprintf(stderr,
                "[stage5-boolpair]   ir[%d] op=0x%02X rd=r%u rs1=r%u rs2=r%u imm=%d syn=%d kind=%d%s\n",
                i, n->opcode, n->rd, n->rs1, n->rs2, n->imm,
                n->synthetic ? 1 : 0, n->kind, (i == terminal_idx) ? " <term>" : "");
    }
}

static bool cg_trace_mix_enabled(void) {
    static bool inited = false;
    static bool enabled = false;
    if (!inited) {
        const char *v = getenv("SLOW32_DBT_STAGE5_CODEGEN_TRACE_MIX");
        enabled = (v && v[0] != '\0' && strcmp(v, "0") != 0);
        inited = true;
    }
    return enabled;
}

static bool cg_predicate_branch_only_enabled(void) {
    static bool inited = false;
    static bool enabled = false;
    if (!inited) {
        const char *v = getenv("SLOW32_DBT_STAGE5_CODEGEN_PREDICATE_BRANCH_ONLY");
        enabled = (v && v[0] != '\0' && strcmp(v, "0") != 0);
        inited = true;
    }
    return enabled;
}

static bool cg_pattern_is_predicate_branch(int emitted_pattern) {
    return emitted_pattern == STAGE5_BURG_PATTERN_CMP_BRANCH_ZERO ||
           emitted_pattern == STAGE5_BURG_PATTERN_CMP_BRANCH_CONST01 ||
           emitted_pattern == STAGE5_BURG_PATTERN_CMP_BRANCH_XOR1 ||
           emitted_pattern == STAGE5_BURG_PATTERN_CMP_BRANCH_BOOLPAIR ||
           emitted_pattern == STAGE5_BURG_PATTERN_CMP_BRANCH_CMPDEP ||
           emitted_pattern == STAGE5_BURG_PATTERN_CMP_BRANCH_NOCMPDEP;
}

static bool cg_pattern_uses_cmp_mix_fusion(int emitted_pattern) {
    return emitted_pattern == STAGE5_BURG_PATTERN_CMP_BRANCH_ZERO ||
           emitted_pattern == STAGE5_BURG_PATTERN_CMP_BRANCH_CONST01 ||
           emitted_pattern == STAGE5_BURG_PATTERN_CMP_BRANCH_XOR1;
}

static bool cg_opcode_is_mem(uint8_t op) {
    return op == OP_LDB || op == OP_LDH || op == OP_LDW ||
           op == OP_LDBU || op == OP_LDHU ||
           op == OP_STB || op == OP_STH || op == OP_STW;
}

static void cg_region_mem_prefix_flags(const stage5_lift_region_t *region,
                                       int terminal_idx,
                                       bool *has_load_out,
                                       bool *has_store_out,
                                       bool *has_store_stb_out,
                                       bool *has_store_sth_out,
                                       bool *has_store_stw_out) {
    bool has_load = false;
    bool has_store = false;
    bool has_store_stb = false;
    bool has_store_sth = false;
    bool has_store_stw = false;
    if (!region) {
        if (has_load_out) *has_load_out = false;
        if (has_store_out) *has_store_out = false;
        if (has_store_stb_out) *has_store_stb_out = false;
        if (has_store_sth_out) *has_store_sth_out = false;
        if (has_store_stw_out) *has_store_stw_out = false;
        return;
    }
    for (uint32_t i = 0; i < region->ir_count; i++) {
        if (terminal_idx >= 0 && (int)i == terminal_idx) continue;
        const stage5_ir_node_t *n = &region->ir[i];
        if (n->synthetic) continue;
        if (!cg_opcode_is_mem(n->opcode)) continue;
        if (n->opcode == OP_LDB || n->opcode == OP_LDH || n->opcode == OP_LDW ||
            n->opcode == OP_LDBU || n->opcode == OP_LDHU) {
            has_load = true;
        }
        if (n->opcode == OP_STB || n->opcode == OP_STH || n->opcode == OP_STW) {
            has_store = true;
            if (n->opcode == OP_STB) has_store_stb = true;
            if (n->opcode == OP_STH) has_store_sth = true;
            if (n->opcode == OP_STW) has_store_stw = true;
        }
        if (has_load && has_store_stb && has_store_sth && has_store_stw) break;
    }
    if (has_load_out) *has_load_out = has_load;
    if (has_store_out) *has_store_out = has_store;
    if (has_store_stb_out) *has_store_stb_out = has_store_stb;
    if (has_store_sth_out) *has_store_sth_out = has_store_sth;
    if (has_store_stw_out) *has_store_stw_out = has_store_stw;
}

// ============================================================================
// Forward declarations for translate.c functions we need
// ============================================================================

// These are defined in translate.c and we need access to them.
// Since they're static, we need them re-declared or passed via ctx.
// We use the public API where possible and replicate small helpers.

// Host register array (must match translate.c)
static const x64_reg_t cg_reg_alloc_hosts[REG_ALLOC_SLOTS] = {
    RBX, R12, R13, RSI, RDI, R11, R8, R9
};

// ============================================================================
// Codegen context (stack-allocated, per-region)
// ============================================================================

typedef struct {
    translate_ctx_t *ctx;
    emit_ctx_t *e;
    const stage5_lift_region_t *region;
    uint32_t emitted_insts;        // Number of guest instructions emitted
    int current_idx;               // Current instruction index in region->ir
} stage5_cg_t;

typedef struct {
    size_t emit_offset;
    bool emit_overflow;
    bool emit_rax_pending;
    uint32_t guest_pc;
    int current_inst_idx;
    int exit_idx;
    int side_exit_emitted;
    int deferred_exit_count;
    int pc_map_count;
    int validated_range_count;

    struct {
        uint8_t guest_reg;
        bool allocated;
        bool dirty;
    } reg_alloc[REG_ALLOC_SLOTS];
    int8_t reg_alloc_map[32];

    struct {
        bool valid;
        uint8_t guest_reg;
        host_reg_t host_reg;
        bool can_skip_store;
    } pending_write;

    struct {
        bool valid;
        uint8_t opcode;
        uint8_t rd;
        uint8_t rs1, rs2;
        bool rs2_is_imm;
        int32_t imm;
        int inst_idx;
    } pending_cond;

    struct {
        bool valid;
        uint32_t value;
    } reg_constants[32];

    struct {
        uint8_t guest_reg;
        uint32_t lo_offset;
        uint32_t hi_end;
        bool is_store_ok;
    } validated_ranges[MAX_VALIDATED_RANGES];

    struct {
        size_t jmp_patch_offset;
        uint32_t target_pc;
        int exit_idx;
        uint32_t branch_pc;
        bool dirty_snapshot[REG_ALLOC_SLOTS];
        bool allocated_snapshot[REG_ALLOC_SLOTS];
        uint8_t guest_reg_snapshot[REG_ALLOC_SLOTS];
        bool pending_write_valid;
        uint8_t pending_write_guest_reg;
        host_reg_t pending_write_host_reg;
        bool force_full_flush;
    } deferred_exits[MAX_BLOCK_EXITS];
} stage5_codegen_state_t;

static void cg_state_save(stage5_codegen_state_t *s, const translate_ctx_t *ctx) {
    s->emit_offset = ctx->emit.offset;
    s->emit_overflow = ctx->emit.overflow;
    s->emit_rax_pending = ctx->emit.rax_pending;
    s->guest_pc = ctx->guest_pc;
    s->current_inst_idx = ctx->current_inst_idx;
    s->exit_idx = ctx->exit_idx;
    s->side_exit_emitted = ctx->side_exit_emitted;
    s->deferred_exit_count = ctx->deferred_exit_count;
    s->pc_map_count = ctx->pc_map_count;
    s->validated_range_count = ctx->validated_range_count;
    memcpy(s->reg_alloc, ctx->reg_alloc, sizeof(s->reg_alloc));
    memcpy(s->reg_alloc_map, ctx->reg_alloc_map, sizeof(s->reg_alloc_map));
    memcpy(&s->pending_write, &ctx->pending_write, sizeof(s->pending_write));
    memcpy(&s->pending_cond, &ctx->pending_cond, sizeof(s->pending_cond));
    memcpy(s->reg_constants, ctx->reg_constants, sizeof(s->reg_constants));
    memcpy(s->validated_ranges, ctx->validated_ranges, sizeof(s->validated_ranges));
    memcpy(s->deferred_exits, ctx->deferred_exits, sizeof(s->deferred_exits));
}

static void cg_state_restore(translate_ctx_t *ctx, const stage5_codegen_state_t *s) {
    ctx->emit.offset = s->emit_offset;
    ctx->emit.overflow = s->emit_overflow;
    ctx->emit.rax_pending = s->emit_rax_pending;
    ctx->guest_pc = s->guest_pc;
    ctx->current_inst_idx = s->current_inst_idx;
    ctx->exit_idx = s->exit_idx;
    ctx->side_exit_emitted = s->side_exit_emitted;
    ctx->deferred_exit_count = s->deferred_exit_count;
    ctx->pc_map_count = s->pc_map_count;
    ctx->validated_range_count = s->validated_range_count;
    memcpy(ctx->reg_alloc, s->reg_alloc, sizeof(s->reg_alloc));
    memcpy(ctx->reg_alloc_map, s->reg_alloc_map, sizeof(s->reg_alloc_map));
    memcpy(&ctx->pending_write, &s->pending_write, sizeof(s->pending_write));
    memcpy(&ctx->pending_cond, &s->pending_cond, sizeof(s->pending_cond));
    memcpy(ctx->reg_constants, s->reg_constants, sizeof(s->reg_constants));
    memcpy(ctx->validated_ranges, s->validated_ranges, sizeof(s->validated_ranges));
    memcpy(ctx->deferred_exits, s->deferred_exits, sizeof(s->deferred_exits));
}

// ============================================================================
// Register resolution helpers
// ============================================================================

static bool cg_is_reg_live(const stage5_cg_t *cg, uint8_t guest_reg) {
    if (guest_reg == 0 || guest_reg >= 32) return false;
    if (!cg->region->reg_flow_valid) return true; // Conservative
    const stage5_reg_flow_t *f = &cg->region->reg_flow[guest_reg];
    if (f->live_across_edge) return true;
    return (int)f->last_use >= cg->current_idx;
}

static x64_reg_t cg_alloc_reg(stage5_cg_t *cg, uint8_t guest_reg, bool is_write,
                              uint32_t protect_mask) {
    if (guest_reg == 0 || guest_reg >= 32) return X64_NOREG;
    translate_ctx_t *ctx = cg->ctx;

    // 1. Already mapped?
    int8_t slot = ctx->reg_alloc_map[guest_reg];
    if (slot >= 0) return cg_reg_alloc_hosts[slot];

    // 2. Any free slot?
    for (int i = 0; i < REG_ALLOC_SLOTS; i++) {
        if (!ctx->reg_alloc[i].allocated) {
            ctx->reg_alloc[i].guest_reg = guest_reg;
            ctx->reg_alloc[i].allocated = true;
            ctx->reg_alloc[i].dirty = false;
            ctx->reg_alloc_map[guest_reg] = (int8_t)i;
            if (!is_write) {
                emit_mov_r32_m32(cg->e, cg_reg_alloc_hosts[i], RBP, GUEST_REG_OFFSET(guest_reg));
            }
            return cg_reg_alloc_hosts[i];
        }
    }

    // 3. Eviction
    int best_slot = -1;
    int min_last_use = 999999;
    for (int i = 0; i < REG_ALLOC_SLOTS; i++) {
        uint8_t g = ctx->reg_alloc[i].guest_reg;
        if (protect_mask & (1u << g)) continue;

        if (!cg_is_reg_live(cg, g)) {
            best_slot = i;
            break;
        }

        int last = (int)cg->region->reg_flow[g].last_use;
        if (last < min_last_use) {
            min_last_use = last;
            best_slot = i;
        }
    }

    if (best_slot >= 0) {
        uint8_t old_g = ctx->reg_alloc[best_slot].guest_reg;
        if (ctx->reg_alloc[best_slot].dirty) {
            emit_mov_m32_r32(cg->e, RBP, GUEST_REG_OFFSET(old_g), cg_reg_alloc_hosts[best_slot]);
        }
        ctx->reg_alloc_map[old_g] = -1;
        stage5_codegen_evictions++;

        ctx->reg_alloc[best_slot].guest_reg = guest_reg;
        ctx->reg_alloc[best_slot].allocated = true;
        ctx->reg_alloc[best_slot].dirty = false;
        ctx->reg_alloc_map[guest_reg] = (int8_t)best_slot;
        if (!is_write) {
            emit_mov_r32_m32(cg->e, cg_reg_alloc_hosts[best_slot], RBP, GUEST_REG_OFFSET(guest_reg));
        }
        return cg_reg_alloc_hosts[best_slot];
    }

    return X64_NOREG;
}

// Return the host register for a guest register, or X64_NOREG if not cached.
static inline x64_reg_t cg_guest_host(const stage5_cg_t *cg, uint8_t guest_reg) {
    if (guest_reg == 0) return X64_NOREG;
    if (!cg->ctx->reg_cache_enabled) return X64_NOREG;
    int8_t slot = cg->ctx->reg_alloc_map[guest_reg];
    return (slot >= 0) ? cg_reg_alloc_hosts[slot] : X64_NOREG;
}

// Resolve a source operand. If cached, returns the host reg.
// If not cached, loads from memory into 'scratch' and returns scratch.
// Guest r0 loads zero into scratch.
static inline x64_reg_t cg_resolve_src(stage5_cg_t *cg, uint8_t guest_reg,
                                        x64_reg_t scratch, uint32_t protect_mask) {
    if (guest_reg == 0) {
        emit_xor_r32_r32(cg->e, scratch, scratch);
        return scratch;
    }
    x64_reg_t h = cg_alloc_reg(cg, guest_reg, false, protect_mask);
    if (h != X64_NOREG) return h;
    // Load from memory
    emit_mov_r32_m32(cg->e, scratch, RBP, GUEST_REG_OFFSET(guest_reg));
    return scratch;
}

// Resolve destination. If cached, returns the host reg.
// If not cached (spilled), returns RAX (caller must store back).
static inline x64_reg_t cg_resolve_dst(stage5_cg_t *cg, uint8_t guest_reg,
                                        uint32_t protect_mask) {
    if (guest_reg == 0) return RAX; // writes to r0 are discarded
    x64_reg_t h = cg_alloc_reg(cg, guest_reg, true, protect_mask);
    return (h != X64_NOREG) ? h : RAX;
}

// Mark a destination register as dirty after writing.
// Must mirror reg_cache_mark_written() in translate.c to keep ctx consistent.
static inline void cg_mark_dirty(stage5_cg_t *cg, uint8_t guest_reg) {
    if (guest_reg == 0) return;
    translate_ctx_t *ctx = cg->ctx;

    // Invalidate constant propagation
    ctx->reg_constants[guest_reg].valid = false;

    // Invalidate bounds check elimination ranges that use this register as base
    for (int i = 0; i < ctx->validated_range_count; i++) {
        if (ctx->validated_ranges[i].guest_reg == guest_reg) {
            ctx->validated_ranges[i].guest_reg = 0;
        }
    }

    // Discard pending write for the same guest register (superseded by our write)
    if (ctx->pending_write.valid && ctx->pending_write.guest_reg == guest_reg) {
        ctx->pending_write.valid = false;
        ctx->emit.rax_pending = false;
    }

    // Mark the cached slot as dirty
    int8_t slot = ctx->reg_alloc_map[guest_reg];
    if (slot >= 0) {
        ctx->reg_alloc[slot].dirty = true;
    }
}

// Store spilled destination back to memory (when rd is not cached).
static inline void cg_store_spilled(stage5_cg_t *cg, uint8_t guest_reg,
                                     x64_reg_t host_reg) {
    if (guest_reg == 0) return;
    if (cg_guest_host(cg, guest_reg) != X64_NOREG) return; // cached, dirty handles it
    emit_mov_m32_r32(cg->e, RBP, GUEST_REG_OFFSET(guest_reg), host_reg);
}

// ============================================================================
// (Load/Store emission moved above — native x86-64, no Stage 4 delegation)
// ============================================================================

// ============================================================================
// Individual instruction emitters
// ============================================================================

// ALU register-register: ADD, SUB, AND, OR, XOR
static bool cg_emit_alu_rr(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    uint8_t rd = n->rd, rs1 = n->rs1, rs2 = n->rs2;
    if (rd == 0) return true; // discard

    uint32_t protect = (1u << rs1) | (1u << rs2);
    emit_ctx_t *e = cg->e;
    x64_reg_t dst_h = cg_resolve_dst(cg, rd, protect);
    bool commutative = (n->opcode != OP_SUB);

    // Choose the emit function
    void (*emit_op)(emit_ctx_t *, x64_reg_t, x64_reg_t) = NULL;
    switch (n->opcode) {
        case OP_ADD: emit_op = emit_add_r32_r32; break;
        case OP_SUB: emit_op = emit_sub_r32_r32; break;
        case OP_AND: emit_op = emit_and_r32_r32; break;
        case OP_OR:  emit_op = emit_or_r32_r32; break;
        case OP_XOR: emit_op = emit_xor_r32_r32; break;
        default: return false;
    }

    x64_reg_t h_rd = cg_guest_host(cg, rd);
    x64_reg_t h_rs1 = cg_guest_host(cg, rs1);
    x64_reg_t h_rs2 = cg_guest_host(cg, rs2);

    if (h_rd != X64_NOREG && rd == rs1) {
        // rd == rs1 and both cached: OP dst, src2
        x64_reg_t src2_h = cg_resolve_src(cg, rs2, RCX, protect);
        emit_op(e, dst_h, src2_h);
    } else if (!commutative && h_rd != X64_NOREG && rd == rs2) {
        // Non-commutative hazard (e.g., SUB): rd == rs2.
        // Preserve old rd/rs2 before loading rs1 into dst.
        x64_reg_t src1_h = cg_resolve_src(cg, rs1, RAX, protect);
        x64_reg_t tmp_h = (src1_h == RCX) ? RAX : RCX;
        emit_mov_r32_r32(e, tmp_h, dst_h);
        if (dst_h != src1_h)
            emit_mov_r32_r32(e, dst_h, src1_h);
        emit_op(e, dst_h, tmp_h);
    } else if (h_rd != X64_NOREG && rd == rs2 && commutative) {
        // rd == rs2 commutative: OP dst, src1
        x64_reg_t src1_h = cg_resolve_src(cg, rs1, RCX, protect);
        emit_op(e, dst_h, src1_h);
    } else {
        // General case: mov dst, src1; OP dst, src2
        x64_reg_t src1_h = cg_resolve_src(cg, rs1, RAX, protect);
        if (dst_h != src1_h)
            emit_mov_r32_r32(e, dst_h, src1_h);
        x64_reg_t src2_h = cg_resolve_src(cg, rs2, RCX, protect);
        emit_op(e, dst_h, src2_h);
    }

    cg_mark_dirty(cg, rd);
    cg_store_spilled(cg, rd, dst_h);
    return true;
}

// ALU register-immediate: ADDI, ANDI, ORI, XORI
static bool cg_emit_alu_ri(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    uint8_t rd = n->rd, rs1 = n->rs1;
    int32_t imm = n->imm;
    if (rd == 0) return true;

    uint32_t protect = (1u << rs1);
    emit_ctx_t *e = cg->e;
    x64_reg_t dst_h = cg_resolve_dst(cg, rd, protect);

    // For ADDI with imm=0, it's a move
    if (n->opcode == OP_ADDI && imm == 0) {
        if (rs1 == 0) {
            emit_xor_r32_r32(e, dst_h, dst_h);
        } else {
            x64_reg_t src1_h = cg_resolve_src(cg, rs1, RAX, protect);
            if (dst_h != src1_h)
                emit_mov_r32_r32(e, dst_h, src1_h);
        }
        cg_mark_dirty(cg, rd);
        cg_store_spilled(cg, rd, dst_h);
        return true;
    }

    // Choose emit function
    void (*emit_op)(emit_ctx_t *, x64_reg_t, int32_t) = NULL;
    switch (n->opcode) {
        case OP_ADDI: emit_op = emit_add_r32_imm32; break;
        case OP_ANDI: emit_op = emit_and_r32_imm32; break;
        case OP_ORI:  emit_op = emit_or_r32_imm32; break;
        case OP_XORI: emit_op = emit_xor_r32_imm32; break;
        default: return false;
    }

    x64_reg_t h_rd = cg_guest_host(cg, rd);

    if (h_rd != X64_NOREG && rd == rs1) {
        // In-place: OP dst, imm
        emit_op(e, dst_h, imm);
    } else {
        x64_reg_t src1_h = cg_resolve_src(cg, rs1, RAX, protect);
        if (dst_h != src1_h)
            emit_mov_r32_r32(e, dst_h, src1_h);
        emit_op(e, dst_h, imm);
    }

    cg_mark_dirty(cg, rd);
    cg_store_spilled(cg, rd, dst_h);
    return true;
}

// Shift immediate: SLLI, SRLI, SRAI
static bool cg_emit_shift_imm(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    uint8_t rd = n->rd, rs1 = n->rs1;
    uint8_t shamt = (uint8_t)(n->imm & 31);
    if (rd == 0) return true;

    uint32_t protect = (1u << rs1);
    emit_ctx_t *e = cg->e;
    x64_reg_t dst_h = cg_resolve_dst(cg, rd, protect);

    void (*emit_sh)(emit_ctx_t *, x64_reg_t, uint8_t) = NULL;
    switch (n->opcode) {
        case OP_SLLI: emit_sh = emit_shl_r32_imm8; break;
        case OP_SRLI: emit_sh = emit_shr_r32_imm8; break;
        case OP_SRAI: emit_sh = emit_sar_r32_imm8; break;
        default: return false;
    }

    x64_reg_t src1_h = cg_resolve_src(cg, rs1, RAX, protect);
    if (dst_h != src1_h)
        emit_mov_r32_r32(e, dst_h, src1_h);
    emit_sh(e, dst_h, shamt);

    cg_mark_dirty(cg, rd);
    cg_store_spilled(cg, rd, dst_h);
    return true;
}

// Shift register: SLL, SRL, SRA
// Requires shift amount in CL. RCX is scratch (never cached).
static bool cg_emit_shift_reg(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    uint8_t rd = n->rd, rs1 = n->rs1, rs2 = n->rs2;
    if (rd == 0) return true;

    uint32_t protect = (1u << rs1) | (1u << rs2);
    emit_ctx_t *e = cg->e;
    x64_reg_t dst_h = cg_resolve_dst(cg, rd, protect);

    void (*emit_sh)(emit_ctx_t *, x64_reg_t) = NULL;
    switch (n->opcode) {
        case OP_SLL: emit_sh = emit_shl_r32_cl; break;
        case OP_SRL: emit_sh = emit_shr_r32_cl; break;
        case OP_SRA: emit_sh = emit_sar_r32_cl; break;
        default: return false;
    }

    // Load shift amount into RCX
    x64_reg_t rs2_h = cg_guest_host(cg, rs2);
    if (rs2_h != X64_NOREG) {
        emit_mov_r32_r32(e, RCX, rs2_h);
    } else if (rs2 == 0) {
        emit_xor_r32_r32(e, RCX, RCX);
    } else {
        emit_mov_r32_m32(e, RCX, RBP, GUEST_REG_OFFSET(rs2));
    }

    // Prepare dst = src1
    x64_reg_t src1_h = cg_resolve_src(cg, rs1, RAX, protect);
    if (dst_h == RCX) {
        // dst is RCX (spilled rd maps to RAX, but let's be safe)
        // This shouldn't happen since rd==0 returns early and RCX isn't cached
        dst_h = RAX;
    }
    if (dst_h != src1_h)
        emit_mov_r32_r32(e, dst_h, src1_h);

    emit_sh(e, dst_h);

    cg_mark_dirty(cg, rd);
    cg_store_spilled(cg, rd, dst_h);
    return true;
}

// LUI: load upper immediate
static bool cg_emit_lui(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    uint8_t rd = n->rd;
    if (rd == 0) return true;

    emit_ctx_t *e = cg->e;
    x64_reg_t dst_h = cg_resolve_dst(cg, rd, 0);
    // n->imm for LUI is already the shifted value (raw & 0xFFFFF000)
    emit_mov_r32_imm32(e, dst_h, (uint32_t)n->imm);

    cg_mark_dirty(cg, rd);
    cg_store_spilled(cg, rd, dst_h);
    return true;
}

// MUL: multiply (low 32 bits)
static bool cg_emit_mul(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    uint8_t rd = n->rd, rs1 = n->rs1, rs2 = n->rs2;
    if (rd == 0) return true;

    uint32_t protect = (1u << rs1) | (1u << rs2);
    emit_ctx_t *e = cg->e;
    x64_reg_t dst_h = cg_resolve_dst(cg, rd, protect);

    x64_reg_t src1_h = cg_resolve_src(cg, rs1, RAX, protect);
    if (dst_h != src1_h)
        emit_mov_r32_r32(e, dst_h, src1_h);
    x64_reg_t src2_h = cg_resolve_src(cg, rs2, RCX, protect);
    emit_imul_r32_r32(e, dst_h, src2_h);

    cg_mark_dirty(cg, rd);
    cg_store_spilled(cg, rd, dst_h);
    return true;
}

// Comparisons: SLT, SLTU, SEQ, SNE, SGT, SGTU, SLE, SLEU, SGE, SGEU
static bool cg_emit_cmp_rr(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    uint8_t rd = n->rd;
    if (rd == 0) return true;

    uint32_t protect = (1u << n->rs1) | (1u << n->rs2);
    emit_ctx_t *e = cg->e;
    x64_reg_t dst_h = cg_resolve_dst(cg, rd, protect);
    x64_reg_t lhs_h = cg_resolve_src(cg, n->rs1, RCX, protect);
    x64_reg_t rhs_h = cg_resolve_src(cg, n->rs2, RDX, protect);

    emit_cmp_r32_r32(e, lhs_h, rhs_h);

    switch (n->opcode) {
        case OP_SLT:  emit_setl(e, dst_h);  break;
        case OP_SLTU: emit_setb(e, dst_h);  break;
        case OP_SEQ:  emit_sete(e, dst_h);  break;
        case OP_SNE:  emit_setne(e, dst_h); break;
        case OP_SGT:  emit_setg(e, dst_h);  break;
        case OP_SGTU: emit_seta(e, dst_h);  break;
        case OP_SLE:  emit_setle(e, dst_h); break;
        case OP_SLEU: emit_setbe(e, dst_h); break;
        case OP_SGE:  emit_setge(e, dst_h); break;
        case OP_SGEU: emit_setae(e, dst_h); break;
        default: return false;
    }
    emit_movzx_r32_r8(e, dst_h, dst_h);
    cg_mark_dirty(cg, rd);
    cg_store_spilled(cg, rd, dst_h);
    return true;
}

// Compare immediate: SLTI, SLTIU
static bool cg_emit_cmp_ri(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    uint8_t rd = n->rd;
    if (rd == 0) return true;

    uint32_t protect = (1u << n->rs1);
    emit_ctx_t *e = cg->e;
    x64_reg_t dst_h = cg_resolve_dst(cg, rd, protect);
    x64_reg_t lhs_h = cg_resolve_src(cg, n->rs1, RCX, protect);
    emit_cmp_r32_imm32(e, lhs_h, n->imm);

    switch (n->opcode) {
        case OP_SLTI:  emit_setl(e, dst_h); break;
        case OP_SLTIU: emit_setb(e, dst_h); break;
        default: return false;
    }
    emit_movzx_r32_r8(e, dst_h, dst_h);
    cg_mark_dirty(cg, rd);
    cg_store_spilled(cg, rd, dst_h);
    return true;
}

// ============================================================================
// MULH/MULHU/DIV/REM — native x86-64
// ============================================================================

// MULH: rd = hi(rs1 * rs2) (signed)
// Uses IMUL r/m32 (one-operand form): EDX:EAX = EAX * src (signed)
static bool cg_emit_mulh(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    uint8_t rd = n->rd, rs1 = n->rs1, rs2 = n->rs2;
    if (rd == 0) return true;
    emit_ctx_t *e = cg->e;

    uint32_t protect = (1u << rs1) | (1u << rs2);
    x64_reg_t src1_h = cg_resolve_src(cg, rs1, RAX, protect);
    if (src1_h != RAX) emit_mov_r32_r32(e, RAX, src1_h);
    x64_reg_t src2_h = cg_resolve_src(cg, rs2, RCX, protect);
    emit_imul_one_r32(e, src2_h);  // EDX:EAX = EAX * src2_h (signed)
    x64_reg_t dst_h = cg_resolve_dst(cg, rd, 0);
    emit_mov_r32_r32(e, dst_h, RDX);  // high 32 bits
    cg_mark_dirty(cg, rd);
    cg_store_spilled(cg, rd, dst_h);
    return true;
}

// MULHU: rd = hi(rs1 * rs2) (unsigned)
// Uses MUL r/m32 (one-operand form): EDX:EAX = EAX * src (unsigned)
static bool cg_emit_mulhu(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    uint8_t rd = n->rd, rs1 = n->rs1, rs2 = n->rs2;
    if (rd == 0) return true;
    emit_ctx_t *e = cg->e;

    uint32_t protect = (1u << rs1) | (1u << rs2);
    x64_reg_t src1_h = cg_resolve_src(cg, rs1, RAX, protect);
    if (src1_h != RAX) emit_mov_r32_r32(e, RAX, src1_h);
    x64_reg_t src2_h = cg_resolve_src(cg, rs2, RCX, protect);
    emit_mul_r32(e, src2_h);  // EDX:EAX = EAX * src2_h (unsigned)
    x64_reg_t dst_h = cg_resolve_dst(cg, rd, 0);
    emit_mov_r32_r32(e, dst_h, RDX);  // high 32 bits
    cg_mark_dirty(cg, rd);
    cg_store_spilled(cg, rd, dst_h);
    return true;
}

// DIV: rd = rs1 / rs2 (signed)
// Guard against INT32_MIN / -1 (would SIGFPE on x86).
static bool cg_emit_div(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    uint8_t rd = n->rd, rs1 = n->rs1, rs2 = n->rs2;
    if (rd == 0) return true;
    emit_ctx_t *e = cg->e;

    uint32_t protect = (1u << rs1) | (1u << rs2);
    x64_reg_t src1_h = cg_resolve_src(cg, rs1, RAX, protect);
    if (src1_h != RAX) emit_mov_r32_r32(e, RAX, src1_h);
    x64_reg_t src2_h = cg_resolve_src(cg, rs2, RCX, protect);

    // Guard: INT32_MIN / -1 → INT32_MIN (avoid x86 SIGFPE)
    emit_cmp_r32_imm32(e, src2_h, -1);
    emit_jne_rel32(e, 0);
    size_t patch_not_neg1 = e->offset - 4;
    emit_cmp_r32_imm32(e, RAX, (int32_t)0x80000000);
    emit_jne_rel32(e, 0);
    size_t patch_not_intmin = e->offset - 4;
    // INT32_MIN / -1 = INT32_MIN
    emit_mov_r32_imm32(e, RAX, 0x80000000);
    emit_jmp_rel32(e, 0);
    size_t patch_skip_idiv = e->offset - 4;

    // Normal path
    emit_patch_rel32(e, patch_not_neg1, e->offset);
    emit_patch_rel32(e, patch_not_intmin, e->offset);
    emit_cdq(e);
    emit_idiv_r32(e, src2_h);

    emit_patch_rel32(e, patch_skip_idiv, e->offset);
    x64_reg_t dst_h = cg_resolve_dst(cg, rd, 0);
    if (dst_h != RAX) emit_mov_r32_r32(e, dst_h, RAX);
    cg_mark_dirty(cg, rd);
    cg_store_spilled(cg, rd, dst_h);
    return true;
}

// REM: rd = rs1 % rs2 (signed)
// Guard against INT32_MIN % -1 (would SIGFPE on x86).
static bool cg_emit_rem(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    uint8_t rd = n->rd, rs1 = n->rs1, rs2 = n->rs2;
    if (rd == 0) return true;
    emit_ctx_t *e = cg->e;

    uint32_t protect = (1u << rs1) | (1u << rs2);
    x64_reg_t src1_h = cg_resolve_src(cg, rs1, RAX, protect);
    if (src1_h != RAX) emit_mov_r32_r32(e, RAX, src1_h);
    x64_reg_t src2_h = cg_resolve_src(cg, rs2, RCX, protect);

    // Guard: INT32_MIN % -1 → 0 (avoid x86 SIGFPE)
    emit_cmp_r32_imm32(e, src2_h, -1);
    emit_jne_rel32(e, 0);
    size_t patch_not_neg1 = e->offset - 4;
    emit_cmp_r32_imm32(e, RAX, (int32_t)0x80000000);
    emit_jne_rel32(e, 0);
    size_t patch_not_intmin = e->offset - 4;
    // INT32_MIN % -1 = 0
    emit_xor_r32_r32(e, RAX, RAX);
    emit_jmp_rel32(e, 0);
    size_t patch_skip_idiv = e->offset - 4;

    // Normal path
    emit_patch_rel32(e, patch_not_neg1, e->offset);
    emit_patch_rel32(e, patch_not_intmin, e->offset);
    emit_cdq(e);
    emit_idiv_r32(e, src2_h);
    emit_mov_r32_r32(e, RAX, RDX);  // remainder in EDX

    emit_patch_rel32(e, patch_skip_idiv, e->offset);
    x64_reg_t dst_h = cg_resolve_dst(cg, rd, 0);
    if (dst_h != RAX) emit_mov_r32_r32(e, dst_h, RAX);
    cg_mark_dirty(cg, rd);
    cg_store_spilled(cg, rd, dst_h);
    return true;
}

// ============================================================================
// Load/Store emission — native x86-64 (no Stage 4 delegation)
// ============================================================================

// Flush pending write and pending cond before native emission.
// This is necessary because:
// 1. pending_write may hold a value in RAX that we're about to clobber
// 2. pending_cond may reference registers we're about to write
// Called by both ALU emission (via cg_emit_node) and load/store emission.
static inline void cg_flush_pending(stage5_cg_t *cg) {
    translate_ctx_t *ctx = cg->ctx;
    if (ctx->pending_write.valid) {
        emit_mov_m32_r32(cg->e, RBP,
                         GUEST_REG_OFFSET(ctx->pending_write.guest_reg),
                         ctx->pending_write.host_reg);
        ctx->pending_write.valid = false;
        cg->e->rax_pending = false;
    }
    if (ctx->pending_cond.valid) {
        stage5_flush_pending_for_codegen(ctx);
    }
}

// Compute guest effective address into RAX.
// Uses Stage 5 RA to resolve rs1 — no flush_pending_write, no constant propagation.
static void cg_emit_compute_addr(stage5_cg_t *cg, uint8_t rs1, int32_t imm,
                                  uint32_t protect_mask) {
    emit_ctx_t *e = cg->e;
    if (rs1 == 0) {
        emit_mov_r32_imm32(e, RAX, (uint32_t)imm);
        return;
    }
    x64_reg_t base_h = cg_resolve_src(cg, rs1, RCX, protect_mask);
    if (imm == 0) {
        emit_mov_r32_r32(e, RAX, base_h);
    } else {
        emit_lea_r32_r32_disp(e, RAX, base_h, imm);
    }
}

// Emit inline bounds and W^X checks for a memory access.
// Address must be in RAX. Emits fault stub with JMP-over pattern.
// Uses translate-time constants from ctx->cpu (no bounds_elim, no reg_constants).
static void cg_emit_bounds_check(stage5_cg_t *cg, uint32_t access_size,
                                  bool is_store, uint32_t pc) {
    translate_ctx_t *ctx = cg->ctx;
    emit_ctx_t *e = cg->e;
    dbt_cpu_state_t *cpu = ctx->cpu;

    if (cpu->bounds_checks_disabled) return;

    exit_reason_t reason = is_store ? EXIT_FAULT_STORE : EXIT_FAULT_LOAD;

    if (cpu->mem_size < access_size) {
        // Always faults — emit unconditional fault exit
        emit_fault_exit_for_codegen(ctx, reason, pc, RAX);
        return;
    }

    size_t fault_jumps[8];
    int fault_count = 0;
    size_t ok_jumps[8];
    int ok_count = 0;

    // 1. Bounds check
    uint32_t limit = cpu->mem_size - access_size;
    emit_cmp_r32_imm32(e, RAX, (int32_t)limit);
    fault_jumps[fault_count++] = emit_offset(e) + 2;
    emit_ja_rel32(e, 0);

    // 2. Alignment check
    if (cpu->align_traps_enabled && access_size > 1) {
        emit_mov_r32_r32(e, RDX, RAX);
        emit_and_r32_imm32(e, RDX, (int32_t)(access_size - 1));
        fault_jumps[fault_count++] = emit_offset(e) + 2;
        emit_jne_rel32(e, 0);
    }

    // 3. MMIO disabled check
    if (!cpu->mmio_enabled && cpu->mmio_base != 0) {
        emit_cmp_r32_imm32(e, RAX, (int32_t)cpu->mmio_base);
        ok_jumps[ok_count++] = emit_offset(e) + 2;
        emit_jb_rel32(e, 0);
        emit_cmp_r32_imm32(e, RAX, (int32_t)(cpu->mmio_base + 0x10000));
        fault_jumps[fault_count++] = emit_offset(e) + 2;
        emit_jb_rel32(e, 0);
    }

    // 4. W^X check (stores only)
    if (is_store && cpu->wxorx_enabled) {
        uint32_t wx_limit = cpu->rodata_limit ? cpu->rodata_limit : cpu->code_limit;
        if (wx_limit > 0) {
            emit_cmp_r32_imm32(e, RAX, (int32_t)wx_limit);
            fault_jumps[fault_count++] = emit_offset(e) + 2;
            emit_jb_rel32(e, 0);
        }
    }

    if (fault_count == 0 && ok_count == 0) return;

    // Jump over fault stub (hot path)
    size_t ok_jump = emit_offset(e) + 1;
    emit_jmp_rel32(e, 0);

    // Fault stub (cold path — flush regs, store PC/reason/info, RET)
    size_t fault_offset = emit_offset(e);
    emit_fault_exit_for_codegen(ctx, reason, pc, RAX);

    // Patch all jumps
    size_t ok_offset = emit_offset(e);
    emit_patch_rel32(e, ok_jump, ok_offset);
    for (int i = 0; i < ok_count; i++) {
        emit_patch_rel32(e, ok_jumps[i], ok_offset);
    }
    for (int i = 0; i < fault_count; i++) {
        emit_patch_rel32(e, fault_jumps[i], fault_offset);
    }
}

static bool cg_emit_load(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    if (n->rd == 0) return true;

    // Flush any pending state before we clobber RAX for address computation.
    cg_flush_pending(cg);

    uint32_t protect = (1u << n->rs1);
    emit_ctx_t *e = cg->e;

    // Determine access size
    uint32_t access_size;
    switch (n->opcode) {
        case OP_LDW:                access_size = 4; break;
        case OP_LDH: case OP_LDHU: access_size = 2; break;
        case OP_LDB: case OP_LDBU: access_size = 1; break;
        default: return false;
    }

    // Compute effective address into RAX
    cg_emit_compute_addr(cg, n->rs1, n->imm, protect);

    // Bounds check (address in RAX)
    cg_emit_bounds_check(cg, access_size, false, n->pc);

    // Resolve destination (may evict, but address is safely in RAX)
    x64_reg_t dst_h = cg_resolve_dst(cg, n->rd, 0);

    // Load from guest memory [R14 + RAX]
    switch (n->opcode) {
        case OP_LDW:  emit_mov_r32_m32_idx(e, dst_h, R14, RAX);    break;
        case OP_LDH:  emit_movsx_r32_m16_idx(e, dst_h, R14, RAX);  break;
        case OP_LDB:  emit_movsx_r32_m8_idx(e, dst_h, R14, RAX);   break;
        case OP_LDHU: emit_movzx_r32_m16_idx(e, dst_h, R14, RAX);  break;
        case OP_LDBU: emit_movzx_r32_m8_idx(e, dst_h, R14, RAX);   break;
        default: return false;
    }

    cg_mark_dirty(cg, n->rd);
    cg_store_spilled(cg, n->rd, dst_h);
    stage5_codegen_native_loads++;
    return true;
}

static bool cg_emit_store(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    // Flush any pending Stage 4 state before we clobber RAX for address computation.
    cg_flush_pending(cg);
    emit_ctx_t *e = cg->e;
    uint32_t protect = (1u << n->rs1) | (1u << n->rs2);

    // Determine access size
    uint32_t access_size;
    switch (n->opcode) {
        case OP_STW: access_size = 4; break;
        case OP_STH: access_size = 2; break;
        case OP_STB: access_size = 1; break;
        default: return false;
    }

    // Pre-allocate both operands to prevent mutual eviction
    cg_alloc_reg(cg, n->rs1, false, protect);
    cg_alloc_reg(cg, n->rs2, false, protect);

    // Compute effective address into RAX
    cg_emit_compute_addr(cg, n->rs1, n->imm, protect);

    // Bounds + W^X check (address in RAX)
    cg_emit_bounds_check(cg, access_size, true, n->pc);

    // Resolve value operand (scratch=RCX since RAX holds address)
    x64_reg_t val_h = cg_resolve_src(cg, n->rs2, RCX, protect);

    // Store to guest memory [R14 + RAX]
    switch (n->opcode) {
        case OP_STW: emit_mov_m32_r32_idx(e, R14, RAX, val_h); break;
        case OP_STH: emit_mov_m16_r16_idx(e, R14, RAX, val_h); break;
        case OP_STB: emit_mov_m8_r8_idx(e, R14, RAX, val_h);   break;
        default: return false;
    }

    stage5_codegen_native_stores++;
    return true;
}

// ============================================================================
// IR node dispatch
// ============================================================================

static bool cg_emit_node(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    // Flush pending state before native emission (ALU/compare/multiply/div instructions).
    // Load/store functions call cg_flush_pending() internally, so skip them here
    // to avoid double-flushing (harmless but wasteful).
    bool is_load_store = (n->opcode >= OP_LDB && n->opcode <= OP_STW);
    if (!is_load_store && n->opcode != OP_NOP) {
        cg_flush_pending(cg);
    }

    switch (n->opcode) {
        case OP_NOP:
            return true;

        // ALU register-register
        case OP_ADD:
        case OP_SUB:
        case OP_AND:
        case OP_OR:
        case OP_XOR:
            return cg_emit_alu_rr(cg, n);

        // ALU register-immediate
        case OP_ADDI:
        case OP_ANDI:
        case OP_ORI:
        case OP_XORI:
            return cg_emit_alu_ri(cg, n);

        // Shift immediate
        case OP_SLLI:
        case OP_SRLI:
        case OP_SRAI:
            return cg_emit_shift_imm(cg, n);

        // Shift register
        case OP_SLL:
        case OP_SRL:
        case OP_SRA:
            return cg_emit_shift_reg(cg, n);

        // Upper immediate
        case OP_LUI:
            return cg_emit_lui(cg, n);

        // Multiply
        case OP_MUL:
            return cg_emit_mul(cg, n);

        // Comparisons register-register
        case OP_SLT:
        case OP_SLTU:
        case OP_SEQ:
        case OP_SNE:
        case OP_SGT:
        case OP_SGTU:
        case OP_SLE:
        case OP_SLEU:
        case OP_SGE:
        case OP_SGEU:
            return cg_emit_cmp_rr(cg, n);

        // Comparisons immediate
        case OP_SLTI:
        case OP_SLTIU:
            return cg_emit_cmp_ri(cg, n);

        // Loads
        case OP_LDW:
        case OP_LDH:
        case OP_LDB:
        case OP_LDHU:
        case OP_LDBU:
            return cg_emit_load(cg, n);

        // Stores
        case OP_STW:
        case OP_STH:
        case OP_STB:
            return cg_emit_store(cg, n);

        // Native MULH/MULHU/DIV/REM
        case OP_MULH:
            return cg_emit_mulh(cg, n);
        case OP_MULHU:
            return cg_emit_mulhu(cg, n);
        case OP_DIV:
            return cg_emit_div(cg, n);
        case OP_REM:
            return cg_emit_rem(cg, n);
        case OP_FADD_S: case OP_FSUB_S: case OP_FMUL_S: case OP_FDIV_S:
        case OP_FSQRT_S: case OP_FEQ_S: case OP_FLT_S: case OP_FLE_S:
        case OP_FCVT_W_S: case OP_FCVT_WU_S: case OP_FCVT_S_W: case OP_FCVT_S_WU:
        case OP_FNEG_S: case OP_FABS_S:
        case OP_FADD_D: case OP_FSUB_D: case OP_FMUL_D: case OP_FDIV_D:
        case OP_FSQRT_D: case OP_FEQ_D: case OP_FLT_D: case OP_FLE_D:
        case OP_FCVT_W_D: case OP_FCVT_WU_D: case OP_FCVT_D_W: case OP_FCVT_D_WU:
        case OP_FCVT_D_S: case OP_FCVT_S_D: case OP_FNEG_D: case OP_FABS_D:
        case OP_FCVT_L_S: case OP_FCVT_LU_S: case OP_FCVT_S_L: case OP_FCVT_S_LU:
        case OP_FCVT_L_D: case OP_FCVT_LU_D: case OP_FCVT_D_L: case OP_FCVT_D_LU:
            translate_fp_r_type(cg->ctx, n->opcode, n->rd, n->rs1, n->rs2);
            return true;

        default:
            // Unknown opcode — abort
            stage5_codegen_fallback_unsupported_op++;
            return false;
    }
}

static bool cg_opcode_supported(uint8_t opcode, bool cmp_rr_enabled, bool cmp_ri_enabled) {
    switch (opcode) {
        case OP_NOP:
        case OP_ADD: case OP_SUB: case OP_AND: case OP_OR: case OP_XOR:
        case OP_ADDI: case OP_ANDI: case OP_ORI: case OP_XORI:
        case OP_SLLI: case OP_SRLI: case OP_SRAI:
        case OP_SLL: case OP_SRL: case OP_SRA:
        case OP_LUI: case OP_MUL:
        case OP_MULH: case OP_MULHU: case OP_DIV: case OP_REM:
        case OP_LDW: case OP_LDH: case OP_LDB: case OP_LDHU: case OP_LDBU:
        case OP_STW: case OP_STH: case OP_STB:
        case OP_BEQ: case OP_BNE: case OP_BLT: case OP_BGE: case OP_BLTU: case OP_BGEU:
        case OP_FADD_S: case OP_FSUB_S: case OP_FMUL_S: case OP_FDIV_S:
        case OP_FSQRT_S: case OP_FEQ_S: case OP_FLT_S: case OP_FLE_S:
        case OP_FCVT_W_S: case OP_FCVT_WU_S: case OP_FCVT_S_W: case OP_FCVT_S_WU:
        case OP_FNEG_S: case OP_FABS_S:
        case OP_FADD_D: case OP_FSUB_D: case OP_FMUL_D: case OP_FDIV_D:
        case OP_FSQRT_D: case OP_FEQ_D: case OP_FLT_D: case OP_FLE_D:
        case OP_FCVT_W_D: case OP_FCVT_WU_D: case OP_FCVT_D_W: case OP_FCVT_D_WU:
        case OP_FCVT_D_S: case OP_FCVT_S_D: case OP_FNEG_D: case OP_FABS_D:
        case OP_FCVT_L_S: case OP_FCVT_LU_S: case OP_FCVT_S_L: case OP_FCVT_S_LU:
        case OP_FCVT_L_D: case OP_FCVT_LU_D: case OP_FCVT_D_L: case OP_FCVT_D_LU:
            return true;
        case OP_SLT: case OP_SLTU: case OP_SEQ: case OP_SNE:
        case OP_SGT: case OP_SGTU: case OP_SLE: case OP_SLEU:
        case OP_SGE: case OP_SGEU:
            return cmp_rr_enabled;
        case OP_SLTI: case OP_SLTIU:
            return cmp_ri_enabled;
        default:
            return false;
    }
}

static bool cg_terminal_supported(uint8_t opcode, bool branch_term_enabled) {
    switch (opcode) {
        case OP_JAL:
        case OP_JALR:
        case OP_HALT:
        case OP_DEBUG:
        case OP_YIELD:
            return true;
        case OP_BEQ:
        case OP_BNE:
        case OP_BLT:
        case OP_BGE:
        case OP_BLTU:
        case OP_BGEU:
            return branch_term_enabled;
        default:
            return false;
    }
}

// Identify canonical compare+branch mix candidates when BURG did not provide
// fuse_cmp_idx. Returns:
//   >=0 : cmp idx to fuse with terminal branch
//   -1  : cmp exists but not safely fusible
//   -2  : no cmp in prefix (nothing to fuse)
//
// This supports nonadjacent cmp->branch when simple def-use checks prove the
// compare result register is not used/clobbered before the terminal branch.
static bool cg_is_cmp_opcode(uint8_t opcode) {
    return (opcode == OP_SLT || opcode == OP_SLTU ||
            opcode == OP_SEQ || opcode == OP_SNE ||
            opcode == OP_SGT || opcode == OP_SGTU ||
            opcode == OP_SLE || opcode == OP_SLEU ||
            opcode == OP_SGE || opcode == OP_SGEU ||
            opcode == OP_SLTI || opcode == OP_SLTIU);
}

static bool cg_mix_cmp_opcode_allowed(uint8_t opcode) {
    bool allow_slt = cg_branch_cmp_mix_slt_enabled();
    bool allow_slti = cg_branch_cmp_mix_slti_enabled();
    bool allow_sltiu = cg_branch_cmp_mix_sltiu_enabled();
    bool allow_unsigned_rr = cg_branch_cmp_mix_unsigned_rr_enabled();
    return (opcode == OP_SEQ || opcode == OP_SNE ||
            (allow_slt && opcode == OP_SLT) ||
            (allow_slti && opcode == OP_SLTI) ||
            (allow_sltiu && opcode == OP_SLTIU) ||
            (allow_unsigned_rr && (opcode == OP_SLTU || opcode == OP_SGTU)));
}

// Return true iff `reg` is provably zero at `use_idx` by local backwards scan.
// Conservative by design: only accepts explicit zero-def patterns and bails out
// on any other reaching definition.
static bool cg_reg_known_const01_at_depth(const stage5_lift_region_t *region,
                                          int use_idx,
                                          uint8_t reg,
                                          int depth,
                                          int *out_value) {
    if (out_value) *out_value = -1;
    if (depth > 8) return false;
    if (reg == 0) {
        if (out_value) *out_value = 0;
        return true;
    }
    for (int i = use_idx - 1; i >= 0; i--) {
        const stage5_ir_node_t *n = &region->ir[i];
        if (n->synthetic) continue;
        if (n->rd != reg) continue;
        if (n->opcode == OP_LUI && n->imm == 0) {
            if (out_value) *out_value = 0;
            return true;
        }
        if (n->opcode == OP_ADDI && n->imm == 0) {
            return cg_reg_known_const01_at_depth(region, i, n->rs1, depth + 1, out_value);
        }
        if (n->opcode == OP_ADDI && n->rs1 == 0 && (n->imm == 0 || n->imm == 1)) {
            if (out_value) *out_value = n->imm;
            return true;
        }
        if (n->opcode == OP_ADDI && (n->imm == 0 || n->imm == 1)) {
            int base = -1;
            if (cg_reg_known_const01_at_depth(region, i, n->rs1, depth + 1, &base)) {
                int v = base + n->imm;
                if (v == 0 || v == 1) {
                    if (out_value) *out_value = v;
                    return true;
                }
            }
        }
        if (n->opcode == OP_ORI && n->rs1 == 0 && (n->imm == 0 || n->imm == 1)) {
            if (out_value) *out_value = n->imm;
            return true;
        }
        if (n->opcode == OP_ORI && (n->imm == 0 || n->imm == 1)) {
            int base = -1;
            if (cg_reg_known_const01_at_depth(region, i, n->rs1, depth + 1, &base)) {
                int v = base | n->imm;
                if (v == 0 || v == 1) {
                    if (out_value) *out_value = v;
                    return true;
                }
            }
        }
        if (n->opcode == OP_XORI && n->rs1 == 0 && (n->imm == 0 || n->imm == 1)) {
            if (out_value) *out_value = n->imm;
            return true;
        }
        if (n->opcode == OP_XORI && (n->imm == 0 || n->imm == 1)) {
            int base = -1;
            if (cg_reg_known_const01_at_depth(region, i, n->rs1, depth + 1, &base)) {
                int v = base ^ n->imm;
                if (v == 0 || v == 1) {
                    if (out_value) *out_value = v;
                    return true;
                }
            }
        }
        if (n->opcode == OP_ANDI && (n->imm == 0 || n->imm == 1)) {
            int base = -1;
            if (cg_reg_known_const01_at_depth(region, i, n->rs1, depth + 1, &base)) {
                int v = base & n->imm;
                if (v == 0 || v == 1) {
                    if (out_value) *out_value = v;
                    return true;
                }
            }
        }
        if (n->opcode == OP_XOR || n->opcode == OP_OR || n->opcode == OP_AND) {
            int v1 = -1, v2 = -1;
            if (cg_reg_known_const01_at_depth(region, i, n->rs1, depth + 1, &v1) &&
                cg_reg_known_const01_at_depth(region, i, n->rs2, depth + 1, &v2)) {
                int v = -1;
                if (n->opcode == OP_XOR) v = v1 ^ v2;
                else if (n->opcode == OP_OR) v = v1 | v2;
                else v = v1 & v2;
                if (v == 0 || v == 1) {
                    if (out_value) *out_value = v;
                    return true;
                }
            }
        }
        return false;
    }
    return false;
}

static bool cg_reg_known_const01_at(const stage5_lift_region_t *region,
                                    int use_idx,
                                    uint8_t reg,
                                    int *out_value) {
    return cg_reg_known_const01_at_depth(region, use_idx, reg, 0, out_value);
}

// Extract compare-result register used by terminal BEQ/BNE against constant 0/1.
// Accepts canonical rd-vs-r0, rd-vs-rX where rX is proven 0, and rd-vs-rX where
// rX is proven 1 by flipping BEQ/BNE sense.
static bool cg_branch_cmp_test_cond_info(const stage5_lift_region_t *region,
                                         int terminal_idx,
                                         const stage5_ir_node_t *term,
                                         uint8_t *out_reg,
                                         uint8_t *out_norm_branch_opcode) {
    if (!out_reg) return false;
    if (!out_norm_branch_opcode) return false;
    *out_reg = 0;
    *out_norm_branch_opcode = term->opcode;
    if (!(term->opcode == OP_BEQ || term->opcode == OP_BNE)) return false;
    if (term->rs1 == 0 && term->rs2 != 0) {
        *out_reg = term->rs2;
        return true;
    }
    if (term->rs2 == 0 && term->rs1 != 0) {
        *out_reg = term->rs1;
        return true;
    }
    // Noncanonical form: cond_reg compared against reg proven constant 0/1.
    if (term->rs1 != 0 && term->rs2 != 0) {
        int v2 = -1, v1 = -1;
        bool k2 = cg_reg_known_const01_at(region, terminal_idx, term->rs2, &v2);
        bool k1 = cg_reg_known_const01_at(region, terminal_idx, term->rs1, &v1);
        if (k2 && (v2 == 0 || v2 == 1)) {
            *out_reg = term->rs1;
            if (v2 == 1) {
                *out_norm_branch_opcode = (term->opcode == OP_BEQ) ? OP_BNE : OP_BEQ;
            }
            return true;
        }
        if (k1 && (v1 == 0 || v1 == 1)) {
            *out_reg = term->rs2;
            if (v1 == 1) {
                *out_norm_branch_opcode = (term->opcode == OP_BEQ) ? OP_BNE : OP_BEQ;
            }
            return true;
        }
    }
    return false;
}

static void cg_trace_mix_rd_mismatch(const stage5_lift_region_t *region,
                                     int terminal_idx,
                                     int nearest_def_idx,
                                     uint8_t cond_reg) {
    if (!cg_trace_mix_enabled()) return;
    if (terminal_idx < 0 || terminal_idx >= (int)region->ir_count) return;

    int lo = terminal_idx - 6;
    if (lo < 0) lo = 0;
    int hi = terminal_idx + 1;
    if (hi >= (int)region->ir_count) hi = (int)region->ir_count - 1;

    const stage5_ir_node_t *term = &region->ir[terminal_idx];
    fprintf(stderr,
            "[stage5-mix] rd-mismatch term_idx=%d op=0x%02X rs1=r%u rs2=r%u imm=%d cond=r%u def_idx=%d\n",
            terminal_idx, term->opcode, term->rs1, term->rs2, term->imm,
            cond_reg, nearest_def_idx);
    for (int i = lo; i <= hi; i++) {
        const stage5_ir_node_t *n = &region->ir[i];
        fprintf(stderr,
                "[stage5-mix]   ir[%d] op=0x%02X rd=r%u rs1=r%u rs2=r%u imm=%d syn=%d kind=%d%s%s\n",
                i, n->opcode, n->rd, n->rs1, n->rs2, n->imm,
                n->synthetic ? 1 : 0, n->kind,
                (i == terminal_idx) ? " <term>" : "",
                (i == nearest_def_idx) ? " <def>" : "");
    }
}

static void cg_trace_mix_noncanonical_term(const stage5_lift_region_t *region,
                                           int terminal_idx) {
    if (!cg_trace_mix_enabled()) return;
    if (terminal_idx < 0 || terminal_idx >= (int)region->ir_count) return;
    int lo = terminal_idx - 6;
    if (lo < 0) lo = 0;
    int hi = terminal_idx + 1;
    if (hi >= (int)region->ir_count) hi = (int)region->ir_count - 1;
    const stage5_ir_node_t *term = &region->ir[terminal_idx];
    fprintf(stderr,
            "[stage5-mix] noncanonical-term term_idx=%d op=0x%02X rs1=r%u rs2=r%u imm=%d\n",
            terminal_idx, term->opcode, term->rs1, term->rs2, term->imm);
    for (int i = lo; i <= hi; i++) {
        const stage5_ir_node_t *n = &region->ir[i];
        fprintf(stderr,
                "[stage5-mix]   ir[%d] op=0x%02X rd=r%u rs1=r%u rs2=r%u imm=%d syn=%d kind=%d%s\n",
                i, n->opcode, n->rd, n->rs1, n->rs2, n->imm,
                n->synthetic ? 1 : 0, n->kind,
                (i == terminal_idx) ? " <term>" : "");
    }
}

static int cg_find_branch_cmp_mix_idx(const stage5_lift_region_t *region,
                                      int terminal_idx,
                                      int fuse_cmp_idx,
                                      uint32_t *reason_out,
                                      uint8_t *reason_opcode_out,
                                      uint8_t *norm_branch_opcode_out,
                                      int *extra_skip_idx_out,
                                      uint8_t *trace_cond_reg_out,
                                      int *trace_def_idx_out) {
    if (reason_out) *reason_out = 0;
    if (reason_opcode_out) *reason_opcode_out = 0;
    if (norm_branch_opcode_out) *norm_branch_opcode_out = 0;
    if (extra_skip_idx_out) *extra_skip_idx_out = -1;
    if (trace_cond_reg_out) *trace_cond_reg_out = 0;
    if (trace_def_idx_out) *trace_def_idx_out = -1;
    if (terminal_idx <= 0 || terminal_idx >= (int)region->ir_count || fuse_cmp_idx >= 0) {
        return -2;
    }
    const stage5_ir_node_t *term = &region->ir[terminal_idx];
    if (!(term->opcode == OP_BEQ || term->opcode == OP_BNE)) {
        return -2;
    }

    uint8_t br_cond_reg = 0;
    uint8_t norm_branch_opcode = term->opcode;
    if (!cg_branch_cmp_test_cond_info(region, terminal_idx, term, &br_cond_reg,
                                      &norm_branch_opcode)) {
        if (reason_out) *reason_out = 1;  // noncanonical term
        if (reason_opcode_out) *reason_opcode_out = term->opcode;
        return -1;
    }
    if (trace_cond_reg_out) *trace_cond_reg_out = br_cond_reg;
    if (norm_branch_opcode_out) *norm_branch_opcode_out = norm_branch_opcode;

    int cmp_nodes_seen = 0;
    int first_cmp_idx = -1;
    for (int i = 0; i < terminal_idx; i++) {
        if (fuse_cmp_idx >= 0 && i == fuse_cmp_idx) continue;
        const stage5_ir_node_t *n = &region->ir[i];
        if (n->synthetic) continue;
        if (cg_is_cmp_opcode(n->opcode)) {
            if (first_cmp_idx < 0) first_cmp_idx = i;
            cmp_nodes_seen++;
        }
    }
    if (cmp_nodes_seen == 0) return -2;

    int cmp_idx = -1;
    for (int i = terminal_idx - 1; i >= 0; i--) {
        const stage5_ir_node_t *n = &region->ir[i];
        if (n->synthetic) continue;
        if (n->rd == br_cond_reg) {
            cmp_idx = i;
            break;
        }
    }
    if (cmp_idx < 0) {
        if (reason_out) *reason_out = 4;  // rd mismatch/no defining compare
        if (reason_opcode_out && first_cmp_idx >= 0) {
            *reason_opcode_out = region->ir[first_cmp_idx].opcode;
        }
        if (trace_def_idx_out) *trace_def_idx_out = -1;
        return -1;
    }
    if (trace_def_idx_out) *trace_def_idx_out = cmp_idx;

    const stage5_ir_node_t *cmp = &region->ir[cmp_idx];
    if (!cg_is_cmp_opcode(cmp->opcode)) {
        // Narrow rd-mismatch unlock:
        //   cmp_rd = CMP(...)
        //   br_cond = XOR(cmp_rd, const1_reg)
        //   BEQ/BNE br_cond, 0
        // Normalize to branch on cmp_rd with flipped branch sense and skip XOR.
        if (cmp->opcode == OP_XOR) {
            int xor_idx = cmp_idx;
            uint8_t src_cmp = 0, src_k = 0;
            if (cmp->rs1 != 0 && cmp->rs2 != 0) {
                int k2 = -1, k1 = -1;
                bool rs2_k = cg_reg_known_const01_at(region, cmp_idx, cmp->rs2, &k2) && k2 == 1;
                bool rs1_k = cg_reg_known_const01_at(region, cmp_idx, cmp->rs1, &k1) && k1 == 1;
                if (rs2_k) { src_cmp = cmp->rs1; src_k = cmp->rs2; }
                else if (rs1_k) { src_cmp = cmp->rs2; src_k = cmp->rs1; }
            }
            if (src_cmp != 0 && src_k != 0) {
                int src_def_idx = -1;
                for (int i = cmp_idx - 1; i >= 0; i--) {
                    const stage5_ir_node_t *d = &region->ir[i];
                    if (d->synthetic) continue;
                    if (d->rd == src_cmp) { src_def_idx = i; break; }
                }
                if (src_def_idx >= 0) {
                    const stage5_ir_node_t *d = &region->ir[src_def_idx];
                    if (cg_is_cmp_opcode(d->opcode) && cg_mix_cmp_opcode_allowed(d->opcode)) {
                        for (int i = src_def_idx + 1; i < cmp_idx; i++) {
                            const stage5_ir_node_t *n = &region->ir[i];
                            if (n->synthetic) continue;
                            if (n->rd == src_cmp || n->rs1 == src_cmp || n->rs2 == src_cmp) {
                                if (reason_out) *reason_out = 4;
                                if (reason_opcode_out) *reason_opcode_out = cmp->opcode;
                                return -1;
                            }
                        }
                        cmp_idx = src_def_idx;
                        cmp = &region->ir[cmp_idx];
                        if (norm_branch_opcode_out) {
                            uint8_t b = norm_branch_opcode;
                            *norm_branch_opcode_out = (b == OP_BEQ) ? OP_BNE : OP_BEQ;
                        }
                        if (extra_skip_idx_out) *extra_skip_idx_out = xor_idx;
                    } else {
                        if (reason_out) *reason_out = 4;
                        if (reason_opcode_out) *reason_opcode_out = d->opcode;
                        return -1;
                    }
                } else {
                    if (reason_out) *reason_out = 4;
                    if (reason_opcode_out) *reason_opcode_out = cmp->opcode;
                    return -1;
                }
            } else {
                if (reason_out) *reason_out = 4;
                if (reason_opcode_out) *reason_opcode_out = cmp->opcode;
                return -1;
            }
        } else {
            if (reason_out) *reason_out = 4;  // nearest def is not compare
            if (reason_opcode_out) *reason_opcode_out = cmp->opcode;
            return -1;
        }
    }
    if (!cg_mix_cmp_opcode_allowed(cmp->opcode)) {
        if (reason_out) *reason_out = 2;  // opcode class blocked by gate
        if (reason_opcode_out) *reason_opcode_out = cmp->opcode;
        return -1;
    }

    for (int i = cmp_idx + 1; i < terminal_idx; i++) {
        const stage5_ir_node_t *n = &region->ir[i];
        if (n->synthetic) continue;
        if (n->rd == br_cond_reg ||
            n->rs1 == br_cond_reg ||
            n->rs2 == br_cond_reg) {
            if (reason_out) *reason_out = 3;  // nonadjacent not dataflow-safe
            if (reason_opcode_out) *reason_opcode_out = cmp->opcode;
            return -1;
        }
    }

    return cmp_idx;
}

static bool cg_region_preflight(const stage5_lift_region_t *region,
                                int terminal_idx,
                                int fuse_cmp_idx,
                                int emitted_pattern,
                                bool synth_block_end,
                                bool side_exit_emit_enabled,
                                bool side_exit_family_c) {
    bool cmp_rr_enabled = cg_cmp_rr_enabled();
    bool cmp_ri_enabled = cmp_rr_enabled && cg_cmp_ri_enabled();
    bool fused_branch_enabled = cg_fused_branch_enabled();
    bool branch_term_enabled = cg_branch_term_enabled();
    bool branch_cmp_mix_enabled = cg_branch_cmp_mix_enabled();
    bool use_cmp_mix_fusion = cg_pattern_uses_cmp_mix_fusion(emitted_pattern);
    bool side_exit_enabled = cg_side_exit_enabled();
    bool predicate_branch_only = cg_predicate_branch_only_enabled();

    if (fuse_cmp_idx >= 0 && !fused_branch_enabled) {
        stage5_codegen_fallback_preflight_fused_branch++;
        return false;
    }

    if (!synth_block_end && terminal_idx < 0) {
        stage5_codegen_fallback_preflight_missing_terminal++;
        return false;
    }

    if (terminal_idx >= (int)region->ir_count) {
        stage5_codegen_fallback_preflight_bad_terminal_index++;
        return false;
    }

    if (fuse_cmp_idx >= (int)region->ir_count) {
        stage5_codegen_fallback_preflight_bad_fuse_index++;
        return false;
    }

    for (uint32_t i = 0; i < region->ir_count; i++) {
        const stage5_ir_node_t *n = &region->ir[i];
        if (n->synthetic) continue;
        if (terminal_idx >= 0 && (int)i == terminal_idx) continue;
        if (fuse_cmp_idx >= 0 && (int)i == fuse_cmp_idx) continue;
        if (!cg_opcode_supported(n->opcode, cmp_rr_enabled, cmp_ri_enabled)) {
            stage5_codegen_fallback_preflight_opcode++;
            stage5_codegen_fallback_preflight_opcode_hist[n->opcode & 0x7F]++;
            return false;
        }
        if (n->is_side_exit && n->kind == STAGE5_IR_BRANCH &&
            side_exit_emit_enabled && side_exit_family_c) {
            if (!stage5_side_exit_opcode_supported_for_codegen(n->opcode)) {
                stage5_codegen_fallback_preflight_side_exit++;
                return false;
            }
            if (side_exit_enabled) {
                continue;
            }
            stage5_codegen_fallback_preflight_side_exit++;
            return false;
        }
    }

    if (terminal_idx >= 0) {
        const stage5_ir_node_t *term = &region->ir[terminal_idx];
        if (!cg_terminal_supported(term->opcode, branch_term_enabled)) {
            stage5_codegen_fallback_preflight_terminal++;
            return false;
        }
        if (branch_term_enabled &&
            (term->opcode == OP_BEQ || term->opcode == OP_BNE ||
             term->opcode == OP_BLT || term->opcode == OP_BGE ||
             term->opcode == OP_BLTU || term->opcode == OP_BGEU)) {
            bool has_cmp_prefix = false;
            uint8_t first_cmp_opcode = 0;
            for (uint32_t i = 0; i < region->ir_count; i++) {
                if ((int)i == terminal_idx || (fuse_cmp_idx >= 0 && (int)i == fuse_cmp_idx)) continue;
                const stage5_ir_node_t *n = &region->ir[i];
                if (n->synthetic) continue;
                if (cg_is_cmp_opcode(n->opcode)) {
                    has_cmp_prefix = true;
                    first_cmp_opcode = n->opcode;
                    break;
                }
            }
            if (has_cmp_prefix) {
                if (predicate_branch_only &&
                    !cg_pattern_is_predicate_branch(emitted_pattern)) {
                    stage5_codegen_fallback_preflight_branch_cmp_mix++;
                    stage5_codegen_fallback_preflight_branch_cmp_mix_reason_predicate_policy++;
                    if (emitted_pattern >= 0 && emitted_pattern < 64) {
                        stage5_codegen_fallback_preflight_branch_cmp_mix_reason_predicate_policy_pattern_hist[emitted_pattern]++;
                    }
                    stage5_codegen_fallback_preflight_branch_cmp_mix_opcode_hist[term->opcode & 0x7F]++;
                    return false;
                }
                if (use_cmp_mix_fusion) {
                    uint32_t reason = 0;
                    uint8_t reason_opcode = first_cmp_opcode;
                    int mix_idx = -1;
                    uint8_t mix_norm_branch_opcode = term->opcode;
                    if (branch_cmp_mix_enabled && (term->opcode == OP_BEQ || term->opcode == OP_BNE)) {
                        uint8_t trace_cond_reg = 0;
                        int trace_def_idx = -1;
                        mix_idx = cg_find_branch_cmp_mix_idx(region, terminal_idx, fuse_cmp_idx,
                                                             &reason, &reason_opcode,
                                                             &mix_norm_branch_opcode,
                                                             NULL,
                                                             &trace_cond_reg,
                                                             &trace_def_idx);
                        if (mix_idx < 0 && reason == 4) {
                            cg_trace_mix_rd_mismatch(region, terminal_idx, trace_def_idx, trace_cond_reg);
                        } else if (mix_idx < 0 && reason == 1) {
                            cg_trace_mix_noncanonical_term(region, terminal_idx);
                        }
                    }
                    if (mix_idx < 0) {
                    bool allow_boolpair_terminal = !use_cmp_mix_fusion &&
                        (emitted_pattern == STAGE5_BURG_PATTERN_CMP_BRANCH_BOOLPAIR ||
                         emitted_pattern == STAGE5_BURG_PATTERN_CMP_BRANCH_CMPDEP ||
                         emitted_pattern == STAGE5_BURG_PATTERN_CMP_BRANCH_NOCMPDEP);
                    if (allow_boolpair_terminal) {
                        // Explicit predicate-family shape with non-zero/non-const
                        // boolean operands; use normal branch terminal lowering.
                    } else {
                    bool passthrough = false;
                    if (reason == 4 && reason_opcode == OP_XOR) {
                        passthrough = true;
                        stage5_codegen_branch_cmp_mix_passthrough_rd_mismatch_xor++;
                    }
                    if (passthrough) {
                        // Keep normal branch terminal emission instead of forcing
                        // cmp-mix fallback.
                    } else {
                        stage5_codegen_fallback_preflight_branch_cmp_mix++;
                        stage5_codegen_fallback_preflight_branch_cmp_mix_opcode_hist[reason_opcode & 0x7F]++;
                        switch (reason) {
                            case 1:
                                stage5_codegen_fallback_preflight_branch_cmp_mix_reason_noncanonical_term++;
                                stage5_codegen_fallback_preflight_branch_cmp_mix_reason_noncanonical_term_opcode_hist[reason_opcode & 0x7F]++;
                                stage5_codegen_fallback_preflight_branch_cmp_mix_reason_noncanonical_cmp_opcode_hist[first_cmp_opcode & 0x7F]++;
                                break;
                            case 2: stage5_codegen_fallback_preflight_branch_cmp_mix_reason_opcode++; break;
                            case 3: stage5_codegen_fallback_preflight_branch_cmp_mix_reason_nonadjacent++; break;
                            case 4:
                                stage5_codegen_fallback_preflight_branch_cmp_mix_reason_rd_mismatch++;
                                stage5_codegen_fallback_preflight_branch_cmp_mix_reason_rd_mismatch_opcode_hist[reason_opcode & 0x7F]++;
                                break;
                            case 5: stage5_codegen_fallback_preflight_branch_cmp_mix_reason_multi_cmp++; break;
                            default: break;
                        }
                        return false;
                    }
                    }
                }
                }
            }
        }
    }

    if (fuse_cmp_idx >= 0) {
        if (terminal_idx < 0) return false;
        const stage5_ir_node_t *cmp = &region->ir[fuse_cmp_idx];
        const stage5_ir_node_t *br = &region->ir[terminal_idx];
        if (!(cmp->opcode == OP_SLT || cmp->opcode == OP_SLTU ||
              cmp->opcode == OP_SEQ || cmp->opcode == OP_SNE ||
              cmp->opcode == OP_SGT || cmp->opcode == OP_SGTU ||
              cmp->opcode == OP_SLE || cmp->opcode == OP_SLEU ||
              cmp->opcode == OP_SGE || cmp->opcode == OP_SGEU ||
              cmp->opcode == OP_SLTI || cmp->opcode == OP_SLTIU)) {
            return false;
        }
        if (!(br->opcode == OP_BEQ || br->opcode == OP_BNE ||
              br->opcode == OP_BLT || br->opcode == OP_BGE ||
              br->opcode == OP_BLTU || br->opcode == OP_BGEU)) {
            return false;
        }
    }
    return true;
}

// ============================================================================
// Side exit emission helper
// Replicates the side-exit logic from the Family C path in translate.c,
// using the same deferred exit infrastructure.
// ============================================================================

// Forward declarations for translate.c static functions we need:
// stage5_branch_inv_cc, stage5_side_exit_supported, stage5_pick_cmp_scratch_regs,
// stage5_side_exit_trace_emit
// These are static in translate.c so we can't call them directly.
// Replicate the minimal versions here.

static __attribute__((unused)) bool cg_emit_side_exit(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    (void)cg;
    return stage5_emit_side_exit_for_codegen(cg->ctx,
        n->opcode, n->rs1, n->rs2, n->imm, n->pc);
}

// Native branch terminal emission for predicate families.
// Mirrors stage5 compact branch semantics without routing through translate.c.
static bool cg_emit_branch_terminal_native(stage5_cg_t *cg,
                                           uint8_t opcode,
                                           uint8_t rs1, uint8_t rs2,
                                           int32_t imm,
                                           uint32_t branch_pc) {
    translate_ctx_t *ctx = cg->ctx;
    emit_ctx_t *e = cg->e;
    uint32_t fall_pc = branch_pc + 4;
    uint32_t taken_pc = fall_pc + imm;
    void (*emit_taken_jcc)(emit_ctx_t *, int32_t) = NULL;

    switch (opcode) {
        case OP_BEQ:  emit_taken_jcc = emit_je_rel32;  break;
        case OP_BNE:  emit_taken_jcc = emit_jne_rel32; break;
        case OP_BLT:  emit_taken_jcc = emit_jl_rel32;  break;
        case OP_BGE:  emit_taken_jcc = emit_jge_rel32; break;
        case OP_BLTU: emit_taken_jcc = emit_jb_rel32;  break;
        case OP_BGEU: emit_taken_jcc = emit_jae_rel32; break;
        default: return false;
    }

    if (ctx->block) {
        ctx->block->flags |= BLOCK_FLAG_DIRECT;
    }

    // Ensure deferred compare/write state is materialized before compare.
    stage5_flush_pending_for_codegen(ctx);

    if ((opcode == OP_BEQ || opcode == OP_BNE) && rs1 == 0 && rs2 == 0) {
        emit_xor_r32_r32(e, RAX, RAX);
        emit_test_r32_r32(e, RAX, RAX);
    } else if ((opcode == OP_BEQ || opcode == OP_BNE) && (rs1 == 0 || rs2 == 0)) {
        uint8_t rz = (rs1 == 0) ? rs2 : rs1;
        x64_reg_t hz = cg_guest_host(cg, rz);
        if (hz != X64_NOREG) {
            emit_test_r32_r32(e, hz, hz);
        } else {
            emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(rz));
            emit_test_r32_r32(e, RAX, RAX);
        }
    } else {
        x64_reg_t h1 = cg_guest_host(cg, rs1);
        x64_reg_t h2 = cg_guest_host(cg, rs2);
        x64_reg_t cmp_a = (h1 != X64_NOREG) ? h1 : RAX;
        x64_reg_t cmp_b = (h2 != X64_NOREG) ? h2 : RCX;

        if (h1 == X64_NOREG) {
            emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(rs1));
        }
        if (h2 == X64_NOREG) {
            emit_mov_r32_m32(e, RCX, RBP, GUEST_REG_OFFSET(rs2));
        }
        emit_cmp_r32_r32(e, cmp_a, cmp_b);
    }

    size_t jcc_patch = emit_offset(e) + 2;
    emit_taken_jcc(e, 0);

    if (ctx->block && ctx->exit_idx < MAX_BLOCK_EXITS) {
        ctx->block->exits[ctx->exit_idx].branch_pc = branch_pc;
    }
    emit_exit_chained_for_codegen(ctx, fall_pc, ctx->exit_idx++);

    size_t taken_offset = emit_offset(e);
    emit_patch_rel32(e, jcc_patch, taken_offset);
    if (ctx->block && ctx->exit_idx < MAX_BLOCK_EXITS) {
        ctx->block->exits[ctx->exit_idx].branch_pc = branch_pc;
    }
    emit_exit_chained_for_codegen(ctx, taken_pc, ctx->exit_idx++);

    return true;
}

// ============================================================================
// Main codegen entry point
// ============================================================================

// Forward declarations for functions defined in translate.c that we call
// through their public declarations in translate.h:
//   translate_jal, translate_jalr, translate_halt, translate_debug,
//   translate_yield, translate_beq/bne/blt/bge/bltu/bgeu
//   translate_ldw/ldh/ldb/ldhu/ldbu, translate_stw/sth/stb
//
// We also need stage5_emit_cmp_branch_fused and stage5_translate_branch_terminal
// but these are static in translate.c. We'll handle them via the caller
// (translate.c) by returning from stage5_codegen after prefix emission and
// letting the caller handle the terminal.
//
// Actually, looking at the design more carefully: the plan says to emit
// prefix instructions natively, then delegate terminal to existing emitters.
// The caller already has the terminal dispatch code. So the approach is:
//
// 1. stage5_codegen emits all prefix IR nodes natively
// 2. Returns true with a flag indicating "prefix done, terminal needed"
// 3. Caller handles terminal emission with existing code
//
// But that requires changing the interface. The simpler approach:
// stage5_codegen emits prefix AND terminal, since we can call the public
// translate_* functions for terminals.

// ============================================================================
// Native JAL terminal — replaces translate_jal() for non-JAL_JUMP pattern
// ============================================================================

static void cg_emit_jal_terminal(stage5_cg_t *cg, uint8_t rd, int32_t imm) {
    translate_ctx_t *ctx = cg->ctx;
    emit_ctx_t *e = cg->e;
    uint32_t return_pc = ctx->guest_pc + 4;
    uint32_t target_pc = ctx->guest_pc + imm;

    // Store return address to rd (if rd != 0)
    if (rd != 0) {
        x64_reg_t dst_h = cg_resolve_dst(cg, rd, 0);
        emit_mov_r32_imm32(e, dst_h, return_pc);
        cg_mark_dirty(cg, rd);
        cg_store_spilled(cg, rd, dst_h);
    }

    // Mark this as a call for profiling
    if (ctx->block && rd == REG_LR) {
        ctx->block->flags |= BLOCK_FLAG_CALL;
    }

    // RAS push for calls (rd == REG_LR)
    if (ctx->ras_enabled && rd == REG_LR) {
        // Inline the RAS push: ras_stack[ras_top++ & RAS_MASK] = return_pc
        emit_mov_r32_m32(e, RCX, RBP, CPU_RAS_TOP_OFFSET);
        emit_mov_m32_imm32_sib(e, RBP, RCX, 4, CPU_RAS_STACK_OFFSET, return_pc);
        emit_add_r32_imm32(e, RCX, 1);
        emit_and_r32_imm32(e, RCX, RAS_MASK);
        emit_mov_m32_r32(e, RBP, CPU_RAS_TOP_OFFSET, RCX);
    }

    // Exit chained (shared infrastructure)
    emit_exit_chained_for_codegen(ctx, target_pc, ctx->exit_idx++);
}

// ============================================================================
// Native JALR terminal
// ============================================================================

static void cg_emit_jalr_terminal(stage5_cg_t *cg, uint8_t rd, uint8_t rs1, int32_t imm) {
    translate_ctx_t *ctx = cg->ctx;
    emit_ctx_t *e = cg->e;
    uint32_t return_pc = ctx->guest_pc + 4;

    // target = (rs1 + imm) & ~1
    uint32_t protect = (1u << rs1);
    x64_reg_t src_h = cg_resolve_src(cg, rs1, RAX, protect);
    if (src_h != RAX) {
        emit_mov_r32_r32(e, RAX, src_h);
    }
    if (imm != 0) {
        emit_add_r32_imm32(e, RAX, imm);
    }
    emit_and_r32_imm32(e, RAX, (int32_t)~1u);

    // We use R8/R9 in lookup paths; evict their cached ownership first.
    flush_cached_host_regs_for_codegen(ctx, R8, R9);

    // rd gets return address if linked.
    if (rd != 0) {
        emit_mov_r64_r64(e, R8, RAX); // preserve computed target
        x64_reg_t dst_h = cg_resolve_dst(cg, rd, 0);
        emit_mov_r32_imm32(e, dst_h, return_pc);
        cg_mark_dirty(cg, rd);
        cg_store_spilled(cg, rd, dst_h);
        emit_mov_r64_r64(e, RAX, R8); // restore target
    }

    bool is_return = (rd == 0 && rs1 == REG_LR && imm == 0);
    if (ctx->block) {
        ctx->block->flags |= BLOCK_FLAG_INDIRECT;
        if (is_return) {
            ctx->block->flags |= BLOCK_FLAG_RETURN;
        }
    }

    if (ctx->ras_enabled && is_return && ctx->inline_lookup_enabled) {
        emit_ras_predict_for_codegen(ctx, RAX);
    } else if (ctx->inline_lookup_enabled && ctx->cache) {
        emit_indirect_lookup_for_codegen(ctx, RAX);
    } else {
        emit_mov_m32_r32(e, RBP, CPU_PC_OFFSET, RAX);
        emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, EXIT_INDIRECT);
        emit_ret(e);
    }
}

bool stage5_codegen(translate_ctx_t *ctx,
                    const stage5_lift_region_t *region,
                    uint32_t guest_pc,
                    int terminal_idx,
                    int fuse_cmp_idx,
                    int emitted_pattern,
                    bool synth_block_end,
                    bool side_exit_emit_enabled,
                    const void *side_exit_family_cfg_ptr) {
    stage5_codegen_attempted++;

    stage5_codegen_state_t saved;
    cg_state_save(&saved, ctx);
    if (cg_codegen_skip_pc_enabled(guest_pc)) {
        stage5_codegen_fallback++;
        stage5_codegen_fallback_preflight++;
        return false;
    }
    cg_trace_codegen_region(guest_pc, emitted_pattern, region);
    cg_trace_regalloc(ctx, guest_pc);

    // Count allocated registers
    {
        int count = 0;
        for (int i = 0; i < REG_ALLOC_SLOTS; i++) {
            if (ctx->reg_alloc[i].allocated) count++;
        }
        if (count <= REG_ALLOC_SLOTS)
            stage5_codegen_regs_allocated_hist[count]++;
    }

    // Set up codegen context
    stage5_cg_t cg;
    cg.ctx = ctx;
    cg.e = &ctx->emit;
    cg.region = region;
    cg.emitted_insts = 0;
    cg.current_idx = 0;

    bool terminal_fused = false;
    size_t emit_start = emit_offset(cg.e);

    // The side_exit_family_cfg_ptr is a pointer to a stage5_side_exit_family_cfg_t
    // struct from translate.c. We just need the family_c bool.
    typedef struct { bool family_b; bool family_c; } side_exit_cfg_t;
    const side_exit_cfg_t *se_cfg = (const side_exit_cfg_t *)side_exit_family_cfg_ptr;
    bool side_exit_family_c = se_cfg ? se_cfg->family_c : false;
    bool side_exit_codegen = cg_side_exit_enabled();
    bool use_cmp_mix_fusion = cg_pattern_uses_cmp_mix_fusion(emitted_pattern);
    bool allow_cmpdep_with_side_exit =
        (emitted_pattern == STAGE5_BURG_PATTERN_CMP_BRANCH_CMPDEP) &&
        cg_allow_cmpdep_side_exit();
    bool predicate_native_enabled =
        cg_predicate_native_enabled() || cg_boolpair_native_enabled();
    bool boolpair_native_active =
        (emitted_pattern == STAGE5_BURG_PATTERN_CMP_BRANCH_BOOLPAIR &&
         predicate_native_enabled);
    bool pred_native_active =
        ((emitted_pattern == STAGE5_BURG_PATTERN_CMP_BRANCH_CMPDEP ||
          emitted_pattern == STAGE5_BURG_PATTERN_CMP_BRANCH_NOCMPDEP) &&
         predicate_native_enabled);
    bool predicate_native_active = boolpair_native_active || pred_native_active;
    if (predicate_native_active) {
        stage5_codegen_boolpair_native_attempted++;
        cg_trace_boolpair_region(region, guest_pc, terminal_idx);
        if (cg_boolpair_skip_pc_enabled(guest_pc)) {
            stage5_codegen_fallback++;
            stage5_codegen_fallback_preflight++;
            stage5_codegen_boolpair_native_fallback++;
            return false;
        }
    }

    // Keep newer predicate families on the established translate.c terminal
    // path until dedicated native lowering is validated.
    if ((emitted_pattern == STAGE5_BURG_PATTERN_CMP_BRANCH_BOOLPAIR &&
         !predicate_native_enabled) ||
        ((emitted_pattern == STAGE5_BURG_PATTERN_CMP_BRANCH_CMPDEP ||
          emitted_pattern == STAGE5_BURG_PATTERN_CMP_BRANCH_NOCMPDEP) &&
         !predicate_native_enabled)) {
        stage5_codegen_fallback++;
        stage5_codegen_fallback_preflight++;
        if (predicate_native_active) stage5_codegen_boolpair_native_fallback++;
        return false;
    }
    if (predicate_native_active && side_exit_codegen && region->side_exit_count > 0) {
        if (!allow_cmpdep_with_side_exit) {
            stage5_codegen_fallback++;
            stage5_codegen_fallback_preflight++;
            stage5_codegen_fallback_preflight_side_exit++;
            stage5_codegen_boolpair_native_fallback++;
            return false;
        }
        if (emitted_pattern == STAGE5_BURG_PATTERN_CMP_BRANCH_CMPDEP) {
            bool has_load = false;
            bool has_store = false;
            bool has_store_stb = false;
            bool has_store_sth = false;
            bool has_store_stw = false;
            cg_region_mem_prefix_flags(region, terminal_idx, &has_load, &has_store,
                                       &has_store_stb, &has_store_sth, &has_store_stw);
            if (has_load && !cg_allow_cmpdep_side_exit_loads()) {
                stage5_codegen_fallback++;
                stage5_codegen_fallback_preflight++;
                stage5_codegen_fallback_preflight_side_exit++;
                stage5_codegen_fallback_preflight_side_exit_cmpdep_mem++;
                stage5_codegen_fallback_preflight_side_exit_cmpdep_mem_load++;
                stage5_codegen_boolpair_native_fallback++;
                return false;
            }
            if (has_store_stb && !cg_allow_cmpdep_side_exit_store_stb()) {
                bool block_stb = true;
                if (terminal_idx >= 0 && terminal_idx < (int)region->ir_count) {
                    // Narrow guardrail: current known drift family is stb with
                    // cmpdep side-exit regions that terminate on BEQ.
                    const stage5_ir_node_t *term = &region->ir[terminal_idx];
                    block_stb = (term->opcode == OP_BEQ);
                }
                if (block_stb) {
                    stage5_codegen_fallback++;
                    stage5_codegen_fallback_preflight++;
                    stage5_codegen_fallback_preflight_side_exit++;
                    stage5_codegen_fallback_preflight_side_exit_cmpdep_mem++;
                    stage5_codegen_fallback_preflight_side_exit_cmpdep_mem_store++;
                    stage5_codegen_fallback_preflight_side_exit_cmpdep_mem_store_stb++;
                    stage5_codegen_fallback_preflight_side_exit_cmpdep_mem_store_stb_beq++;
                    stage5_codegen_boolpair_native_fallback++;
                    return false;
                }
            }
            if (has_store_sth && !cg_allow_cmpdep_side_exit_store_sth()) {
                stage5_codegen_fallback++;
                stage5_codegen_fallback_preflight++;
                stage5_codegen_fallback_preflight_side_exit++;
                stage5_codegen_fallback_preflight_side_exit_cmpdep_mem++;
                stage5_codegen_fallback_preflight_side_exit_cmpdep_mem_store++;
                stage5_codegen_fallback_preflight_side_exit_cmpdep_mem_store_sth++;
                stage5_codegen_boolpair_native_fallback++;
                return false;
            }
            if (has_store_stw && !cg_allow_cmpdep_side_exit_store_stw()) {
                stage5_codegen_fallback++;
                stage5_codegen_fallback_preflight++;
                stage5_codegen_fallback_preflight_side_exit++;
                stage5_codegen_fallback_preflight_side_exit_cmpdep_mem++;
                stage5_codegen_fallback_preflight_side_exit_cmpdep_mem_store++;
                stage5_codegen_fallback_preflight_side_exit_cmpdep_mem_store_stw++;
                stage5_codegen_boolpair_native_fallback++;
                return false;
            }
            if (has_store &&
                cg_allow_cmpdep_side_exit_stores_has_override() &&
                !cg_allow_cmpdep_side_exit_stores()) {
                stage5_codegen_fallback++;
                stage5_codegen_fallback_preflight++;
                stage5_codegen_fallback_preflight_side_exit++;
                stage5_codegen_fallback_preflight_side_exit_cmpdep_mem++;
                stage5_codegen_fallback_preflight_side_exit_cmpdep_mem_store++;
                stage5_codegen_boolpair_native_fallback++;
                return false;
            }
        }
    }
    if (emitted_pattern == STAGE5_BURG_PATTERN_CMP_BRANCH_NOCMPDEP) {
        stage5_codegen_fallback++;
        stage5_codegen_fallback_preflight++;
        if (predicate_native_active) stage5_codegen_boolpair_native_fallback++;
        return false;
    }

    int mix_cmp_idx = -1;
    int mix_extra_skip_idx = -1;
    uint8_t mix_branch_opcode = 0;
    if (cg_branch_cmp_mix_enabled() && use_cmp_mix_fusion) {
        mix_cmp_idx = cg_find_branch_cmp_mix_idx(region, terminal_idx, fuse_cmp_idx,
                                                 NULL, NULL, &mix_branch_opcode,
                                                 &mix_extra_skip_idx,
                                                 NULL, NULL);
    }

    if (!cg_region_preflight(region, terminal_idx, fuse_cmp_idx, emitted_pattern,
                             synth_block_end,
                             side_exit_emit_enabled, side_exit_family_c)) {
        stage5_codegen_fallback++;
        stage5_codegen_fallback_preflight++;
        if (predicate_native_active) stage5_codegen_boolpair_native_fallback++;
        return false;
    }

    // Build a region-local SSA overlay (value numbering + def/use links).
    // This is a foundation pass for later SSA-side simplification and RA work.
    if (stage5_ssa_enabled()) {
        stage5_ssa_overlay_t ssa;
        if (!stage5_ssa_build_overlay(region, &ssa)) {
            stage5_codegen_fallback++;
            stage5_codegen_fallback_preflight++;
            if (predicate_native_active) stage5_codegen_boolpair_native_fallback++;
            cg_state_restore(ctx, &saved);
            return false;
        }
    }

    // ========================================================================
    // Phase 1: Emit prefix instructions (everything except terminal)
    // ========================================================================

    for (uint32_t i = 0; i < region->ir_count; i++) {
        cg.current_idx = (int)i;
        const stage5_ir_node_t *n = &region->ir[i];
        if (n->synthetic) continue;
        if (terminal_idx >= 0 && (int)i == terminal_idx) continue;
        if (fuse_cmp_idx >= 0 && (int)i == fuse_cmp_idx) continue;
        if (mix_cmp_idx >= 0 && (int)i == mix_cmp_idx) continue;
        if (mix_extra_skip_idx >= 0 && (int)i == mix_extra_skip_idx) continue;

        // Try to fuse ADDI + LOAD/STORE (Addressing Mode Fusion)
        if (n->opcode == OP_ADDI && n->rd != 0 && n->rd != n->rs1 &&
            terminal_idx >= 0 && (int)i < terminal_idx) {
            int next_idx = -1;
            for (uint32_t k = i + 1; k < region->ir_count; k++) {
                if (region->ir[k].synthetic) continue;
                if ((int)k == terminal_idx) {
                    next_idx = (int)k;
                    break;
                }
                if ((int)k == fuse_cmp_idx) continue;
                if ((int)k == mix_cmp_idx) continue;
                if ((int)k == mix_extra_skip_idx) continue;
                next_idx = (int)k;
                break;
            }

            if (next_idx != -1) {
                const stage5_ir_node_t *next_n = &region->ir[next_idx];
                bool is_mem = (next_n->opcode >= OP_LDB && next_n->opcode <= OP_STW);
                if (is_mem && next_n->rs1 == n->rd) {
                    bool addi_emitted = false;
                    ctx->guest_pc = n->pc;
                    ctx->current_inst_idx = (int)i;
                    if (!cg_emit_node(&cg, n)) {
                         stage5_codegen_fallback++;
                         stage5_codegen_fallback_emit_node++;
                         if (predicate_native_active) stage5_codegen_boolpair_native_fallback++;
                         cg_state_restore(ctx, &saved);
                         return false;
                    }
                    addi_emitted = true;
                    cg.emitted_insts++;

                    ctx->guest_pc = next_n->pc;
                    ctx->current_inst_idx = next_idx;
                    int32_t total_disp = n->imm + next_n->imm;
                    bool fused_ok = false;
                    // Build fused node with ADDI's original base + combined displacement
                    stage5_ir_node_t fused = *next_n;
                    fused.rs1 = n->rs1;
                    fused.imm = total_disp;
                    bool is_load = (next_n->opcode >= OP_LDB && next_n->opcode <= OP_LDHU);
                    if (is_load) {
                        fused_ok = cg_emit_load(&cg, &fused);
                    } else {
                        fused_ok = cg_emit_store(&cg, &fused);
                    }
                    if (fused_ok) {
                        stage5_codegen_fused_addi_mem++;
                        cg.emitted_insts++;
                        if (next_idx == terminal_idx) terminal_fused = true;
                        i = next_idx; 
                        continue;
                    }

                    // ADDI already emitted above; if fused mem emission did not
                    // happen, continue without re-emitting ADDI in the generic path.
                    if (addi_emitted) {
                        continue;
                    }
                }
            }
        }

        // Optional side-exit ownership by native codegen.
        if (n->is_side_exit && n->kind == STAGE5_IR_BRANCH &&
            side_exit_emit_enabled && side_exit_family_c &&
            ctx->exit_idx + 2 < MAX_BLOCK_EXITS &&
            ctx->deferred_exit_count < MAX_BLOCK_EXITS) {
            // Guardrail: predicate-native regions with in-prefix side exits
            // still have unresolved correctness drift on some loops.
            // Keep them on the established fallback path for now.
            if (predicate_native_active && !allow_cmpdep_with_side_exit) {
                stage5_codegen_fallback++;
                stage5_codegen_fallback_side_exit++;
                if (predicate_native_active) stage5_codegen_boolpair_native_fallback++;
                cg_state_restore(ctx, &saved);
                return false;
            }
            if (side_exit_codegen) {
                if (!cg_emit_side_exit(&cg, n)) {
                    stage5_codegen_fallback++;
                    stage5_codegen_fallback_side_exit++;
                    if (predicate_native_active) stage5_codegen_boolpair_native_fallback++;
                    cg_state_restore(ctx, &saved);
                    return false;
                }
                cg.emitted_insts++;
                continue;
            } else {
                stage5_codegen_fallback++;
                stage5_codegen_fallback_side_exit++;
                if (predicate_native_active) stage5_codegen_boolpair_native_fallback++;
                cg_state_restore(ctx, &saved);
                return false;
            }
        }

        ctx->guest_pc = n->pc;
        ctx->current_inst_idx = (int)i;

        if (!cg_emit_node(&cg, n)) {
            // Unsupported instruction — abort
            stage5_codegen_fallback++;
            stage5_codegen_fallback_emit_node++;
            if (predicate_native_active) stage5_codegen_boolpair_native_fallback++;
            cg_state_restore(ctx, &saved);
            return false;
        }
        cg.emitted_insts++;
    }

    // ========================================================================
    // Phase 2: Emit terminal
    // ========================================================================

    bool ended = false;

    if (terminal_fused) {
        ended = true;
    } else if (synth_block_end) {
        ctx->guest_pc = guest_pc + region->guest_inst_count * 4;
        // reg_cache_flush is called by emit_exit_chained
        emit_exit_chained_for_codegen(ctx, ctx->guest_pc, ctx->exit_idx++);
        ended = true;
    } else if (fuse_cmp_idx >= 0 && terminal_idx >= 0) {
        // Fused compare+branch — delegate to existing emitter
        const stage5_ir_node_t *c = &region->ir[fuse_cmp_idx];
        const stage5_ir_node_t *b = &region->ir[terminal_idx];
        bool cmp_is_imm = (c->opcode == OP_SLTI || c->opcode == OP_SLTIU);
        ctx->guest_pc = b->pc;
        ctx->current_inst_idx = terminal_idx;
        ended = stage5_emit_cmp_branch_fused_for_codegen(ctx,
            c->opcode, c->rs1, c->rs2, c->imm, cmp_is_imm,
            b->opcode, b->imm, b->pc);
    } else if (mix_cmp_idx >= 0 && terminal_idx >= 0) {
        // Native fast path for canonical compare-result branch mixes.
        const stage5_ir_node_t *c = &region->ir[mix_cmp_idx];
        const stage5_ir_node_t *b = &region->ir[terminal_idx];
        ctx->guest_pc = b->pc;
        ctx->current_inst_idx = terminal_idx;
        uint8_t branch_opcode = mix_branch_opcode ? mix_branch_opcode : b->opcode;
        ended = stage5_emit_cmp_branch_fused_for_codegen(ctx,
            c->opcode, c->rs1, c->rs2, 0, false,
            branch_opcode, b->imm, b->pc);
    } else if (terminal_idx >= 0) {
        const stage5_ir_node_t *last = &region->ir[terminal_idx];
        ctx->guest_pc = last->pc;
        ctx->current_inst_idx = terminal_idx;
        if (predicate_native_active) {
            stage5_flush_pending_for_codegen(ctx);
        }
        switch (last->opcode) {
            case OP_JAL:
                if (emitted_pattern == STAGE5_BURG_PATTERN_JAL_JUMP) {
                    stage5_translate_jal_jump_compact_for_codegen(ctx,
                        last->rd, last->imm);
                } else {
                    cg_emit_jal_terminal(&cg, last->rd, last->imm);
                }
                ended = true;
                break;
            case OP_JALR:
                cg_emit_jalr_terminal(&cg, last->rd, last->rs1, last->imm);
                ended = true;
                break;
            case OP_HALT:
                translate_halt(ctx);
                ended = true;
                break;
            case OP_DEBUG:
                translate_debug(ctx, last->rs1);
                ended = true;
                break;
            case OP_YIELD:
                translate_yield(ctx);
                ended = true;
                break;
            case OP_BEQ:
            case OP_BNE:
            case OP_BLT:
            case OP_BGE:
            case OP_BLTU:
            case OP_BGEU:
                if (predicate_native_active) {
                    ended = cg_emit_branch_terminal_native(&cg, last->opcode,
                        last->rs1, last->rs2, last->imm, last->pc);
                    if (ended) {
                        stage5_codegen_predicate_native_terminal_success++;
                        stage5_codegen_predicate_native_terminal_opcode_hist[last->opcode & 0x7F]++;
                    } else {
                        stage5_codegen_predicate_native_terminal_fallback++;
                    }
                } else {
                    ended = stage5_translate_branch_terminal_for_codegen(ctx,
                        last->opcode, last->rs1, last->rs2, last->imm);
                }
                break;
            default:
                ended = false;
                break;
        }
    }

    if (!ended) {
        stage5_codegen_fallback++;
        stage5_codegen_fallback_terminal++;
        if (predicate_native_active) stage5_codegen_boolpair_native_fallback++;
        if (terminal_idx >= 0 && terminal_idx < (int)region->ir_count) {
            stage5_codegen_fallback_terminal_opcode_hist[
                region->ir[terminal_idx].opcode & 0x7F]++;
        }
        cg_state_restore(ctx, &saved);
        return false;
    }

    // ========================================================================
    // Success: update telemetry
    // ========================================================================

    size_t emit_end = emit_offset(cg.e);
    stage5_codegen_success++;
    if (predicate_native_active) stage5_codegen_boolpair_native_success++;
    stage5_codegen_guest_insts += region->guest_inst_count;
    if (emit_end > emit_start) {
        stage5_codegen_host_bytes += (uint64_t)(emit_end - emit_start);
    }
    if (predicate_native_active) {
        stage5_codegen_predicate_native_guest_insts += region->guest_inst_count;
        if (emit_end > emit_start) {
            stage5_codegen_predicate_native_host_bytes +=
                (uint64_t)(emit_end - emit_start);
        }
    }

    return true;
}
