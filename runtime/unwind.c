/*
 * SLOW-32 DWARF Stack Unwinder (libunwind)
 *
 * Implements the Itanium ABI _Unwind_* functions using DWARF .eh_frame data.
 * Parses CIE/FDE records, executes DWARF CFA bytecode to determine register
 * save locations, and performs two-phase stack unwinding for C++ exceptions.
 *
 * Simplifications for SLOW-32:
 * - Single-threaded (no locking)
 * - No PIC (absolute 32-bit pointers, DW_EH_PE_absptr everywhere)
 * - No dynamic linking (linker provides __eh_frame_start/end)
 * - FP always saved (common CFA = FP + 0)
 */

#include <stdint.h>
#include <string.h>

/* Enable debug tracing: compile with -DDEBUG_UNWIND=1 to see unwinder internals */
#ifndef DEBUG_UNWIND
#define DEBUG_UNWIND 0
#endif

#if DEBUG_UNWIND
extern int printf(const char *, ...);
#define UNWIND_TRACE(...) printf(__VA_ARGS__)
#else
#define UNWIND_TRACE(...) ((void)0)
#endif

/* Linker-provided symbols for .eh_frame discovery */
extern uint32_t __eh_frame_start;
extern uint32_t __eh_frame_end;

/* ========================================================================
 * Unwind context: holds register state during unwinding
 * ======================================================================== */

typedef struct _Unwind_Context {
    uint32_t regs[32];      /* r0..r31 */
    uint32_t ip;            /* instruction pointer (PC) */
    /* Per-frame info from FDE lookup */
    uint32_t region_start;  /* function start address */
    uint32_t lsda;          /* language-specific data area pointer */
    uint32_t personality;   /* personality routine address */
} _Unwind_Context;

/* ========================================================================
 * Include the public API header (after _Unwind_Context is defined)
 * ======================================================================== */

/* We redefine struct _Unwind_Context above, so just pull in the types/enums */
#include "include/unwind.h"

/* ========================================================================
 * DWARF CFA constants
 * ======================================================================== */

#define DW_CFA_advance_loc      0x40
#define DW_CFA_offset           0x80
#define DW_CFA_restore          0xC0
#define DW_CFA_nop              0x00
#define DW_CFA_set_loc          0x01
#define DW_CFA_advance_loc1     0x02
#define DW_CFA_advance_loc2     0x03
#define DW_CFA_advance_loc4     0x04
#define DW_CFA_offset_extended  0x05
#define DW_CFA_restore_extended 0x06
#define DW_CFA_def_cfa_op       0x0C
#define DW_CFA_def_cfa_register 0x0D
#define DW_CFA_def_cfa_offset   0x0E
#define DW_CFA_remember_state   0x0A
#define DW_CFA_restore_state    0x0B

/* DW_EH_PE encodings */
#define DW_EH_PE_absptr 0x00
#define DW_EH_PE_omit   0xFF

/* ========================================================================
 * LEB128 decoding
 * ======================================================================== */

static uint32_t read_uleb128(const uint8_t **p) {
    uint32_t result = 0;
    int shift = 0;
    uint8_t byte;
    do {
        byte = *(*p)++;
        result |= (uint32_t)(byte & 0x7F) << shift;
        shift += 7;
    } while (byte & 0x80);
    return result;
}

static int32_t read_sleb128(const uint8_t **p) {
    int32_t result = 0;
    int shift = 0;
    uint8_t byte;
    do {
        byte = *(*p)++;
        result |= (int32_t)(byte & 0x7F) << shift;
        shift += 7;
    } while (byte & 0x80);
    /* Sign extend */
    if (shift < 32 && (byte & 0x40))
        result |= -(1 << shift);
    return result;
}

static uint32_t read_u32(const uint8_t **p) {
    uint32_t val;
    memcpy(&val, *p, 4);
    *p += 4;
    return val;
}

static uint16_t read_u16(const uint8_t **p) {
    uint16_t val;
    memcpy(&val, *p, 2);
    *p += 2;
    return val;
}

/* ========================================================================
 * CIE/FDE parsing
 * ======================================================================== */

typedef struct {
    const uint8_t *start;       /* start of CIE record (after length) */
    uint32_t code_align;
    int32_t data_align;
    uint32_t ra_reg;
    const uint8_t *augmentation;
    uint32_t aug_len;
    uint8_t fde_encoding;       /* 'R' augmentation */
    uint8_t lsda_encoding;      /* 'L' augmentation */
    uint8_t personality_encoding;/* 'P' augmentation */
    uint32_t personality;        /* personality routine address */
    const uint8_t *initial_instructions;
    uint32_t initial_instructions_len;
} cie_info_t;

typedef struct {
    uint32_t pc_begin;
    uint32_t pc_range;
    uint32_t lsda;
    const cie_info_t *cie;
    const uint8_t *instructions;
    uint32_t instructions_len;
} fde_info_t;

/* Read an encoded pointer value (only absptr supported for SLOW-32) */
static uint32_t read_encoded_ptr(const uint8_t **p, uint8_t encoding) {
    if (encoding == DW_EH_PE_omit) return 0;
    /* For SLOW-32, all pointers are absolute 32-bit */
    return read_u32(p);
}

/* Parse a CIE record */
static int parse_cie(const uint8_t *data, uint32_t length, cie_info_t *cie) {
    const uint8_t *p = data;
    const uint8_t *end = data + length;

    cie->start = data;
    cie->fde_encoding = DW_EH_PE_absptr;
    cie->lsda_encoding = DW_EH_PE_omit;
    cie->personality_encoding = DW_EH_PE_omit;
    cie->personality = 0;

    /* Skip CIE_id field (4 bytes, value 0x00000000) — already peeked by caller */
    p += 4;

    /* Version */
    uint8_t version = *p++;
    if (version != 1 && version != 3) return -1;

    /* Augmentation string */
    cie->augmentation = p;
    while (*p) p++;
    p++; /* skip null terminator */

    cie->code_align = read_uleb128(&p);
    cie->data_align = read_sleb128(&p);
    cie->ra_reg = read_uleb128(&p);

    /* Parse augmentation data if 'z' prefix */
    if (cie->augmentation[0] == 'z') {
        cie->aug_len = read_uleb128(&p);
        const uint8_t *aug_end = p + cie->aug_len;
        const uint8_t *aug = cie->augmentation + 1; /* skip 'z' */

        while (*aug) {
            switch (*aug++) {
                case 'P':
                    cie->personality_encoding = *p++;
                    cie->personality = read_encoded_ptr(&p, cie->personality_encoding);
                    break;
                case 'L':
                    cie->lsda_encoding = *p++;
                    break;
                case 'R':
                    cie->fde_encoding = *p++;
                    break;
                default:
                    /* Unknown augmentation, skip to end */
                    p = aug_end;
                    goto done_aug;
            }
        }
done_aug:
        p = aug_end; /* ensure we're past augmentation data */
    }

    cie->initial_instructions = p;
    cie->initial_instructions_len = (uint32_t)(end - p);
    return 0;
}

/* Parse an FDE record */
static int parse_fde(const uint8_t *data, uint32_t length,
                     const uint8_t *eh_frame_start, const cie_info_t *cies,
                     int num_cies, fde_info_t *fde) {
    const uint8_t *p = data;
    const uint8_t *end = data + length;

    /* CIE pointer: offset from this field back to the CIE's length field.
     * In .eh_frame, cie_ptr = address_of_this_field - address_of_CIE_record.
     * data points to the CIE_pointer field itself. */
    uint32_t cie_ptr = read_u32(&p);
    const uint8_t *cie_record = data - cie_ptr;

    /* Match against known CIEs. cie->start points to the CIE body (CIE_id field),
     * which is 4 bytes after the CIE record start (the length field). */
    fde->cie = NULL;
    for (int i = 0; i < num_cies; i++) {
        if (cies[i].start - 4 == cie_record) {
            fde->cie = &cies[i];
            break;
        }
    }
    if (!fde->cie) return -1;

    fde->pc_begin = read_encoded_ptr(&p, fde->cie->fde_encoding);
    fde->pc_range = read_encoded_ptr(&p, fde->cie->fde_encoding);
    fde->lsda = 0;

    /* Augmentation data */
    if (fde->cie->augmentation[0] == 'z') {
        uint32_t aug_len = read_uleb128(&p);
        const uint8_t *aug_end = p + aug_len;
        if (fde->cie->lsda_encoding != DW_EH_PE_omit && aug_len > 0) {
            fde->lsda = read_encoded_ptr(&p, fde->cie->lsda_encoding);
        }
        p = aug_end;
    }

    fde->instructions = p;
    fde->instructions_len = (uint32_t)(end - p);
    return 0;
}

/* ========================================================================
 * DWARF CFA interpreter
 * ======================================================================== */

/* Register save rule types */
#define RULE_UNDEFINED  0
#define RULE_SAME       1
#define RULE_OFFSET     2  /* saved at CFA + offset */
#define RULE_REGISTER   3

typedef struct {
    uint8_t rule;
    int32_t value;  /* offset for RULE_OFFSET, reg for RULE_REGISTER */
} reg_rule_t;

typedef struct {
    uint32_t cfa_reg;
    uint32_t cfa_offset;
    reg_rule_t regs[32];
} cfa_state_t;

#define MAX_STATE_STACK 4

static void execute_cfi(const uint8_t *instrs, uint32_t len,
                        uint32_t code_align, int32_t data_align,
                        uint32_t target_offset, cfa_state_t *state) {
    const uint8_t *p = instrs;
    const uint8_t *end = instrs + len;
    uint32_t loc = 0;
    cfa_state_t state_stack[MAX_STATE_STACK];
    int stack_depth = 0;

    while (p < end && loc <= target_offset) {
        uint8_t op = *p++;
        uint8_t high2 = op & 0xC0;
        uint8_t low6 = op & 0x3F;

        if (high2 == DW_CFA_advance_loc) {
            loc += low6 * code_align;
            if (loc > target_offset) break;
        } else if (high2 == DW_CFA_offset) {
            uint32_t offset = read_uleb128(&p);
            state->regs[low6].rule = RULE_OFFSET;
            state->regs[low6].value = (int32_t)(offset * data_align);
        } else if (high2 == DW_CFA_restore) {
            state->regs[low6].rule = RULE_SAME;
            state->regs[low6].value = 0;
        } else {
            switch (op) {
                case DW_CFA_nop:
                    break;
                case DW_CFA_set_loc:
                    loc = read_u32(&p);
                    if (loc > target_offset) goto done;
                    break;
                case DW_CFA_advance_loc1:
                    loc += (*p++) * code_align;
                    if (loc > target_offset) goto done;
                    break;
                case DW_CFA_advance_loc2:
                    loc += read_u16(&p) * code_align;
                    if (loc > target_offset) goto done;
                    break;
                case DW_CFA_advance_loc4:
                    loc += read_u32(&p) * code_align;
                    if (loc > target_offset) goto done;
                    break;
                case DW_CFA_offset_extended: {
                    uint32_t reg = read_uleb128(&p);
                    uint32_t off = read_uleb128(&p);
                    if (reg < 32) {
                        state->regs[reg].rule = RULE_OFFSET;
                        state->regs[reg].value = (int32_t)(off * data_align);
                    }
                    break;
                }
                case DW_CFA_restore_extended: {
                    uint32_t reg = read_uleb128(&p);
                    if (reg < 32) {
                        state->regs[reg].rule = RULE_SAME;
                        state->regs[reg].value = 0;
                    }
                    break;
                }
                case DW_CFA_def_cfa_op: {
                    state->cfa_reg = read_uleb128(&p);
                    state->cfa_offset = read_uleb128(&p);
                    break;
                }
                case DW_CFA_def_cfa_register:
                    state->cfa_reg = read_uleb128(&p);
                    break;
                case DW_CFA_def_cfa_offset:
                    state->cfa_offset = read_uleb128(&p);
                    break;
                case DW_CFA_remember_state:
                    if (stack_depth < MAX_STATE_STACK) {
                        state_stack[stack_depth++] = *state;
                    }
                    break;
                case DW_CFA_restore_state:
                    if (stack_depth > 0) {
                        *state = state_stack[--stack_depth];
                    }
                    break;
                default:
                    /* Unknown opcode — skip (best effort) */
                    goto done;
            }
        }
    }
done:
    return;
}

/* ========================================================================
 * FDE lookup: find the FDE containing a given PC
 * ======================================================================== */

/* Maximum CIEs/FDEs we can handle */
#define MAX_CIES 64
#define MAX_FDES 512

static cie_info_t g_cies[MAX_CIES];
static int g_num_cies = 0;
static fde_info_t g_fdes[MAX_FDES];
static int g_num_fdes = 0;
static int g_eh_frame_parsed = 0;

static void parse_eh_frame(void) {
    if (g_eh_frame_parsed) return;
    g_eh_frame_parsed = 1;

    const uint8_t *start = (const uint8_t *)&__eh_frame_start;
    const uint8_t *end = (const uint8_t *)&__eh_frame_end;
    const uint8_t *p = start;

    UNWIND_TRACE("[unwind] parse_eh_frame: start=0x%X end=0x%X (%d bytes)\n",
                 (uint32_t)(uintptr_t)start, (uint32_t)(uintptr_t)end,
                 (int)(end - start));

    while (p < end) {
        uint32_t length = read_u32(&p);
        if (length == 0) continue; /* skip zero terminators (between merged sections) */
        const uint8_t *record_end = p + length;

        /* Peek at CIE_id/CIE_pointer */
        uint32_t id;
        memcpy(&id, p, 4);

        if (id == 0) {
            /* CIE */
            const uint8_t *cie_data = p;
            p += 4; /* skip CIE_id */
            if (g_num_cies < MAX_CIES) {
                int rc = parse_cie(cie_data, length, &g_cies[g_num_cies]);
                if (rc == 0) {
                    UNWIND_TRACE("[unwind]   CIE #%d: aug='%s' personality=0x%X\n",
                                 g_num_cies, (const char *)g_cies[g_num_cies].augmentation,
                                 g_cies[g_num_cies].personality);
                    g_num_cies++;
                } else {
                    UNWIND_TRACE("[unwind]   CIE PARSE FAILED at offset 0x%X len=%d\n",
                                 (uint32_t)(cie_data - start), length);
                }
            }
        } else {
            /* FDE */
            if (g_num_fdes < MAX_FDES) {
                int rc = parse_fde(p, length, start, g_cies, g_num_cies,
                              &g_fdes[g_num_fdes]);
                if (rc == 0) {
                    UNWIND_TRACE("[unwind]   FDE #%d: pc=0x%X..0x%X lsda=0x%X\n",
                                 g_num_fdes, g_fdes[g_num_fdes].pc_begin,
                                 g_fdes[g_num_fdes].pc_begin + g_fdes[g_num_fdes].pc_range,
                                 g_fdes[g_num_fdes].lsda);
                    g_num_fdes++;
                } else {
                    UNWIND_TRACE("[unwind]   FDE PARSE FAILED at offset 0x%X len=%d cie_ptr=0x%X\n",
                                 (uint32_t)(p - start), length, id);
                }
            }
        }

        p = record_end;
    }

    UNWIND_TRACE("[unwind] parse_eh_frame: %d CIEs, %d FDEs\n", g_num_cies, g_num_fdes);
}

static fde_info_t *find_fde(uint32_t pc) {
    parse_eh_frame();
    for (int i = 0; i < g_num_fdes; i++) {
        if (pc >= g_fdes[i].pc_begin &&
            pc < g_fdes[i].pc_begin + g_fdes[i].pc_range) {
            UNWIND_TRACE("[unwind] find_fde(0x%X) -> FDE #%d [0x%X..0x%X]\n",
                         pc, i, g_fdes[i].pc_begin,
                         g_fdes[i].pc_begin + g_fdes[i].pc_range);
            return &g_fdes[i];
        }
    }
    UNWIND_TRACE("[unwind] find_fde(0x%X) -> NOT FOUND\n", pc);
    return NULL;
}

/* ========================================================================
 * Frame stepping: restore previous frame's registers
 * ======================================================================== */

static int step_frame(_Unwind_Context *ctx) {
    UNWIND_TRACE("[unwind] step_frame: ip=0x%X sp=0x%X fp=0x%X\n",
                 ctx->ip, ctx->regs[29], ctx->regs[30]);
    /* Use ip-1 for FDE lookup: the return address may point past the end of
     * the calling function (e.g., noreturn calls like __cxa_throw).
     * Subtracting 1 ensures we land inside the calling function's range.
     * This is the standard DWARF convention for non-signal frames. */
    uint32_t lookup_ip = ctx->ip > 0 ? ctx->ip - 1 : ctx->ip;
    fde_info_t *fde = find_fde(lookup_ip);
    if (!fde || !fde->cie) return -1;

    /* Note: personality/LSDA/region_start are NOT set here. step_frame
     * advances the context to the CALLER frame, so those fields must be
     * looked up for the NEW ip after step_frame returns. See
     * lookup_frame_info() which is called after step_frame(). */

    /* Initialize CFA state from CIE initial instructions */
    cfa_state_t state;
    memset(&state, 0, sizeof(state));
    state.cfa_reg = 29; /* SP default */

    uint32_t code_align = fde->cie->code_align;
    int32_t data_align = fde->cie->data_align;

    /* Execute CIE initial instructions (no target offset limit) */
    execute_cfi(fde->cie->initial_instructions,
                fde->cie->initial_instructions_len,
                code_align, data_align, 0xFFFFFFFF, &state);

    /* Execute FDE instructions up to current PC (use lookup_ip for consistency) */
    uint32_t pc_offset = lookup_ip - fde->pc_begin;
    execute_cfi(fde->instructions, fde->instructions_len,
                code_align, data_align, pc_offset, &state);

    /* Compute CFA value */
    uint32_t cfa = ctx->regs[state.cfa_reg] + state.cfa_offset;

    /* Restore registers from CFA-relative offsets */
    uint32_t new_regs[32];
    memcpy(new_regs, ctx->regs, sizeof(new_regs));

    for (int i = 0; i < 32; i++) {
        if (state.regs[i].rule == RULE_OFFSET) {
            /* Register saved at CFA + offset */
            uint32_t addr = (uint32_t)((int32_t)cfa + state.regs[i].value);
            uint32_t val;
            memcpy(&val, (void *)addr, 4);
            new_regs[i] = val;
        }
    }

    /* Restore return address as new IP */
    if (state.regs[fde->cie->ra_reg].rule == RULE_OFFSET) {
        uint32_t ra_addr = (uint32_t)((int32_t)cfa + state.regs[fde->cie->ra_reg].value);
        uint32_t ra;
        memcpy(&ra, (void *)ra_addr, 4);
        ctx->ip = ra;
    } else {
        /* RA register not saved — use current value */
        ctx->ip = ctx->regs[fde->cie->ra_reg];
    }

    /* SP = CFA (by definition, CFA is the caller's stack pointer) */
    new_regs[29] = cfa;

    memcpy(ctx->regs, new_regs, sizeof(ctx->regs));
    UNWIND_TRACE("[unwind] step_frame -> ip=0x%X sp=0x%X fp=0x%X\n",
                 ctx->ip, ctx->regs[29], ctx->regs[30]);
    return 0;
}

/* Look up the FDE for the current frame's IP and populate
 * personality/LSDA/region_start in the context. Called after step_frame()
 * advances the context to a new frame — we need these fields to reflect
 * the frame ctx now represents, not the one we stepped out of. */
static void lookup_frame_info(_Unwind_Context *ctx) {
    uint32_t lookup_ip = ctx->ip > 0 ? ctx->ip - 1 : ctx->ip;
    fde_info_t *fde = find_fde(lookup_ip);
    if (fde && fde->cie) {
        ctx->region_start = fde->pc_begin;
        ctx->lsda = fde->lsda;
        ctx->personality = fde->cie->personality;
    } else {
        ctx->region_start = 0;
        ctx->lsda = 0;
        ctx->personality = 0;
    }
    UNWIND_TRACE("[unwind] lookup_frame_info: ip=0x%X -> region=0x%X lsda=0x%X personality=0x%X\n",
                 ctx->ip, ctx->region_start, ctx->lsda, ctx->personality);
}

/* ========================================================================
 * Context accessors (called by personality routines)
 * ======================================================================== */

uint32_t _Unwind_GetGR(struct _Unwind_Context *ctx, int reg) {
    if (reg < 0 || reg > 31) return 0;
    return ctx->regs[reg];
}

void _Unwind_SetGR(struct _Unwind_Context *ctx, int reg, uint32_t value) {
    if (reg >= 0 && reg <= 31)
        ctx->regs[reg] = value;
}

uint32_t _Unwind_GetIP(struct _Unwind_Context *ctx) {
    return ctx->ip;
}

void _Unwind_SetIP(struct _Unwind_Context *ctx, uint32_t ip) {
    ctx->ip = ip;
}

uint32_t _Unwind_GetRegionStart(struct _Unwind_Context *ctx) {
    return ctx->region_start;
}

uint32_t _Unwind_GetLanguageSpecificData(struct _Unwind_Context *ctx) {
    return ctx->lsda;
}

/* ========================================================================
 * Assembly glue (defined in unwind_asm.s)
 * ======================================================================== */

/* Saves all registers into ctx, calls the C unwinder, then restores
 * registers from ctx and jumps to the landing pad.
 * Prototype: void _unwind_restore_context(_Unwind_Context *ctx) __attribute__((noreturn));
 */
extern void _unwind_restore_context(_Unwind_Context *ctx) __attribute__((noreturn));

/* ========================================================================
 * Two-phase unwinding
 * ======================================================================== */

/* Internal: perform one phase of unwinding.
 * Returns _URC_HANDLER_FOUND (phase 1) or _URC_INSTALL_CONTEXT (phase 2)
 * when a handler is found, or _URC_END_OF_STACK on failure.
 */
static _Unwind_Reason_Code unwind_phase1(struct _Unwind_Exception *exc,
                                          _Unwind_Context *ctx) {
    /* Phase 1: search for a handler */
    UNWIND_TRACE("[unwind] === Phase 1: searching for handler ===\n");
    _Unwind_Context search_ctx = *ctx;

    for (;;) {
        if (step_frame(&search_ctx) != 0) {
            UNWIND_TRACE("[unwind] Phase 1: step_frame failed -> END_OF_STACK\n");
            return _URC_END_OF_STACK;
        }

        /* Look up FDE for the frame we just stepped INTO (the caller) */
        lookup_frame_info(&search_ctx);

        /* Check if this frame's FDE has a personality */
        if (search_ctx.personality) {
            UNWIND_TRACE("[unwind] Phase 1: calling personality at 0x%X\n",
                         search_ctx.personality);
            _Unwind_Personality_Fn personality =
                (_Unwind_Personality_Fn)(uintptr_t)search_ctx.personality;
            _Unwind_Reason_Code result = personality(
                1, _UA_SEARCH_PHASE, exc->exception_class, exc, &search_ctx);
            UNWIND_TRACE("[unwind] Phase 1: personality returned %d\n", (int)result);
            if (result == _URC_HANDLER_FOUND) {
                /* Record where the handler is */
                exc->private_1 = search_ctx.ip;
                exc->private_2 = 0; /* will be set in phase 2 */
                UNWIND_TRACE("[unwind] Phase 1: HANDLER FOUND at ip=0x%X\n",
                             search_ctx.ip);
                return _URC_HANDLER_FOUND;
            }
            if (result != _URC_CONTINUE_UNWIND)
                return _URC_FATAL_PHASE1_ERROR;
        } else {
            UNWIND_TRACE("[unwind] Phase 1: no personality, continue\n");
        }

        /* No personality or continue: keep searching */
        if (search_ctx.ip == 0) {
            UNWIND_TRACE("[unwind] Phase 1: ip=0 -> END_OF_STACK\n");
            return _URC_END_OF_STACK;
        }
    }
}

static _Unwind_Reason_Code unwind_phase2(struct _Unwind_Exception *exc,
                                          _Unwind_Context *ctx) {
    /* Phase 2: cleanup and transfer control to handler */
    for (;;) {
        if (step_frame(ctx) != 0)
            return _URC_FATAL_PHASE2_ERROR;

        /* Look up FDE for the frame we just stepped INTO (the caller) */
        lookup_frame_info(ctx);

        if (ctx->personality) {
            _Unwind_Action actions = _UA_CLEANUP_PHASE;
            /* Check if this is the handler frame */
            if (ctx->ip == exc->private_1) {
                actions |= _UA_HANDLER_FRAME;
            }

            _Unwind_Personality_Fn personality =
                (_Unwind_Personality_Fn)(uintptr_t)ctx->personality;
            _Unwind_Reason_Code result = personality(
                1, actions, exc->exception_class, exc, ctx);

            if (result == _URC_INSTALL_CONTEXT) {
                /* Personality has set IP to landing pad and GR to exception info.
                 * Transfer control. */
                _unwind_restore_context(ctx);
                /* NOTREACHED */
            }
            if (result != _URC_CONTINUE_UNWIND)
                return _URC_FATAL_PHASE2_ERROR;
        }

        if (ctx->ip == 0)
            return _URC_END_OF_STACK;
    }
}

/* ========================================================================
 * Public API — called from _Unwind_RaiseException_impl after asm saves regs
 * ======================================================================== */

_Unwind_Reason_Code _Unwind_RaiseException_impl(
    struct _Unwind_Exception *exc, _Unwind_Context *ctx) {
    /* Phase 1: search for handler */
    _Unwind_Reason_Code rc = unwind_phase1(exc, ctx);
    if (rc != _URC_HANDLER_FOUND)
        return rc;

    /* Phase 2: cleanup and install handler */
    rc = unwind_phase2(exc, ctx);
    return rc;  /* Should not reach here — phase2 calls _unwind_restore_context */
}

void _Unwind_Resume_impl(struct _Unwind_Exception *exc, _Unwind_Context *ctx) {
    /* Continue phase 2 from a cleanup landing pad */
    _Unwind_Reason_Code rc = unwind_phase2(exc, ctx);
    if (rc != _URC_INSTALL_CONTEXT) {
        /* Fatal: no handler found during resume */
        __builtin_trap();
    }
    /* NOTREACHED — phase2 calls _unwind_restore_context */
}
