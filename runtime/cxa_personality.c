/*
 * SLOW-32 C++ Personality Routine (__gxx_personality_v0)
 *
 * This is the language-specific handler called by the DWARF unwinder during
 * both Phase 1 (search for handler) and Phase 2 (cleanup/transfer).
 *
 * It parses the Language-Specific Data Area (LSDA) in .gcc_except_table
 * to find call sites, landing pads, and type filters for the current frame.
 *
 * LSDA layout (emitted by LLVM):
 *   Header:
 *     uint8_t  LPStart_encoding  (DW_EH_PE_omit = use region_start)
 *     uint8_t  TType_encoding    (DW_EH_PE_absptr for SLOW-32)
 *     uleb128  TType_base_offset (offset from here to end of type table)
 *     uint8_t  call_site_encoding (DW_EH_PE_uleb128)
 *     uleb128  call_site_table_length
 *   Call site table:
 *     For each call site:
 *       encoded  cs_start   (offset from region_start)
 *       encoded  cs_len     (length of call site)
 *       encoded  cs_lp      (landing pad offset from region_start, 0=no LP)
 *       uleb128  cs_action  (1-based index into action table, 0=cleanup)
 *   Action table:
 *     For each action record:
 *       sleb128  ar_filter  (>0 = type filter index, 0 = cleanup, <0 = exception spec)
 *       sleb128  ar_disp    (byte offset to next action, 0 = end of chain)
 *   Type table (indexed backwards from TType_base):
 *     For each type:
 *       pointer  type_info  (absolute 32-bit pointer to std::type_info, 0 = catch-all)
 */

#include <stdint.h>
#include <string.h>
#include "unwind.h"

/* DW_EH_PE encoding constants */
#define DW_EH_PE_absptr  0x00
#define DW_EH_PE_uleb128 0x01
#define DW_EH_PE_udata2  0x02
#define DW_EH_PE_udata4  0x03
#define DW_EH_PE_sleb128 0x09
#define DW_EH_PE_sdata2  0x0A
#define DW_EH_PE_sdata4  0x0B
#define DW_EH_PE_omit    0xFF

/* ========================================================================
 * LEB128 decoders
 * ======================================================================== */

static uint32_t read_uleb128(const uint8_t **pp) {
    uint32_t result = 0;
    int shift = 0;
    uint8_t byte;
    do {
        byte = **pp;
        (*pp)++;
        result |= (uint32_t)(byte & 0x7F) << shift;
        shift += 7;
    } while (byte & 0x80);
    return result;
}

static int32_t read_sleb128(const uint8_t **pp) {
    int32_t result = 0;
    int shift = 0;
    uint8_t byte;
    do {
        byte = **pp;
        (*pp)++;
        result |= (int32_t)(byte & 0x7F) << shift;
        shift += 7;
    } while (byte & 0x80);
    /* Sign extend */
    if (shift < 32 && (byte & 0x40))
        result |= -(1 << shift);
    return result;
}

/* ========================================================================
 * Encoded pointer reader
 * ======================================================================== */

static uint32_t read_encoded_ptr(const uint8_t **pp, uint8_t encoding) {
    if (encoding == DW_EH_PE_omit) return 0;

    uint32_t result;
    uint8_t format = encoding & 0x0F;

    switch (format) {
        case DW_EH_PE_absptr: {
            uint32_t val;
            memcpy(&val, *pp, 4);
            *pp += 4;
            result = val;
            break;
        }
        case DW_EH_PE_uleb128:
            result = read_uleb128(pp);
            break;
        case DW_EH_PE_udata2: {
            uint16_t val;
            memcpy(&val, *pp, 2);
            *pp += 2;
            result = val;
            break;
        }
        case DW_EH_PE_udata4: {
            uint32_t val;
            memcpy(&val, *pp, 4);
            *pp += 4;
            result = val;
            break;
        }
        case DW_EH_PE_sleb128:
            result = (uint32_t)read_sleb128(pp);
            break;
        case DW_EH_PE_sdata2: {
            int16_t val;
            memcpy(&val, *pp, 2);
            *pp += 2;
            result = (uint32_t)(int32_t)val;
            break;
        }
        case DW_EH_PE_sdata4: {
            int32_t val;
            memcpy(&val, *pp, 4);
            *pp += 4;
            result = (uint32_t)val;
            break;
        }
        default:
            result = 0;
            break;
    }

    return result;
}

/* ========================================================================
 * Type matching
 *
 * For SLOW-32, type matching uses simple pointer equality on
 * std::type_info pointers. A NULL type_info means catch-all.
 * ======================================================================== */

/* Forward-declare __cxa_exception for accessing thrown type_info */
struct __cxa_exception {
    void *exceptionType;
    void (*exceptionDestructor)(void *);
    void (*unexpectedHandler)(void);
    void (*terminateHandler)(void);
    struct __cxa_exception *nextException;
    int handlerCount;
    int handlerSwitchValue;
    const uint8_t *actionRecord;
    const uint8_t *languageSpecificData;
    void *catchTemp;
    void *adjustedPtr;
    struct _Unwind_Exception unwindHeader;
};

static void *get_thrown_type(struct _Unwind_Exception *exc) {
    struct __cxa_exception *header =
        (struct __cxa_exception *)((char *)exc -
            __builtin_offsetof(struct __cxa_exception, unwindHeader));
    return header->exceptionType;
}

static int type_matches(void *thrown_type, void *catch_type) {
    /* catch(...) — catch_type is NULL */
    if (catch_type == 0) return 1;
    /* Exact pointer match on type_info */
    return thrown_type == catch_type;
}

/* ========================================================================
 * __gxx_personality_v0
 *
 * The C++ personality routine called by the DWARF unwinder.
 * ======================================================================== */

_Unwind_Reason_Code __gxx_personality_v0(
    int version,
    _Unwind_Action actions,
    _Unwind_Exception_Class exception_class,
    struct _Unwind_Exception *exception_object,
    struct _Unwind_Context *context)
{
    (void)exception_class; /* We handle all exceptions the same way */

    if (version != 1)
        return _URC_FATAL_PHASE1_ERROR;

    /* Get LSDA pointer for this frame */
    uint32_t lsda_addr = _Unwind_GetLanguageSpecificData(context);
    if (lsda_addr == 0)
        return _URC_CONTINUE_UNWIND; /* No LSDA = no handlers in this frame */

    const uint8_t *lsda = (const uint8_t *)(uintptr_t)lsda_addr;
    uint32_t region_start = _Unwind_GetRegionStart(context);
    uint32_t ip = _Unwind_GetIP(context);

    /* IP points to the instruction AFTER the call, so subtract 1
     * to get an address within the call site range */
    uint32_t ip_offset = ip - region_start - 1;

    /* ----------------------------------------------------------------
     * Parse LSDA header
     * ---------------------------------------------------------------- */

    const uint8_t *p = lsda;

    /* LPStart encoding */
    uint8_t lpstart_enc = *p++;
    uint32_t lpstart = region_start;
    if (lpstart_enc != DW_EH_PE_omit) {
        lpstart = read_encoded_ptr(&p, lpstart_enc);
    }

    /* TType encoding and base */
    uint8_t ttype_enc = *p++;
    const uint8_t *ttype_base = 0;
    if (ttype_enc != DW_EH_PE_omit) {
        uint32_t ttype_base_offset = read_uleb128(&p);
        ttype_base = p + ttype_base_offset;
    }

    /* Call site encoding */
    uint8_t cs_enc = *p++;
    uint32_t cs_table_len = read_uleb128(&p);
    const uint8_t *cs_table = p;
    const uint8_t *cs_table_end = cs_table + cs_table_len;
    const uint8_t *action_table = cs_table_end;

    /* ----------------------------------------------------------------
     * Search call site table for current IP
     * ---------------------------------------------------------------- */

    uint32_t cs_lp = 0;
    uint32_t cs_action = 0;
    int found = 0;

    p = cs_table;
    while (p < cs_table_end) {
        uint32_t cs_start = read_encoded_ptr(&p, cs_enc);
        uint32_t cs_len = read_encoded_ptr(&p, cs_enc);
        uint32_t cs_landing_pad = read_encoded_ptr(&p, cs_enc);
        uint32_t cs_action_idx = read_uleb128(&p);

        if (ip_offset >= cs_start && ip_offset < cs_start + cs_len) {
            cs_lp = cs_landing_pad;
            cs_action = cs_action_idx;
            found = 1;
            break;
        }
    }

    if (!found)
        return _URC_CONTINUE_UNWIND; /* IP not in any call site */

    if (cs_lp == 0)
        return _URC_CONTINUE_UNWIND; /* No landing pad for this call site */

    /* Compute actual landing pad address */
    uint32_t landing_pad = lpstart + cs_lp;

    /* ----------------------------------------------------------------
     * Check action table for handler type matching
     * ---------------------------------------------------------------- */

    if (cs_action == 0) {
        /* Action index 0 = cleanup only (no catch handlers) */
        if (actions & _UA_SEARCH_PHASE) {
            /* Phase 1: cleanups are not handlers */
            return _URC_CONTINUE_UNWIND;
        }
        /* Phase 2: install cleanup landing pad */
        _Unwind_SetGR(context, 3, (uint32_t)(uintptr_t)exception_object);
        _Unwind_SetGR(context, 4, 0); /* selector = 0 for cleanup */
        _Unwind_SetIP(context, landing_pad);
        return _URC_INSTALL_CONTEXT;
    }

    /* Walk the action chain */
    void *thrown_type = get_thrown_type(exception_object);
    const uint8_t *action_record = action_table + cs_action - 1;

    for (;;) {
        const uint8_t *ap = action_record;
        int32_t ar_filter = read_sleb128(&ap);
        int32_t ar_disp = read_sleb128(&ap);

        if (ar_filter > 0) {
            /* Positive filter: catch handler. Index into type table. */
            /* Type table is indexed backwards from ttype_base, each entry is
             * a pointer (4 bytes for DW_EH_PE_absptr) */
            if (ttype_base) {
                uint32_t ttype_size = 4; /* DW_EH_PE_absptr = 4 bytes */
                const uint8_t *type_entry = ttype_base - ar_filter * ttype_size;
                uint32_t catch_type_addr;
                memcpy(&catch_type_addr, type_entry, 4);
                void *catch_type = (void *)(uintptr_t)catch_type_addr;

                if (type_matches(thrown_type, catch_type)) {
                    /* Found a matching handler */
                    if (actions & _UA_SEARCH_PHASE) {
                        return _URC_HANDLER_FOUND;
                    }
                    /* Phase 2: install handler */
                    _Unwind_SetGR(context, 3, (uint32_t)(uintptr_t)exception_object);
                    _Unwind_SetGR(context, 4, (uint32_t)ar_filter); /* selector > 0 */
                    _Unwind_SetIP(context, landing_pad);
                    return _URC_INSTALL_CONTEXT;
                }
            }
        } else if (ar_filter == 0) {
            /* Filter 0 = cleanup action. Only run in Phase 2. */
            if (actions & _UA_SEARCH_PHASE) {
                /* Keep looking for a real handler */
            } else {
                /* Phase 2: install cleanup */
                _Unwind_SetGR(context, 3, (uint32_t)(uintptr_t)exception_object);
                _Unwind_SetGR(context, 4, 0);
                _Unwind_SetIP(context, landing_pad);
                return _URC_INSTALL_CONTEXT;
            }
        }
        /* ar_filter < 0 = exception specification (not used by modern C++) */

        if (ar_disp == 0) break; /* End of action chain */
        action_record += ar_disp;
    }

    return _URC_CONTINUE_UNWIND;
}
