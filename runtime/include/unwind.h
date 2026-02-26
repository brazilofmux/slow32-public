/*
 * SLOW-32 Itanium ABI Unwind Interface
 *
 * Minimal implementation of the Level I and Level II unwinding APIs
 * as specified by the Itanium C++ ABI exception handling specification.
 * Used by the C++ exception runtime (__cxa_throw, __gxx_personality_v0).
 */

#ifndef _UNWIND_H
#define _UNWIND_H

#include <stdint.h>

/* Reason codes returned by _Unwind functions */
typedef enum {
    _URC_NO_REASON                = 0,
    _URC_FOREIGN_EXCEPTION_CAUGHT = 1,
    _URC_FATAL_PHASE2_ERROR       = 2,
    _URC_FATAL_PHASE1_ERROR       = 3,
    _URC_NORMAL_STOP              = 4,
    _URC_END_OF_STACK             = 5,
    _URC_HANDLER_FOUND            = 6,
    _URC_INSTALL_CONTEXT          = 7,
    _URC_CONTINUE_UNWIND          = 8
} _Unwind_Reason_Code;

/* Action flags passed to personality routine */
typedef int _Unwind_Action;
#define _UA_SEARCH_PHASE  1
#define _UA_CLEANUP_PHASE 2
#define _UA_HANDLER_FRAME 4
#define _UA_FORCE_UNWIND  8

/* Exception class: 8-byte vendor+language identifier */
typedef uint64_t _Unwind_Exception_Class;

/* Forward declarations */
struct _Unwind_Exception;
struct _Unwind_Context;

/* Exception cleanup function */
typedef void (*_Unwind_Exception_Cleanup_Fn)(
    _Unwind_Reason_Code reason,
    struct _Unwind_Exception *exc);

/* Exception header (prepended to all thrown exceptions) */
struct _Unwind_Exception {
    _Unwind_Exception_Class exception_class;
    _Unwind_Exception_Cleanup_Fn exception_cleanup;
    /* Private fields used by the unwinder */
    uint32_t private_1;  /* landing pad IP */
    uint32_t private_2;  /* handler switch value */
};

/* Personality routine type */
typedef _Unwind_Reason_Code (*_Unwind_Personality_Fn)(
    int version,
    _Unwind_Action actions,
    _Unwind_Exception_Class exception_class,
    struct _Unwind_Exception *exception_object,
    struct _Unwind_Context *context);

/* Unwinder entry points */
_Unwind_Reason_Code _Unwind_RaiseException(struct _Unwind_Exception *exc);
void _Unwind_Resume(struct _Unwind_Exception *exc) __attribute__((noreturn));

/* Context accessors (used by personality routines) */
uint32_t _Unwind_GetGR(struct _Unwind_Context *ctx, int reg);
void _Unwind_SetGR(struct _Unwind_Context *ctx, int reg, uint32_t value);
uint32_t _Unwind_GetIP(struct _Unwind_Context *ctx);
void _Unwind_SetIP(struct _Unwind_Context *ctx, uint32_t ip);
uint32_t _Unwind_GetRegionStart(struct _Unwind_Context *ctx);
uint32_t _Unwind_GetLanguageSpecificData(struct _Unwind_Context *ctx);

#endif /* _UNWIND_H */
