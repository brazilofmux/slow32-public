/*
 * SLOW-32 C++ Exception Handling Runtime
 *
 * Implements the C++ exception ABI functions:
 * - __cxa_allocate_exception / __cxa_free_exception
 * - __cxa_throw
 * - __cxa_begin_catch / __cxa_end_catch
 * - __cxa_rethrow
 *
 * Based on the Itanium C++ ABI exception handling specification.
 * Single-threaded implementation for SLOW-32.
 */

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "unwind.h"

/* ========================================================================
 * __cxa_exception header
 *
 * Prepended before every thrown exception object in memory.
 * The user-visible exception pointer points AFTER this header.
 * ======================================================================== */

struct __cxa_exception {
    /* Type info for the thrown object */
    void *exceptionType;        /* std::type_info* */

    /* Destructor for the thrown object (may be NULL) */
    void (*exceptionDestructor)(void *);

    /* Handler that caught this exception (for rethrow) */
    void (*unexpectedHandler)(void);

    /* Terminate handler */
    void (*terminateHandler)(void);

    /* Link to next exception in the chain (nested exceptions) */
    struct __cxa_exception *nextException;

    /* Number of active catch handlers for this exception */
    int handlerCount;

    /* Handler switch value from personality (for rethrow) */
    int handlerSwitchValue;

    /* Pointer to the action record in LSDA (for rethrow) */
    const uint8_t *actionRecord;

    /* Pointer to LSDA (for rethrow) */
    const uint8_t *languageSpecificData;

    /* IP to resume at after catch (for rethrow) */
    void *catchTemp;

    /* Adjusted pointer for the caught exception (cast-to-base) */
    void *adjustedPtr;

    /* Unwind header — must be LAST (Itanium ABI requirement) */
    struct _Unwind_Exception unwindHeader;
};

/* C++ exception class: "GNUCC++\0" encoded as 8 bytes */
static const _Unwind_Exception_Class gnu_cxx_exception_class =
    ((uint64_t)'G' << 56) | ((uint64_t)'N' << 48) |
    ((uint64_t)'U' << 40) | ((uint64_t)'C' << 32) |
    ((uint64_t)'C' << 24) | ((uint64_t)'+' << 16) |
    ((uint64_t)'+' <<  8) | ((uint64_t)'\0');

/* ========================================================================
 * Per-thread globals (single-threaded on SLOW-32)
 * ======================================================================== */

struct __cxa_eh_globals {
    struct __cxa_exception *caughtExceptions;
    unsigned int uncaughtExceptions;
};

static struct __cxa_eh_globals eh_globals = { 0, 0 };

struct __cxa_eh_globals *__cxa_get_globals(void) {
    return &eh_globals;
}

struct __cxa_eh_globals *__cxa_get_globals_fast(void) {
    return &eh_globals;
}

/* ========================================================================
 * Helper: convert between exception object pointer and header
 * ======================================================================== */

static struct __cxa_exception *exception_from_object(void *obj) {
    return (struct __cxa_exception *)((char *)obj - sizeof(struct __cxa_exception));
}

static void *object_from_exception(struct __cxa_exception *exc) {
    return (void *)((char *)exc + sizeof(struct __cxa_exception));
}

/* ========================================================================
 * Exception cleanup callback (called by unwinder if foreign exception)
 * ======================================================================== */

static void exception_cleanup(_Unwind_Reason_Code reason,
                               struct _Unwind_Exception *exc) {
    (void)reason;
    struct __cxa_exception *header =
        (struct __cxa_exception *)((char *)exc -
            __builtin_offsetof(struct __cxa_exception, unwindHeader));
    void *obj = object_from_exception(header);
    if (header->exceptionDestructor)
        header->exceptionDestructor(obj);
    free(header);
}

/* ========================================================================
 * __cxa_allocate_exception / __cxa_free_exception
 * ======================================================================== */

void *__cxa_allocate_exception(unsigned int thrown_size) {
    /* Allocate header + thrown object */
    unsigned int total = sizeof(struct __cxa_exception) + thrown_size;
    struct __cxa_exception *header = (struct __cxa_exception *)malloc(total);
    if (!header) {
        /* Out of memory during throw — terminate */
        __builtin_trap();
    }
    memset(header, 0, sizeof(struct __cxa_exception));
    /* Return pointer past the header (where the user's object lives) */
    return object_from_exception(header);
}

void __cxa_free_exception(void *thrown_object) {
    struct __cxa_exception *header = exception_from_object(thrown_object);
    free(header);
}

/* ========================================================================
 * __cxa_throw
 * ======================================================================== */

void __cxa_throw(void *thrown_object, void *tinfo, void (*dest)(void *)) {
    struct __cxa_exception *header = exception_from_object(thrown_object);

    header->exceptionType = tinfo;
    header->exceptionDestructor = dest;
    header->unexpectedHandler = 0;
    header->terminateHandler = 0;

    /* Set up the unwind header */
    header->unwindHeader.exception_class = gnu_cxx_exception_class;
    header->unwindHeader.exception_cleanup = exception_cleanup;

    eh_globals.uncaughtExceptions++;

    /* Launch the unwinder */
    _Unwind_Reason_Code rc = _Unwind_RaiseException(&header->unwindHeader);

    /* If we get here, no handler was found — terminate */
    (void)rc;
    __builtin_trap();
}

/* ========================================================================
 * __cxa_begin_catch / __cxa_end_catch
 * ======================================================================== */

void *__cxa_begin_catch(void *exc_obj) {
    /* exc_obj is the _Unwind_Exception pointer passed in r3 by the landing pad.
     * We need to get back to the __cxa_exception header. */
    struct _Unwind_Exception *unwind_exc = (struct _Unwind_Exception *)exc_obj;
    struct __cxa_exception *header =
        (struct __cxa_exception *)((char *)unwind_exc -
            __builtin_offsetof(struct __cxa_exception, unwindHeader));

    /* Push onto caught exceptions stack */
    header->nextException = eh_globals.caughtExceptions;
    eh_globals.caughtExceptions = header;
    header->handlerCount++;
    eh_globals.uncaughtExceptions--;

    /* Return the thrown object pointer (past the header) */
    return object_from_exception(header);
}

void __cxa_end_catch(void) {
    struct __cxa_exception *header = eh_globals.caughtExceptions;
    if (!header) return;

    header->handlerCount--;
    if (header->handlerCount == 0) {
        /* Remove from caught stack */
        eh_globals.caughtExceptions = header->nextException;

        /* Destroy the exception object */
        void *obj = object_from_exception(header);
        if (header->exceptionDestructor)
            header->exceptionDestructor(obj);
        free(header);
    }
}

/* ========================================================================
 * __cxa_rethrow
 * ======================================================================== */

void __cxa_rethrow(void) {
    struct __cxa_exception *header = eh_globals.caughtExceptions;
    if (!header) {
        /* No exception to rethrow — terminate */
        __builtin_trap();
    }

    /* Remove from caught stack and re-throw */
    eh_globals.caughtExceptions = header->nextException;
    eh_globals.uncaughtExceptions++;

    _Unwind_Resume(&header->unwindHeader);
    /* NOTREACHED */
    __builtin_trap();
}

/* ========================================================================
 * __cxa_current_exception_type — used by catch-all handlers
 * ======================================================================== */

void *__cxa_current_exception_type(void) {
    struct __cxa_exception *header = eh_globals.caughtExceptions;
    if (!header) return 0;
    return header->exceptionType;
}
