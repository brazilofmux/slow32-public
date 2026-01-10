/*
 * SLOW-32 C++ ABI Runtime Support
 *
 * Minimal implementation of C++ runtime functions required for:
 * - Global object destruction (__cxa_atexit, __dso_handle)
 * - Static local initialization guards (__cxa_guard_*)
 * - Pure virtual function handling (__cxa_pure_virtual)
 * - Dynamic memory allocation (operator new/delete)
 *
 * This is a freestanding implementation for embedded use.
 * Thread safety is not implemented (single-threaded assumption).
 */

#include <stdlib.h>

/* ========================================================================
 * DSO Handle
 * ======================================================================== */

/*
 * __dso_handle is used by __cxa_atexit to identify which shared object
 * registered a destructor. For static linking, this is just a marker.
 */
void *__dso_handle = 0;

/* ========================================================================
 * Static Destructor Registration
 * ======================================================================== */

#define ATEXIT_MAX 32

struct atexit_entry {
    void (*destructor)(void *);
    void *arg;
    void *dso_handle;
};

static struct atexit_entry atexit_list[ATEXIT_MAX];
static int atexit_count = 0;

/*
 * __cxa_atexit - Register a destructor for a static object
 *
 * Called by compiler-generated code when a static object with a
 * destructor is initialized.
 */
int __cxa_atexit(void (*destructor)(void *), void *arg, void *dso_handle) {
    if (atexit_count >= ATEXIT_MAX) {
        return -1;  /* No space left */
    }
    atexit_list[atexit_count].destructor = destructor;
    atexit_list[atexit_count].arg = arg;
    atexit_list[atexit_count].dso_handle = dso_handle;
    atexit_count++;
    return 0;
}

/*
 * __cxa_finalize - Run registered destructors
 *
 * If dso_handle is NULL, runs all destructors.
 * Otherwise, runs only destructors registered by that DSO.
 * Called from exit() or when a shared library is unloaded.
 */
void __cxa_finalize(void *dso_handle) {
    for (int i = atexit_count - 1; i >= 0; i--) {
        if (dso_handle == 0 || atexit_list[i].dso_handle == dso_handle) {
            if (atexit_list[i].destructor) {
                atexit_list[i].destructor(atexit_list[i].arg);
                atexit_list[i].destructor = 0;  /* Mark as called */
            }
        }
    }
}

/* ========================================================================
 * Static Local Variable Guards (Thread-Safe Initialization)
 * ======================================================================== */

/*
 * Guard variables are 64 bits. The first byte indicates initialization status:
 *   0 = not initialized
 *   1 = initialized
 *
 * In a multi-threaded environment, additional bytes would be used for
 * locking. This implementation is single-threaded only.
 */

/*
 * __cxa_guard_acquire - Check if static local needs initialization
 *
 * Returns 1 if initialization should proceed, 0 if already initialized.
 */
int __cxa_guard_acquire(long long *guard) {
    char *guard_byte = (char *)guard;
    if (*guard_byte != 0) {
        return 0;  /* Already initialized */
    }
    return 1;  /* Needs initialization */
}

/*
 * __cxa_guard_release - Mark static local as initialized
 */
void __cxa_guard_release(long long *guard) {
    char *guard_byte = (char *)guard;
    *guard_byte = 1;
}

/*
 * __cxa_guard_abort - Initialization failed (e.g., exception thrown)
 *
 * Since we don't support exceptions, this is a no-op.
 */
void __cxa_guard_abort(long long *guard) {
    (void)guard;
}

/* ========================================================================
 * Pure Virtual Function Handler
 * ======================================================================== */

/*
 * __cxa_pure_virtual - Called when a pure virtual function is invoked
 *
 * This should never happen in correct code. It indicates a bug such as
 * calling a virtual function from a constructor/destructor.
 */
void __cxa_pure_virtual(void) {
    /* Could print error message if we had I/O */
    __builtin_trap();
}

/*
 * __cxa_deleted_virtual - Called when a deleted virtual function is invoked
 */
void __cxa_deleted_virtual(void) {
    __builtin_trap();
}

/* ========================================================================
 * Operator New / Delete
 *
 * These use C++ mangled names. We use asm labels to define them.
 * Mangled names:
 *   _Znwj  = operator new(unsigned int)
 *   _Znaj  = operator new[](unsigned int)
 *   _ZdlPv = operator delete(void*)
 *   _ZdlPvj = operator delete(void*, unsigned int)
 *   _ZdaPv = operator delete[](void*)
 *   _ZdaPvj = operator delete[](void*, unsigned int)
 * ======================================================================== */

/* operator new(unsigned int) */
void *_Znwj(unsigned int size) {
    void *p = malloc(size);
    if (!p) {
        /* No exceptions, so trap on OOM */
        __builtin_trap();
    }
    return p;
}

/* operator new[](unsigned int) */
void *_Znaj(unsigned int size) {
    return _Znwj(size);
}

/* operator delete(void*) */
void _ZdlPv(void *ptr) {
    free(ptr);
}

/* operator delete(void*, unsigned int) - sized delete */
void _ZdlPvj(void *ptr, unsigned int size) {
    (void)size;
    free(ptr);
}

/* operator delete[](void*) */
void _ZdaPv(void *ptr) {
    free(ptr);
}

/* operator delete[](void*, unsigned int) - sized delete[] */
void _ZdaPvj(void *ptr, unsigned int size) {
    (void)size;
    free(ptr);
}
