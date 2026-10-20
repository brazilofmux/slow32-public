/*
 * The small pieces of the C++ run time: static-local guards, the pure and
 * deleted virtual handlers, std::terminate, and operator new/delete over
 * malloc.  Part of libs32cxx.s32a, the runtime's C++ ABI for freestanding
 * C++; a program on libc++ links libc++abi instead, which has its own.
 *
 * Single-threaded.
 */

#include <stdlib.h>

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

/*
 * std::terminate() (_ZSt9terminatev) - what clang's __clang_call_terminate
 * calls when an exception escapes a noexcept function or a destructor
 * during unwinding.  There is no terminate handler to install; it aborts.
 */
void _ZSt9terminatev(void) {
    abort();
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
    /* new T[0] is legal and owes a unique pointer; malloc(0) is NULL here */
    void *p = malloc(size ? size : 1);
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
