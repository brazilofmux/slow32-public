/*
 * Exit-time destruction: atexit, __cxa_atexit, __cxa_finalize, __dso_handle.
 *
 * In the Itanium C++ ABI these are the C library's, called by exit() and by
 * the code the compiler emits for objects with static storage, so they stay
 * in libc for every program.  The rest of the C++ run time (guards, pure
 * virtuals, new/delete, exceptions, RTTI) is libs32cxx.s32a, cxa_support.c
 * and cxa_*.c -- or libc++abi for programs on libc++.
 *
 * Single-threaded.
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

#define ATEXIT_MAX 64       /* the standard asks for 32 atexit functions; static destructors share the list */

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
 * atexit - C's registration: the same list, so functions and static
 * destructors run in the reverse of the order they were registered in,
 * interleaved as they were registered.  The function takes no argument;
 * it is called with one it does not look at.
 */
int atexit(void (*function)(void)) {
    return __cxa_atexit((void (*)(void *))function, 0, 0);
}

/*
 * __cxa_finalize - Run registered destructors
 *
 * If dso_handle is NULL, runs all destructors.
 * Otherwise, runs only destructors registered by that DSO.
 * Called from exit() or when a shared library is unloaded.
 */
void __cxa_finalize(void *dso_handle) {
    if (dso_handle == 0) {
        /* exit: the last registered first -- and one that registers
         * another on its way out has it run next */
        while (atexit_count > 0) {
            struct atexit_entry e = atexit_list[--atexit_count];
            if (e.destructor) e.destructor(e.arg);
        }
        return;
    }
    for (int i = atexit_count - 1; i >= 0; i--) {
        if (dso_handle == 0 || atexit_list[i].dso_handle == dso_handle) {
            if (atexit_list[i].destructor) {
                atexit_list[i].destructor(atexit_list[i].arg);
                atexit_list[i].destructor = 0;  /* Mark as called */
            }
        }
    }
}
