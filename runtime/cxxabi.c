/*
 * Exit-time destruction: atexit, __cxa_atexit, __cxa_finalize, __dso_handle.
 *
 * In the Itanium C++ ABI these are the C library's, called by exit() and by
 * the code the compiler emits for objects with static storage, so they stay
 * in libc for every program.  The rest of the C++ run time (guards, pure
 * virtuals, new/delete, exceptions, RTTI) is libs32cxx.s32a, cxa_support.c
 * and cxa_*.c -- or libc++abi for programs on libc++.
 *
 * Single-threaded: the one thread's thread_local destructors
 * (__cxa_thread_atexit_impl, libc++abi's __cxa_thread_atexit) run at exit,
 * before the static ones, as [basic.start.term] orders them.
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

/* The first ATEXIT_STATIC registrations need no allocation (the standard
 * asks for 32 atexit functions, and they must not fail for want of
 * memory); past them the list grows on the heap.  A large C++ program
 * registers a destructor per static object, far more than 64. */
#define ATEXIT_STATIC 64

struct atexit_entry {
    void (*destructor)(void *);
    void *arg;
    void *dso_handle;
};

struct atexit_list {
    struct atexit_entry *entries;
    int count, cap;
};

static struct atexit_entry atexit_static[ATEXIT_STATIC];
static struct atexit_list exit_list = { atexit_static, 0, ATEXIT_STATIC };
static struct atexit_entry tls_static[16];
static struct atexit_list tls_list = { tls_static, 0, 16 };

static int list_add(struct atexit_list *l, struct atexit_entry *first, void (*fn)(void *),
                    void *arg, void *dso) {
    if (l->count == l->cap) {
        int ncap = l->cap * 2;
        struct atexit_entry *n;
        if (l->entries == first) {
            n = malloc((size_t)ncap * sizeof *n);
            if (n) for (int i = 0; i < l->count; i++) n[i] = first[i];
        } else {
            n = realloc(l->entries, (size_t)ncap * sizeof *n);
        }
        if (!n) return -1;
        l->entries = n;
        l->cap = ncap;
    }
    l->entries[l->count].destructor = fn;
    l->entries[l->count].arg = arg;
    l->entries[l->count].dso_handle = dso;
    l->count++;
    return 0;
}

/* the last registered first; one that registers another on its way out
 * has it run next */
static void list_run(struct atexit_list *l) {
    while (l->count > 0) {
        struct atexit_entry e = l->entries[--l->count];
        if (e.destructor) e.destructor(e.arg);
    }
}

/*
 * __cxa_atexit - Register a destructor for a static object
 *
 * Called by compiler-generated code when a static object with a
 * destructor is initialized.
 */
int __cxa_atexit(void (*destructor)(void *), void *arg, void *dso_handle) {
    return list_add(&exit_list, atexit_static, destructor, arg, dso_handle);
}

/*
 * __cxa_thread_atexit_impl - Register a destructor for a thread_local
 * object.  The one thread ends when the program does, so these run at
 * exit -- first, before the static destructors.
 */
int __cxa_thread_atexit_impl(void (*destructor)(void *), void *obj, void *dso_symbol) {
    return list_add(&tls_list, tls_static, destructor, obj, dso_symbol);
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
        /* exit: the thread's thread_local objects, then the statics */
        list_run(&tls_list);
        list_run(&exit_list);
        return;
    }
    for (int i = exit_list.count - 1; i >= 0; i--) {
        struct atexit_entry *e = &exit_list.entries[i];
        if (e->dso_handle == dso_handle && e->destructor) {
            void (*fn)(void *) = e->destructor;
            e->destructor = 0;  /* Mark as called */
            fn(e->arg);
        }
    }
}
