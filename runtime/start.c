#include <stdlib.h>

#include "slow32_args.h"

extern int main(int argc, char **argv);

// C++ constructor/destructor support
typedef void (*ctor_func)(void);

// Linker-provided symbols for .init_array section
extern ctor_func __init_array_start[];
extern ctor_func __init_array_end[];

// From cxxabi.c
extern void __cxa_finalize(void *dso_handle);

// Call all global constructors
static void __call_constructors(void) {
    for (ctor_func *p = __init_array_start; p < __init_array_end; p++) {
        if (*p) {
            (*p)();
        }
    }
}

int __slow32_start(void) {
    static char *const empty_argv[] = { NULL };

    int argc = 0;
    char **argv = NULL;

    // Call C++ global constructors before main
    __call_constructors();

    int fetch_status = __slow32_fetch_args(&argc, &argv);
    if (fetch_status != 0 || argv == NULL) {
        argc = 0;
        argv = (char **)empty_argv;
    }

    int rc = main(argc, argv);

    // Call C++ global destructors after main
    __cxa_finalize(0);

    __slow32_release_args();
    exit(rc);
    return rc;
}
