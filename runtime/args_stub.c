#include <stddef.h>

#include "slow32_args.h"

int __slow32_fetch_args(int *argc_out, char ***argv_out) {
    if (argc_out) {
        *argc_out = 0;
    }
    if (argv_out) {
        static char *default_argv[] = { NULL };
        *argv_out = default_argv;
    }
    return 0;
}

void __slow32_release_args(void) {
    // Nothing to do for stub implementation
}
