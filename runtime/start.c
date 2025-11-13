#include <stdlib.h>

#include "slow32_args.h"

extern int main(int argc, char **argv);

int __slow32_start(void) {
    static char *const empty_argv[] = { NULL };

    int argc = 0;
    char **argv = NULL;

    int fetch_status = __slow32_fetch_args(&argc, &argv);
    if (fetch_status != 0 || argv == NULL) {
        argc = 0;
        argv = (char **)empty_argv;
    }

    int rc = main(argc, argv);

    __slow32_release_args();
    exit(rc);
    return rc;
}
