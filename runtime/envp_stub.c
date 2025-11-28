// Stub implementation of environment variables for DEBUG mode
// Always returns NULL - no environment support without MMIO

#include <stddef.h>

char *getenv(const char *name) {
    (void)name;
    return NULL;
}

char **__slow32_get_environ(void) {
    static char *empty_environ[] = { NULL };
    return empty_environ;
}

void __slow32_release_envp(void) {
    // Nothing to release in stub mode
}
