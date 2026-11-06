// Stub implementation of environment variables for DEBUG mode
// Always returns NULL - no environment support without MMIO

#include <stddef.h>

/* A change the program made (setenv and the rest, posix_env.c) comes first. */
char *(*__env_hook)(const char *name, int *found);
char ***__env_vector;

char *getenv(const char *name) {
    if (__env_hook) {
        int found;
        char *v = __env_hook(name, &found);
        if (found) return v;
    }
    return NULL;
}

char **__slow32_get_environ(void) {
    static char *empty_environ[] = { NULL };
    return empty_environ;
}

void __slow32_release_envp(void) {
    // Nothing to release in stub mode
}
