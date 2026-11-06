/* environ: the environment as POSIX's variable.  Its own object, so a
 * program that never names environ never fetches the environment at start
 * (getenv asks for one variable at a time, envp_mmio.c).  A constructor
 * fills it from the host's (or with an empty list, in libc_debug); setenv
 * and the rest give it a new vector (posix_env.c). */
extern char **__slow32_get_environ(void);
extern char ***__env_vector;

char **environ;

__attribute__((constructor)) static void environ_init(void) {
    environ = __slow32_get_environ();
    __env_vector = &environ;
}
