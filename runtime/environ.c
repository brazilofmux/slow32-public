/* environ: the environment as POSIX's variable.  Its own object, so a
 * program that never names environ never fetches the environment at start
 * (getenv asks for one variable at a time, envp_mmio.c).  A constructor
 * fills it from the host's (or with an empty list, in libc_debug); the
 * library has no setenv, so it does not change after. */
extern char **__slow32_get_environ(void);

char **environ;

__attribute__((constructor)) static void environ_init(void) {
    environ = __slow32_get_environ();
}
