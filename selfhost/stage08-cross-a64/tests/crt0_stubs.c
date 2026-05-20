/* crt0_stubs.c — no-op libc stubs so crt0 + main can link without libc.
 *
 * crt0 calls __save_envp(envp) before main; we just stash the pointer
 * (and offer it via __crt0_test_envp for any test that wants to inspect).
 */

char **__crt0_test_envp;

void __save_envp(char **envp) {
    __crt0_test_envp = envp;
}
