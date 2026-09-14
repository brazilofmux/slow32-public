/* crt0_stubs.c — no-op libc stubs so crt0 + main can link without libc.
 *
 * crt0 calls __save_envp(envp) before main; we just stash the pointer
 * (and offer it via __crt0_test_envp for any test that wants to inspect).
 */

char **__crt0_test_envp;

void __save_envp(char **envp) {
    __crt0_test_envp = envp;
}

/* The compiler emits calls to memcpy/memset for aggregate copies and
 * zero-initialisations over 32 bytes (d8de8ef1), so freestanding tests
 * need the two of them here.  Byte loops on purpose: these are the
 * reference, not the libc. */
typedef unsigned long size_t;
void *memcpy(void *d, const void *s, size_t n) {
    unsigned char *dp = d; const unsigned char *sp = s;
    while (n--) *dp++ = *sp++;
    return d;
}
void *memset(void *d, int c, size_t n) {
    unsigned char *dp = d;
    while (n--) *dp++ = (unsigned char)c;
    return d;
}
void *memmove(void *d, const void *s, size_t n) {
    unsigned char *dp = d; const unsigned char *sp = s;
    if (dp < sp) { while (n--) *dp++ = *sp++; }
    else { dp += n; sp += n; while (n--) *--dp = *--sp; }
    return d;
}
