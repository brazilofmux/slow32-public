/* s32vec.h -- grow-on-demand arrays for the self-hosted tools.
 *
 * The tools were built on fixed arrays sized by #define, which is fine
 * until the compiler that feeds them grows.  It has: s12cc.c crossed
 * the linker's 2048-symbol ceiling by 5 symbols simply by gaining a
 * few functions, and the assembler's text buffer and label table were
 * both about one compiler-doubling from the same fate (52% and 48%
 * used).  Capacity should expand, not be rationed.
 *
 * There is a second, less obvious win.  The fixed arrays lived in BSS,
 * and crt0 memsets BSS before main -- the ILP trace measured 83 MILLION
 * instructions clearing 16.7 MB at every compiler startup.  Arrays that
 * start empty and grow to what the input actually needs make the tools
 * start faster as well as scale further.
 *
 * Deliberately not an arena: these are homogeneous indexed tables that
 * want realloc, not many small same-lifetime objects.  (The compiler's
 * AST/HIR nodes are the arena case.)
 *
 * Dialect notes: char pointers and ints only, no void pointers, no
 * pointer casts, no macros with arguments -- the frozen bootstrap
 * compilers take it as is.  (An int-array variant needs typed
 * malloc/realloc aliases in libc; added when the int tables convert.)
 * The grown tail is ZEROED, because every caller inherited BSS
 * zero-initialisation semantics and quietly relies on it.
 */
#ifndef S32VEC_H
#define S32VEC_H

/* Refuse absurd requests rather than wrapping into a small allocation:
 * these counts are multiplied by an element size below. */
#define SV_MAX_ELEMS 268435456

static void sv_oom(char *what) {
    fdputs("s32: out of memory growing ", 2);
    fdputs(what, 2);
    fdputc('\n', 2);
    exit(1);
}

/* Grow *cap (in ELEMENTS) to at least `need`, doubling.  Returns the
 * (possibly moved) base pointer; the new tail is zeroed.  `esz` is the
 * element size in bytes.  A no-op when the capacity already suffices,
 * so callers can call it on every append. */
static char *sv_grow(char *base, int *cap, int need, int esz, char *what) {
    int nc;
    char *nb;
    int i;

    if (need <= *cap) return base;
    if (need > SV_MAX_ELEMS) sv_oom(what);
    nc = *cap;
    if (nc < 64) nc = 64;
    while (nc < need) {
        if (nc > SV_MAX_ELEMS / 2) { nc = need; break; }
        nc = nc * 2;
    }
    if (base == 0) nb = malloc(nc * esz);
    else nb = realloc(base, nc * esz);
    if (nb == 0) sv_oom(what);
    /* Zero the new tail: callers inherited BSS semantics. */
    i = (*cap) * esz;
    while (i < nc * esz) { nb[i] = 0; i = i + 1; }
    *cap = nc;
    return nb;
}

#endif /* S32VEC_H */
