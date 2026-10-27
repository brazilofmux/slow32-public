/* COPIED from selfhost/src/hir_alias.h at 694d26f2 (see hir.h for the ruling). */
/* hir_alias.h -- what an address names, and whether two accesses may
 * touch the same bytes.
 *
 * The one place the optimizer answers memory questions
 * (docs/plans/memory-model.md).  Forwarding, dead-store elimination,
 * the late scalar replacement and the dead-slot pass each kept a
 * resolver of their own; the last two differed only in whether a COPY
 * is an address step, and a pointer-offset rule written for one pass
 * was borrowed by another with a flag.  Everything here is pure: it
 * reads the instruction arrays and writes only its own tables.
 *
 * Addresses are instruction ids.  al_resolve follows the steps that
 * keep an address in the same object -- ADDI, COPY, ADD of a constant
 * -- to what it names:
 *
 *   AL_FRAME   an ALLOCA: base is its id, off the byte offset within
 *   AL_GLOBAL  a GADDR: base is its id; two GADDRs of one symbol are
 *              distinct ids, so compare by h_name
 *   AL_PTR     any other value plus a constant: a pointer from
 *              somewhere; base is that value
 *   AL_UNKNOWN nothing could be said (an unresolvable chain)
 *
 * Two AL_PTR addresses off one base value are told apart by their bytes
 * only when the caller asks (ptr_apart): that is sound, but holding
 * every word of an Argon2 row in a register across its permutation
 * cost more in spills than the reloads it saved (33874324), so
 * forwarding does not ask and dead-store elimination does. */

#define AL_UNKNOWN 0
#define AL_FRAME   1
#define AL_GLOBAL  2
#define AL_PTR     3

static int al_resolve(int id, int *base, int *off) {
    int depth;
    int k;
    *off = 0;
    *base = -1;
    depth = 0;
    while (id >= 0 && depth < 64) {
        k = h_kind[id];
        if (k == HI_ALLOCA) { *base = id; return AL_FRAME; }
        if (k == HI_GADDR) { *base = id; return AL_GLOBAL; }
        if (k == HI_ADDI) { *off = *off + h_val[id]; id = h_src1[id]; }
        else if (k == HI_COPY) { id = h_src1[id]; }
        else if (k == HI_ADD && h_src2[id] >= 0 && h_kind[h_src2[id]] == HI_ICONST) {
            *off = *off + h_val[h_src2[id]]; id = h_src1[id];
        } else if (k == HI_ADD && h_src1[id] >= 0 && h_kind[h_src1[id]] == HI_ICONST) {
            *off = *off + h_val[h_src1[id]]; id = h_src2[id];
        } else {
            *base = id;
            return AL_PTR;
        }
        depth = depth + 1;
    }
    return AL_UNKNOWN;
}

/* The bytes an access of this type touches. */
static int al_acc_size(int ty) {
    int sz;
    sz = ty_size(ty);
    if (sz <= 0) sz = 4;
    return sz;
}

/* May a sza-byte access at a touch the same bytes as a szb-byte access
 * at b?  Sound in the conservative direction: anything unresolved may
 * alias anything, and a pointer may alias anything but (with ptr_apart)
 * other bytes off the same pointer value. */
static int al_may_alias(int a, int sza, int b, int szb, int ptr_apart) {
    int ba; int oa; int ka;
    int bb; int ob; int kb;
    if (a == b) return 1;
    ka = al_resolve(a, &ba, &oa);
    kb = al_resolve(b, &bb, &ob);
    if (ka == AL_UNKNOWN || kb == AL_UNKNOWN) return 1;
    if (ka == AL_PTR || kb == AL_PTR) {
        if (!ptr_apart || ka != kb || ba != bb) return 1;
    } else if (ka != kb) {
        return 0;                          /* a frame slot vs a global */
    } else if (ba != bb) {
        if (ka == AL_FRAME) return 0;      /* two distinct frame slots */
        if (h_name[ba] == NULL || h_name[bb] == NULL) return 1;
        if (strcmp(h_name[ba], h_name[bb]) != 0) return 0;
    }
    /* same object: byte ranges */
    if (oa + sza <= ob) return 0;
    if (ob + szb <= oa) return 0;
    return 1;
}

/* Do two address values name the same bytes?  Two GADDRs of one symbol
 * are distinct ids (hl_addr makes one per access), as are two ADDIs
 * off one slot; forwarding keyed by the id read a global twice in one
 * statement (docs/plans/memory-model.md, step 2b).  A pointer against
 * a pointer must be the same value plus the same offset. */
static int al_same_loc(int a, int b) {
    int ba; int oa; int ka;
    int bb; int ob; int kb;
    if (a == b) return 1;
    ka = al_resolve(a, &ba, &oa);
    kb = al_resolve(b, &bb, &ob);
    if (ka == AL_UNKNOWN || ka != kb || oa != ob) return 0;
    if (ba == bb) return 1;
    if (ka != AL_GLOBAL) return 0;
    if (h_name[ba] == NULL || h_name[bb] == NULL) return 0;
    return strcmp(h_name[ba], h_name[bb]) == 0;
}

/* Roots: for every instruction, the ALLOCA its value is an address
 * into, through ADDI, COPY and ADD-of-a-constant steps, and the byte
 * offset; -1 for a value that is not such an address.  Computed to a
 * fixed point over the function as it stands; al_n is h_ninst then, and
 * entries past it belong to no one. */
static int al_root[HIR_MAX_INST];
static int al_roff[HIR_MAX_INST];
static int al_n;

/* Is i an address step off some value?  Returns that value, -1 if not;
 * *delta gets the step's byte displacement. */
static int al_step(int i, int *delta) {
    int k;
    int a;
    int b;
    k = h_kind[i];
    a = h_src1[i];
    *delta = 0;
    if (k == HI_ADDI) { *delta = h_val[i]; return a; }
    if (k == HI_COPY) return a;
    if (k != HI_ADD) return -1;
    b = h_src2[i];
    if (a < 0 || b < 0) return -1;
    if (h_kind[b] == HI_ICONST) { *delta = h_val[b]; return a; }
    if (h_kind[a] == HI_ICONST) { *delta = h_val[a]; return b; }
    return -1;
}

static void al_roots(void) {
    int i;
    int q;
    int d;
    int grew;
    al_n = h_ninst;
    i = 0;
    while (i < al_n) {
        al_root[i] = -1;
        al_roff[i] = 0;
        if (h_kind[i] == HI_ALLOCA) al_root[i] = i;
        i = i + 1;
    }
    grew = 1;
    while (grew) {
        grew = 0;
        i = 0;
        while (i < al_n) {
            if (al_root[i] < 0 && h_kind[i] != HI_NOP) {
                q = al_step(i, &d);
                if (q >= 0 && al_root[q] >= 0) {
                    al_root[i] = al_root[q];
                    al_roff[i] = al_roff[q] + d;
                    grew = 1;
                }
            }
            i = i + 1;
        }
    }
}

/* Has a root's address left the function?  A use of the root, or of a
 * step off it, that is neither another step nor the address of a LOAD
 * or STORE -- stored as a value, passed to a call, merged by a phi,
 * computed with -- lets a callee or a later load through a pointer
 * reach the slot.  A slot that never escapes is invisible to calls and
 * to loads through pointers, and dead at the function's return
 * (docs/plans/memory-model.md, step 4).  Computed with al_roots. */
static char al_escaped[HIR_MAX_INST];

static void al_escapes(void) {
    int i;
    int n;
    int r;
    int v;
    al_roots();
    i = 0;
    while (i < al_n) { al_escaped[i] = 0; i = i + 1; }
    i = 0;
    while (i < al_n) {
        if (h_kind[i] != HI_NOP && !(al_root[i] >= 0 && h_kind[i] != HI_ALLOCA)) {
            n = 0;
            while (n < hi_nrefs(i)) {
                v = hi_ref(i, n);
                if (v >= 0 && v < al_n && al_root[v] >= 0 && !hi_ref_is_addr(i, n)) {
                    r = al_root[v];
                    al_escaped[r] = 1;
                }
                n = n + 1;
            }
        }
        i = i + 1;
    }
}

/* Is i a step off a root rather than the root itself? */
static int al_is_step(int i) {
    return i >= 0 && i < al_n && al_root[i] >= 0 && h_kind[i] != HI_ALLOCA;
}
