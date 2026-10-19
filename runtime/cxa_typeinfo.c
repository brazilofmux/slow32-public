/*
 * SLOW-32 C++ run-time type information (Itanium C++ ABI, section 2.9.5)
 *
 * The type_info objects the compiler does not emit itself -- every
 * fundamental type T, T* and const T* -- and the vtables that the
 * compiler's own type_info objects point into, one per kind of type.
 * Code here tells the kinds apart by that vtable pointer and reads the
 * fields the ABI lays out after it:
 *
 *   class      { vtable, name }                          no bases
 *   si_class   { vtable, name, base }                    one public non-virtual base at offset 0
 *   vmi_class  { vtable, name, flags, count, {base, offset_flags}[count] }
 *   pointer    { vtable, name, flags, pointee }          T*, with T's cv-qualifiers in flags
 *
 * On that: the class-hierarchy search catch-by-base and dynamic_cast both
 * need (__s32_find_base), and __dynamic_cast itself.  The personality
 * routine (cxa_personality.c) does the catch matching with these.
 *
 * Two type_info objects describe the same type when they are the same
 * object or their names compare equal: a class's type_info is emitted
 * weak in every unit that needs it, so equal types need not share an
 * address.  A name starting with '*' asks for address comparison only.
 */

#include <stddef.h>
#include <stdint.h>
#include <string.h>

typedef struct type_info {
    const void *vtable;
    const char *name;
} type_info;

typedef struct {
    type_info ti;
    const type_info *base;
} si_class_type_info;

typedef struct {
    const type_info *base;
    long offset_flags;          /* offset << 8 | public 2 | virtual 1 */
} base_class_type_info;

typedef struct {
    type_info ti;
    unsigned flags;
    unsigned base_count;
    base_class_type_info base_info[];
} vmi_class_type_info;

typedef struct {
    type_info ti;
    unsigned flags;             /* const 1, volatile 2, restrict 4, ... */
    const type_info *pointee;
} pbase_type_info;

enum { BASE_VIRTUAL = 1, BASE_PUBLIC = 2, OFFSET_SHIFT = 8 };
enum { PTR_CONST = 1, PTR_VOLATILE = 2, PTR_RESTRICT = 4 };

/* ------------------------------------------------------------------------
 * The vtables.  A type_info's vtable pointer is the address two slots in
 * (offset-to-top, then the typeinfo slot); the rest is the destructor
 * pair, which nothing calls -- type_info objects are never destroyed.
 * ------------------------------------------------------------------------ */

static void type_info_dtor(void *self) { (void)self; }

#define TI_VTABLE(sym) \
    void *const sym[4] = { 0, 0, (void *)type_info_dtor, (void *)type_info_dtor }

TI_VTABLE(_ZTVN10__cxxabiv123__fundamental_type_infoE);
TI_VTABLE(_ZTVN10__cxxabiv117__class_type_infoE);
TI_VTABLE(_ZTVN10__cxxabiv120__si_class_type_infoE);
TI_VTABLE(_ZTVN10__cxxabiv121__vmi_class_type_infoE);
TI_VTABLE(_ZTVN10__cxxabiv119__pointer_type_infoE);
TI_VTABLE(_ZTVN10__cxxabiv129__pointer_to_member_type_infoE);
TI_VTABLE(_ZTVN10__cxxabiv116__enum_type_infoE);
TI_VTABLE(_ZTVN10__cxxabiv120__function_type_infoE);
TI_VTABLE(_ZTVN10__cxxabiv117__array_type_infoE);

#define VPTR(sym) ((const void *)&(sym)[2])

/* ------------------------------------------------------------------------
 * Fundamental types: T, T* and const T* for each.
 * ------------------------------------------------------------------------ */

#define FUNDAMENTAL(code)                                                    \
    const type_info _ZTI##code = {                                          \
        VPTR(_ZTVN10__cxxabiv123__fundamental_type_infoE), #code };          \
    const pbase_type_info _ZTIP##code = {                                   \
        { VPTR(_ZTVN10__cxxabiv119__pointer_type_infoE), "P" #code },        \
        0, &_ZTI##code };                                                    \
    const pbase_type_info _ZTIPK##code = {                                  \
        { VPTR(_ZTVN10__cxxabiv119__pointer_type_infoE), "PK" #code },       \
        PTR_CONST, &_ZTI##code };

FUNDAMENTAL(v)      /* void */
FUNDAMENTAL(b)      /* bool */
FUNDAMENTAL(w)      /* wchar_t */
FUNDAMENTAL(c)      /* char */
FUNDAMENTAL(a)      /* signed char */
FUNDAMENTAL(h)      /* unsigned char */
FUNDAMENTAL(s)      /* short */
FUNDAMENTAL(t)      /* unsigned short */
FUNDAMENTAL(i)      /* int */
FUNDAMENTAL(j)      /* unsigned int */
FUNDAMENTAL(l)      /* long */
FUNDAMENTAL(m)      /* unsigned long */
FUNDAMENTAL(x)      /* long long */
FUNDAMENTAL(y)      /* unsigned long long */
FUNDAMENTAL(n)      /* __int128 */
FUNDAMENTAL(o)      /* unsigned __int128 */
FUNDAMENTAL(f)      /* float */
FUNDAMENTAL(d)      /* double */
FUNDAMENTAL(e)      /* long double */
FUNDAMENTAL(Dn)     /* std::nullptr_t */
FUNDAMENTAL(Ds)     /* char16_t */
FUNDAMENTAL(Di)     /* char32_t */
FUNDAMENTAL(Du)     /* char8_t */

/* ------------------------------------------------------------------------
 * Kinds and equality
 * ------------------------------------------------------------------------ */

int __s32_type_eq(const type_info *a, const type_info *b) {
    if (a == b) return 1;
    if (!a || !b) return 0;
    if (a->name == b->name) return 1;
    if (a->name[0] == '*' || b->name[0] == '*') return 0;
    return strcmp(a->name, b->name) == 0;
}

int __s32_type_is_class(const type_info *t) {
    const void *v = t->vtable;
    return v == VPTR(_ZTVN10__cxxabiv117__class_type_infoE) ||
           v == VPTR(_ZTVN10__cxxabiv120__si_class_type_infoE) ||
           v == VPTR(_ZTVN10__cxxabiv121__vmi_class_type_infoE);
}

int __s32_type_is_pointer(const type_info *t) {
    return t->vtable == VPTR(_ZTVN10__cxxabiv119__pointer_type_infoE);
}

/* ------------------------------------------------------------------------
 * The hierarchy search.  Walk every base-class subobject of an object of
 * class type T at OBJ, collecting the distinct addresses at which a
 * subobject of type TARGET sits and whether each is reachable along an
 * all-public path.  A virtual base reached by two paths is one address,
 * so it is one candidate, not an ambiguity.
 * ------------------------------------------------------------------------ */

#define MAX_HITS 16

struct search {
    const type_info *target;
    int n;                              /* distinct addresses found */
    int overflow;
    struct { void *addr; int pub; } hit[MAX_HITS];
};

static void record(struct search *s, void *addr, int pub) {
    for (int i = 0; i < s->n; i++)
        if (s->hit[i].addr == addr) { s->hit[i].pub |= pub; return; }
    if (s->n == MAX_HITS) { s->overflow = 1; return; }
    s->hit[s->n].addr = addr;
    s->hit[s->n].pub = pub;
    s->n++;
}

static void walk(const type_info *t, void *obj, int pub, struct search *s) {
    if (__s32_type_eq(t, s->target)) {
        record(s, obj, pub);
        return;                         /* a class cannot contain its own type */
    }
    if (t->vtable == VPTR(_ZTVN10__cxxabiv120__si_class_type_infoE)) {
        walk(((const si_class_type_info *)t)->base, obj, pub, s);
    } else if (t->vtable == VPTR(_ZTVN10__cxxabiv121__vmi_class_type_infoE)) {
        const vmi_class_type_info *v = (const vmi_class_type_info *)t;
        for (unsigned k = 0; k < v->base_count; k++) {
            long of = v->base_info[k].offset_flags;
            ptrdiff_t off = of >> OFFSET_SHIFT;
            if (of & BASE_VIRTUAL) {
                /* off locates, in OBJ's vtable, where the virtual base's
                 * offset from OBJ is stored */
                const char *vtbl = *(const char *const *)obj;
                off = *(const ptrdiff_t *)(vtbl + off);
            }
            walk(v->base_info[k].base, (char *)obj + off,
                 pub && (of & BASE_PUBLIC), s);
        }
    }
}

/* The unique, publicly reachable subobject of type BASE within an object
 * of class type DERIVED at OBJ: 1 with *OUT set, or 0 when there is none,
 * more than one, or the one there is not public. */
int __s32_find_base(const type_info *derived, void *obj,
                    const type_info *base, void **out) {
    struct search s;
    s.target = base;
    s.n = 0;
    s.overflow = 0;
    walk(derived, obj, 1, &s);
    if (s.n != 1 || s.overflow || !s.hit[0].pub) return 0;
    *out = s.hit[0].addr;
    return 1;
}

/* ------------------------------------------------------------------------
 * dynamic_cast<DST *>(SUB), SUB pointing at a subobject of static type
 * SRC.  The compiler handles the casts it can decide statically and to
 * void*; this sees the rest.  SRC2DST is the compiler's hint about how
 * DST relates to SRC; the search below does not need it.
 * ------------------------------------------------------------------------ */

void *__dynamic_cast(const void *sub, const type_info *src,
                     const type_info *dst, ptrdiff_t src2dst) {
    (void)src2dst;
    if (!sub) return 0;

    /* The most-derived object: offset-to-top and its type_info sit just
     * before the address SUB's vtable pointer holds. */
    const void *const *vptr = *(const void *const *const *)sub;
    ptrdiff_t to_top = (ptrdiff_t)vptr[-2];
    const type_info *whole_t = (const type_info *)vptr[-1];
    char *whole = (char *)sub + to_top;

    struct search d;
    d.target = dst;
    d.n = 0;
    d.overflow = 0;
    walk(whole_t, whole, 1, &d);
    if (d.overflow) return 0;

    /* Downcast: a DST subobject in which SUB is a public SRC base. */
    void *found = 0;
    int nfound = 0;
    for (int i = 0; i < d.n; i++) {
        struct search s;
        s.target = src;
        s.n = 0;
        s.overflow = 0;
        walk(dst, d.hit[i].addr, 1, &s);
        for (int k = 0; k < s.n; k++)
            if (s.hit[k].addr == sub && s.hit[k].pub) {
                if (nfound == 0 || found != d.hit[i].addr) nfound++;
                found = d.hit[i].addr;
            }
    }
    if (nfound == 1) return found;
    if (nfound > 1) return 0;

    /* Cross-cast: SUB a public SRC base of the whole object, and DST a
     * unique public base of it too. */
    struct search s;
    s.target = src;
    s.n = 0;
    s.overflow = 0;
    walk(whole_t, whole, 1, &s);
    int sub_public = 0;
    for (int k = 0; k < s.n; k++)
        if (s.hit[k].addr == sub && s.hit[k].pub) sub_public = 1;
    if (sub_public && d.n == 1 && d.hit[0].pub) return d.hit[0].addr;
    return 0;
}
