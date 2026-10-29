/* ctype.h -- C types for the front end (docs/TYPES.md).
 *
 * A C type is a handle: (record index << 3) | qualifiers.  Records are
 * hash-consed, one per distinct unqualified type, so two unqualified
 * types are the same type iff their handles are equal, and a qualified
 * type is the unqualified handle with bits set (ct_unqual strips them).
 *
 * This is the type the standard talks about.  The machine type that HIR
 * and the code generators use -- ast.h's int encoding, base + pointer
 * depth + unsigned -- is derived from it by ct_mty; the back ends never
 * see a C type.
 *
 * Fixed capacity, as the compiler's other tables, with an error when a
 * table fills.  Ints and char * only: stage07 compiles this.
 */

/* --- Kinds --- */
#define CT_VOID     1
#define CT_BOOL     2
#define CT_CHAR     3    /* plain char: a distinct type (6.2.5p15), signed here */
#define CT_SCHAR    4
#define CT_UCHAR    5
#define CT_SHORT    6
#define CT_USHORT   7
#define CT_INT      8
#define CT_UINT     9
#define CT_LONG    10
#define CT_ULONG   11
#define CT_LLONG   12
#define CT_ULLONG  13
#define CT_I128    14
#define CT_UI128   15
#define CT_FLOAT   16
#define CT_DOUBLE  17
#define CT_LDOUBLE 18
#define CT_ENUM    19    /* count: the enum's tag index (its type is int) */
#define CT_PTR     20    /* base: the pointee */
#define CT_ARRAY   21    /* base: the element; count: length, -1 incomplete */
#define CT_FUNC    22    /* base: the return type; parm/nparm: the parameters */
#define CT_STRUCT  23    /* count: the st_* struct index */
#define CT_UNION   24    /* count: the st_* struct index */
#define CT_ATOMIC  25    /* base: the type, _Atomic(T) */
#define CT_NKINDS  26

/* --- Qualifiers: the handle's low bits --- */
#define CQ_CONST    1
#define CQ_VOLATILE 2
#define CQ_RESTRICT 4
#define CQ_MASK     7

/* --- Function flags --- */
#define CF_VARIADIC 1

#define CT_MAX       16384   /* records: distinct unqualified types */
#define CT_HASH      32768   /* open-addressed intern table, a power of two */
#define CT_MAX_PLIST 65536   /* parameter-type list pool */

static int ct_n;                       /* records in use; record 0 is unused */
static int ct_kind[CT_MAX];
static int ct_base[CT_MAX];
static int ct_count[CT_MAX];
static int ct_parm[CT_MAX];
static int ct_nparm[CT_MAX];           /* -1: declared without a prototype */
static int ct_flags[CT_MAX];
static int ct_hash[CT_HASH];           /* record index, 0 = empty */
static int ct_plist[CT_MAX_PLIST];
static int ct_nplist;

/* The basic types, interned at ct_init: ct_basic[CT_INT] and so on. */
static int ct_basic[CT_NKINDS];

static void ct_full(char *what) {
    fdputs("s12cc: too many ", 2);
    fdputs(what, 2);
    fdputs(" (ctype.h)\n", 2);
    exit(1);
}

/* --- Handles --- */

static int ct_rec(int t)      { return t >> 3; }
static int ct_qual(int t)     { return t & CQ_MASK; }
static int ct_unqual(int t)   { return t & ~CQ_MASK; }
static int ct_kindof(int t)   { return ct_kind[t >> 3]; }
static int ct_baseof(int t)   { return ct_base[t >> 3]; }
static int ct_countof(int t)  { return ct_count[t >> 3]; }
static int ct_nparms(int t)   { return ct_nparm[t >> 3]; }
static int ct_parmty(int t, int i) { return ct_plist[ct_parm[t >> 3] + i]; }
static int ct_variadic(int t) { return (ct_flags[t >> 3] & CF_VARIADIC) != 0; }

/* --- Interning --- */

static unsigned ct_mix(unsigned h, int v) {
    h = h ^ (unsigned)v;
    return h * 16777619u;        /* FNV-1a step; unsigned, so it wraps */
}

static int ct_same_plist(int a, int b, int n) {
    int i;
    i = 0;
    while (i < n) {
        if (ct_plist[a + i] != ct_plist[b + i]) return 0;
        i = i + 1;
    }
    return 1;
}

/* The one record for (kind, base, count, parameters, flags); parm is an
 * index into ct_plist holding nparm entries (nparm -1 or 0: none). */
static int ct_intern(int kind, int base, int count, int parm, int nparm, int flags) {
    unsigned h;
    int i;
    int slot;
    int r;
    int np;
    h = ct_mix(ct_mix(ct_mix(ct_mix(ct_mix(2166136261u, kind), base), count), nparm), flags);
    np = nparm > 0 ? nparm : 0;
    i = 0;
    while (i < np) {
        h = ct_mix(h, ct_plist[parm + i]);
        i = i + 1;
    }
    slot = (int)(h & (CT_HASH - 1));
    while (1) {
        r = ct_hash[slot];
        if (r == 0) break;
        if (ct_kind[r] == kind && ct_base[r] == base && ct_count[r] == count &&
            ct_nparm[r] == nparm && ct_flags[r] == flags &&
            (np == 0 || ct_same_plist(ct_parm[r], parm, np)))
            return r << 3;
        slot = (slot + 1) & (CT_HASH - 1);
    }
    if (ct_n >= CT_MAX) ct_full("C types");
    r = ct_n;
    ct_n = ct_n + 1;
    ct_kind[r] = kind;
    ct_base[r] = base;
    ct_count[r] = count;
    ct_parm[r] = parm;
    ct_nparm[r] = nparm;
    ct_flags[r] = flags;
    ct_hash[slot] = r;
    return r << 3;
}

static void ct_init(void) {
    int k;
    ct_n = 1;
    ct_nplist = 0;
    k = CT_VOID;
    while (k <= CT_LDOUBLE) {
        ct_basic[k] = ct_intern(k, 0, 0, 0, 0, 0);
        k = k + 1;
    }
}

/* --- Constructors --- */

static int ct_ptr(int t)              { return ct_intern(CT_PTR, t, 0, 0, 0, 0); }
static int ct_atomic(int t)           { return ct_intern(CT_ATOMIC, t, 0, 0, 0, 0); }
static int ct_struct(int si)          { return ct_intern(st_is_union[si] ? CT_UNION : CT_STRUCT, 0, si, 0, 0, 0); }
static int ct_enum(int tag)           { return ct_intern(CT_ENUM, 0, tag, 0, 0, 0); }

/* Qualifiers on an array type qualify its element (6.7.3p9). */
static int ct_qualify(int t, int q) {
    if (ct_kindof(t) == CT_ARRAY)
        return ct_intern(CT_ARRAY, ct_qualify(ct_baseof(t), q), ct_countof(t), 0, 0, 0) | ct_qual(t);
    return t | q;
}

static int ct_array(int elem, int n)  { return ct_intern(CT_ARRAY, elem, n, 0, 0, 0); }

/* A function type.  parms: nparm parameter types, already adjusted
 * (arrays and functions to pointers, qualifiers dropped -- 6.7.6.3p7-8,
 * 15); nparm -1 for a declaration without a prototype. */
static int ct_func(int ret, int *parms, int nparm, int variadic) {
    int start;
    int i;
    int t;
    start = ct_nplist;
    if (nparm > 0) {
        if (ct_nplist + nparm > CT_MAX_PLIST) ct_full("parameter types");
        i = 0;
        while (i < nparm) {
            ct_plist[ct_nplist + i] = parms[i];
            i = i + 1;
        }
    }
    t = ct_intern(CT_FUNC, ret, 0, start, nparm, variadic ? CF_VARIADIC : 0);
    /* A new record keeps the list; a found one has its own copy. */
    if (nparm > 0 && ct_parm[t >> 3] == start) ct_nplist = ct_nplist + nparm;
    return t;
}

/* --- Classification (6.2.5) --- */

static int ct_is_integer(int t) {
    int k;
    k = ct_kindof(t);
    return (k >= CT_BOOL && k <= CT_UI128) || k == CT_ENUM;
}
static int ct_is_unsigned(int t) {
    int k;
    k = ct_kindof(t);
    return k == CT_BOOL || k == CT_UCHAR || k == CT_USHORT || k == CT_UINT ||
           k == CT_ULONG || k == CT_ULLONG || k == CT_UI128;
}
static int ct_is_floating(int t) {
    int k;
    k = ct_kindof(t);
    return k == CT_FLOAT || k == CT_DOUBLE || k == CT_LDOUBLE;
}
static int ct_is_arith(int t)  { return ct_is_integer(t) || ct_is_floating(t); }
static int ct_is_ptr(int t)    { return ct_kindof(t) == CT_PTR; }
static int ct_is_scalar(int t) { return ct_is_arith(t) || ct_is_ptr(t); }
static int ct_is_array(int t)  { return ct_kindof(t) == CT_ARRAY; }
static int ct_is_func(int t)   { return ct_kindof(t) == CT_FUNC; }
static int ct_is_record(int t) { return ct_kindof(t) == CT_STRUCT || ct_kindof(t) == CT_UNION; }

/* --- To the machine type --- */

/* The machine type (ast.h encoding) the rest of the compiler has always
 * given an object of C type t.  Arrays are their decayed pointer, and a
 * pointer to a function is the opaque int pointer, as the parser types
 * them; _Bool is unsigned char, as it is still lowered (W2 -- the
 * conversion to it is the C type's business, docs/TYPES.md S3). */
static int ct_mty(int t) {
    int k;
    k = ct_kindof(t);
    if (k == CT_VOID) return TY_VOID;
    if (k == CT_BOOL || k == CT_UCHAR) return TY_CHAR | TY_UNSIGNED;
    if (k == CT_CHAR || k == CT_SCHAR) return TY_CHAR;
    if (k == CT_SHORT) return TY_SHORT;
    if (k == CT_USHORT) return TY_SHORT | TY_UNSIGNED;
    if (k == CT_INT || k == CT_ENUM) return TY_INT;
    if (k == CT_UINT) return TY_INT | TY_UNSIGNED;
    /* long is the target's: int on SLOW-32, long long on the 64-bit crosses */
    if (k == CT_LONG) return ty_ptr_size == 8 ? TY_LLONG : TY_INT;
    if (k == CT_ULONG) return (ty_ptr_size == 8 ? TY_LLONG : TY_INT) | TY_UNSIGNED;
    if (k == CT_LLONG) return TY_LLONG;
    if (k == CT_ULLONG) return TY_LLONG | TY_UNSIGNED;
    if (k == CT_I128) return TY_I128;
    if (k == CT_UI128) return TY_I128 | TY_UNSIGNED;
    if (k == CT_FLOAT) return TY_FLOAT;
    if (k == CT_DOUBLE || k == CT_LDOUBLE) return TY_DOUBLE;
    if (k == CT_STRUCT || k == CT_UNION) return TY_STRUCT_BASE + ct_countof(t);
    if (k == CT_ATOMIC) return ct_mty(ct_baseof(t));
    if (k == CT_ARRAY) {
        /* the machine flattens: an array of arrays is its innermost
         * element's pointer, the column count kept beside it */
        while (ct_kindof(ct_baseof(t)) == CT_ARRAY) t = ct_baseof(t);
        return ct_mty(ct_baseof(t)) + TY_PTR;
    }
    if (k == CT_FUNC) return TY_INT + TY_PTR;
    /* CT_PTR */
    if (ct_kindof(ct_baseof(t)) == CT_FUNC) return TY_INT + TY_PTR;
    /* a pointer to an array is, as the machine has it, a pointer to the
     * innermost element, the row width kept beside it: T (*p)[M] */
    if (ct_kindof(ct_baseof(t)) == CT_ARRAY) return ct_mty(ct_baseof(t));
    return ct_mty(ct_baseof(t)) + TY_PTR;
}

/* --- From a machine type --- */

/* The C type a machine type stands for, where the parser has nothing
 * better: hint is a CT_ kind for what the int encoding cannot say --
 * CT_LONG/CT_ULONG for long, CT_SCHAR for signed char, CT_LDOUBLE,
 * CT_ENUM -- or 0.  Pointer levels are rebuilt from the depth. */
static int ct_from_mty(int ty, int hint) {
    int base;
    int depth;
    int uns;
    int t;
    base = ty & TY_BASE_MASK;
    depth = (ty & TY_PTR_MASK) / TY_PTR;
    uns = (ty & TY_UNSIGNED) != 0;
    if (base >= TY_STRUCT_BASE) t = ct_struct(base - TY_STRUCT_BASE);
    else if (base == TY_VOID) t = ct_basic[CT_VOID];
    else if (base == TY_CHAR) {
        if (uns) t = ct_basic[CT_UCHAR];
        else if (hint == CT_SCHAR) t = ct_basic[CT_SCHAR];
        else t = ct_basic[CT_CHAR];
    }
    else if (base == TY_SHORT) t = ct_basic[uns ? CT_USHORT : CT_SHORT];
    else if (base == TY_INT) {
        if (hint == CT_ENUM && !uns) t = ct_enum(0);
        else if (hint == CT_LONG || hint == CT_ULONG) t = ct_basic[uns ? CT_ULONG : CT_LONG];
        else t = ct_basic[uns ? CT_UINT : CT_INT];
    }
    else if (base == TY_LLONG) {
        if (hint == CT_LONG || hint == CT_ULONG) t = ct_basic[uns ? CT_ULONG : CT_LONG];
        else t = ct_basic[uns ? CT_ULLONG : CT_LLONG];
    }
    else if (base == TY_I128) t = ct_basic[uns ? CT_UI128 : CT_I128];
    else if (base == TY_FLOAT) t = ct_basic[CT_FLOAT];
    else if (base == TY_DOUBLE) t = ct_basic[hint == CT_LDOUBLE ? CT_LDOUBLE : CT_DOUBLE];
    else t = ct_basic[CT_INT];
    while (depth > 0) {
        t = ct_ptr(t);
        depth = depth - 1;
    }
    return t;
}

/* --- Conversions (6.3) --- */

/* Integer conversion rank (6.3.1.1p1): bool < char < short < int < long
 * < long long < __int128; an enum ranks as int, its compatible type. */
static int ct_rank(int t) {
    int k;
    k = ct_kindof(t);
    if (k == CT_BOOL) return 1;
    if (k == CT_CHAR || k == CT_SCHAR || k == CT_UCHAR) return 2;
    if (k == CT_SHORT || k == CT_USHORT) return 3;
    if (k == CT_INT || k == CT_UINT || k == CT_ENUM) return 4;
    if (k == CT_LONG || k == CT_ULONG) return 5;
    if (k == CT_LLONG || k == CT_ULLONG) return 6;
    if (k == CT_I128 || k == CT_UI128) return 7;
    return 0;
}

/* Bytes in an integer type's value, for "can the signed type hold every
 * value of the unsigned one" (6.3.1.8p1).  long follows the target. */
static int ct_int_bytes(int t) {
    int r;
    r = ct_rank(t);
    if (r <= 2) return 1;
    if (r == 3) return 2;
    if (r == 4) return 4;
    if (r == 5) return ty_ptr_size == 8 ? 8 : 4;
    if (r == 6) return 8;
    return 16;
}

/* The unsigned type of an integer type's rank. */
static int ct_to_unsigned(int t) {
    int r;
    r = ct_rank(t);
    if (r <= 2) return ct_basic[CT_UCHAR];
    if (r == 3) return ct_basic[CT_USHORT];
    if (r == 4) return ct_basic[CT_UINT];
    if (r == 5) return ct_basic[CT_ULONG];
    if (r == 6) return ct_basic[CT_ULLONG];
    return ct_basic[CT_UI128];
}

/* The integer promotions (6.3.1.1p2): below int's rank, to int (every
 * such value fits); otherwise the type itself, unqualified.  An enum is
 * int.  A non-integer is returned unqualified. */
static int ct_promote(int t) {
    t = ct_unqual(t);
    if (ct_kindof(t) == CT_ATOMIC) t = ct_unqual(ct_baseof(t));
    if (!ct_is_integer(t)) return t;
    if (ct_rank(t) < 4 || ct_kindof(t) == CT_ENUM) return ct_basic[CT_INT];
    return t;
}

/* The usual arithmetic conversions (6.3.1.8p1): the common real type. */
static int ct_usual(int a, int b) {
    int ka;
    int kb;
    a = ct_promote(a);
    b = ct_promote(b);
    ka = ct_kindof(a);
    kb = ct_kindof(b);
    if (ka == CT_LDOUBLE || kb == CT_LDOUBLE) return ct_basic[CT_LDOUBLE];
    if (ka == CT_DOUBLE || kb == CT_DOUBLE) return ct_basic[CT_DOUBLE];
    if (ka == CT_FLOAT || kb == CT_FLOAT) return ct_basic[CT_FLOAT];
    if (a == b) return a;
    if (ct_is_unsigned(a) == ct_is_unsigned(b))
        return ct_rank(a) >= ct_rank(b) ? a : b;
    /* one signed, one unsigned: u the unsigned, s the signed */
    if (ct_is_unsigned(b)) { ka = a; a = b; b = ka; }
    if (ct_rank(a) >= ct_rank(b)) return a;
    if (ct_int_bytes(b) > ct_int_bytes(a)) return b;
    return ct_to_unsigned(b);
}

/* Array and function decay (6.3.2.1p3-4): what an expression of type t
 * is as an rvalue -- an array its element's pointer, a function its
 * pointer -- and an lvalue's value drops its qualifiers (p2). */
static int ct_decay(int t) {
    if (ct_kindof(t) == CT_ARRAY) return ct_ptr(ct_baseof(t));
    if (ct_kindof(t) == CT_FUNC) return ct_ptr(ct_unqual(t));
    return ct_unqual(t);
}

/* Does C type t agree with machine type ty?  ct_mty(t) == ty, except that
 * a pointer to a function has two machine spellings: the parser types a
 * function-pointer object as int and a cast to one as int *; both are
 * the same word to the code generators. */
static int ct_mty_agrees(int t, int ty) {
    int b;
    if (ct_mty(t) == ty) return 1;
    if (ct_kindof(t) == CT_PTR && ct_kindof(ct_baseof(t)) == CT_FUNC)
        return ty == TY_INT || ty == TY_INT + TY_PTR;
    /* a pointer to a function pointer, at any depth, is spelled as int
     * words too: void (**aExt)(void) fell back to int, and aExt[i]
     * compared with a function pointer as an integer */
    b = t;
    while (ct_kindof(b) == CT_PTR) b = ct_baseof(b);
    if (ct_kindof(b) == CT_FUNC && ct_kindof(ct_baseof(t)) == CT_PTR)
        return (ty & ~TY_PTR_MASK) == TY_INT;
    return 0;
}

/* The alignment of t (6.2.8): an array's is its element's, the rest
 * the machine type's -- a structure's, its members' greatest. */
static int ct_alignof(int t) {
    while (ct_kindof(t) == CT_ARRAY) t = ct_baseof(t);
    if (ct_kindof(t) == CT_FUNC || ct_kindof(t) == CT_VOID) return 1;
    return ty_align(ct_mty(t));
}

/* --- Sizes (6.5.3.4) --- */

/* sizeof a C type, in bytes.  Structs and unions take the parser's
 * layout (ast.h's ty_size); an array of unknown length is 0. */
static int ct_sizeof(int t) {
    int k;
    k = ct_kindof(t);
    if (k == CT_ATOMIC) return ct_sizeof(ct_baseof(t));
    if (k == CT_ARRAY) {
        if (ct_countof(t) < 0) return 0;
        return ct_countof(t) * ct_sizeof(ct_baseof(t));
    }
    if (k == CT_PTR) return ty_ptr_size;
    if (k == CT_STRUCT || k == CT_UNION) return ty_size(TY_STRUCT_BASE + ct_countof(t));
    if (k == CT_VOID || k == CT_FUNC) return 1;   /* a GNU extension; a constraint violation in C */
    if (ct_is_integer(t)) return k == CT_BOOL ? 1 : ct_int_bytes(t);
    if (k == CT_FLOAT) return 4;
    return 8;   /* double, long double */
}
