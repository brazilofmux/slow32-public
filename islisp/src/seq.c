/* seq.c -- characters (§20), arrays (§22), vectors (§23), strings (§24)
 * and the sequence functions (§25).
 *
 * A string is a vector of characters stored as bytes; a general vector
 * holds objects; a general array of rank 2 or more (or rank 0) is a
 * dimension vector over a flat data vector in row-major order.  "Basic
 * array" means any of the three.
 */
#include "isl.h"
#include <string.h>
#include <stdlib.h>

#define HUGE_REQUEST 100000000L

static long need_count(val v) {
    if (!IS_INT(v) || int_sign(v) < 0) err_domain(v, "<integer>");
    if (!IS_FIX(v) || FIXVAL(v) > HUGE_REQUEST) isl_error("<STORAGE-EXHAUSTED>", NULL);
    return FIXVAL(v);
}
static void need_char(val v) { if (!IS_CHR(v)) err_domain(v, "<character>"); }
static void need_string(val v) { if (!IS_STR(v)) err_domain(v, "<string>"); }
static void need_mutable(val v) { if (IS_PTR(v) && ((obj *)v)->imm) err_immutable(v); }

/* ---------- characters ---------- */

static val bi_char_eq(int argc, val *argv) { (void)argc; need_char(argv[0]); need_char(argv[1]); return mk_bool(argv[0] == argv[1]); }
static val bi_char_ne(int argc, val *argv) { (void)argc; need_char(argv[0]); need_char(argv[1]); return mk_bool(argv[0] != argv[1]); }
static val bi_char_lt(int argc, val *argv) { (void)argc; need_char(argv[0]); need_char(argv[1]); return mk_bool(CHRVAL(argv[0]) < CHRVAL(argv[1])); }
static val bi_char_gt(int argc, val *argv) { (void)argc; need_char(argv[0]); need_char(argv[1]); return mk_bool(CHRVAL(argv[0]) > CHRVAL(argv[1])); }
static val bi_char_le(int argc, val *argv) { (void)argc; need_char(argv[0]); need_char(argv[1]); return mk_bool(CHRVAL(argv[0]) <= CHRVAL(argv[1])); }
static val bi_char_ge(int argc, val *argv) { (void)argc; need_char(argv[0]); need_char(argv[1]); return mk_bool(CHRVAL(argv[0]) >= CHRVAL(argv[1])); }

/* ---------- strings ---------- */

static val bi_create_string(int argc, val *argv) {
    long n = need_count(argv[0]);
    int fill = ' ';
    val s;
    if (argc > 1) { need_char(argv[1]); fill = CHRVAL(argv[1]); }
    s = mk_string_uninit(n);
    memset(STR(s)->data, fill, (size_t)n);
    return s;
}

static int str_cmp(val a, val b) {
    long n = STR(a)->len < STR(b)->len ? STR(a)->len : STR(b)->len;
    int c = memcmp(STR(a)->data, STR(b)->data, (size_t)n);
    if (c) return c < 0 ? -1 : 1;
    return STR(a)->len < STR(b)->len ? -1 : (STR(a)->len > STR(b)->len ? 1 : 0);
}
static val str_rel(int argc, val *argv, int lo, int hi) {
    int c;
    (void)argc;
    need_string(argv[0]); need_string(argv[1]);
    c = str_cmp(argv[0], argv[1]);
    return mk_bool(c >= lo && c <= hi);
}
static val bi_string_eq(int argc, val *argv) { return str_rel(argc, argv, 0, 0); }
static val bi_string_ne(int argc, val *argv) { int c; (void)argc; need_string(argv[0]); need_string(argv[1]); c = str_cmp(argv[0], argv[1]); return mk_bool(c != 0); }
static val bi_string_lt(int argc, val *argv) { return str_rel(argc, argv, -1, -1); }
static val bi_string_gt(int argc, val *argv) { return str_rel(argc, argv, 1, 1); }
static val bi_string_le(int argc, val *argv) { return str_rel(argc, argv, -1, 0); }
static val bi_string_ge(int argc, val *argv) { return str_rel(argc, argv, 0, 1); }

/* The suite's reading of §9.2 and §29.4: an index that is not a
 * non-negative integer is a domain error; one past the end is
 * index-out-of-range, a program error. */
static long start_pos(int argc, val *argv, int idx, long len) {
    long st = 0;
    if (argc > idx) {
        if (!IS_INT(argv[idx]) || int_sign(argv[idx]) < 0) err_domain(argv[idx], "<integer>");
        if (!IS_FIX(argv[idx])) err_index_out_of_range(argv[1], argv[idx]);
        st = FIXVAL(argv[idx]);
        if (st >= len) err_index_out_of_range(argv[1], argv[idx]);
    }
    return st;
}
static val bi_char_index(int argc, val *argv) {
    long i, st;
    need_char(argv[0]); need_string(argv[1]);
    st = start_pos(argc, argv, 2, STR(argv[1])->len);
    for (i = st; i < STR(argv[1])->len; i++) if ((unsigned char)STR(argv[1])->data[i] == CHRVAL(argv[0])) return MKFIX(i);
    return NIL;
}
static val bi_string_index(int argc, val *argv) {
    long i, st, n, m;
    need_string(argv[0]); need_string(argv[1]);
    st = start_pos(argc, argv, 2, STR(argv[1])->len);
    n = STR(argv[1])->len; m = STR(argv[0])->len;
    for (i = st; i + m <= n; i++) if (memcmp(STR(argv[1])->data + i, STR(argv[0])->data, (size_t)m) == 0) return MKFIX(i);
    return NIL;
}
static val bi_string_append(int argc, val *argv) {
    long total = 0, pos = 0;
    int i;
    val s;
    for (i = 0; i < argc; i++) { need_string(argv[i]); total += STR(argv[i])->len; }
    s = mk_string_uninit(total);
    for (i = 0; i < argc; i++) { memcpy(STR(s)->data + pos, STR(argv[i])->data, (size_t)STR(argv[i])->len); pos += STR(argv[i])->len; }
    return s;
}

/* ---------- vectors and arrays ---------- */

static val bi_create_vector(int argc, val *argv) {
    long n = need_count(argv[0]);
    return mk_vector(n, argc > 1 ? argv[1] : NIL);
}
static val bi_vector(int argc, val *argv) {
    val v = mk_vector(argc, NIL);
    int i;
    for (i = 0; i < argc; i++) VEC(v)->elt[i] = argv[i];
    return v;
}

static val bi_create_array(int argc, val *argv) {
    val dims = argv[0];
    val fill = argc > 1 ? argv[1] : NIL;
    long rank, total = 1, i;
    val d, dv, data;
    array_t *a;
    long sp = gc_sp;
    rank = list_length(dims);
    if (rank < 0) err_domain(dims, "<list>");
    for (d = dims; IS_CONS(d); d = CDR(d)) {
        long n = need_count(CAR(d));
        total *= n;
        if (total > HUGE_REQUEST) isl_error("<STORAGE-EXHAUSTED>", NULL);
    }
    if (rank == 1) return mk_vector(FIXVAL(CAR(dims)), fill);
    gc_push(fill);
    dv = mk_vector(rank, MKFIX(0));
    gc_push(dv);
    for (d = dims, i = 0; IS_CONS(d); d = CDR(d), i++) VEC(dv)->elt[i] = CAR(d);
    data = mk_vector(total, fill);
    gc_push(data);
    a = (array_t *)heap_alloc(T_ARRAY, sizeof(array_t));
    a->rank = (int)rank; a->dims = dv; a->data = data;
    gc_sp = sp;
    return (val)a;
}

static int is_basic_array(val v) { return IS_VEC(v) || IS_STR(v) || IS_ARR(v); }

/* Row-major offset of the index list argv[0..n) into a; signals on a bad index. */
static long array_offset(val a, int n, val *idx) {
    long off = 0;
    int i;
    if (IS_ARR(a)) {
        if (n != ARR(a)->rank) isl_error("<PROGRAM-ERROR>", sym_object, a, NULL);
        for (i = 0; i < n; i++) {
            long dim = FIXVAL(VEC(ARR(a)->dims)->elt[i]);
            if (!IS_INT(idx[i]) || int_sign(idx[i]) < 0) err_domain(idx[i], "<integer>");
            if (!IS_FIX(idx[i]) || FIXVAL(idx[i]) >= dim) err_index_out_of_range(a, idx[i]);
            off = off * dim + FIXVAL(idx[i]);
        }
        return off;
    }
    if (n != 1) isl_error("<PROGRAM-ERROR>", sym_object, a, NULL);
    if (!IS_INT(idx[0]) || int_sign(idx[0]) < 0) err_domain(idx[0], "<integer>");
    {
        long len = IS_VEC(a) ? VEC(a)->n : STR(a)->len;
        if (!IS_FIX(idx[0]) || FIXVAL(idx[0]) >= len) err_index_out_of_range(a, idx[0]);
    }
    return FIXVAL(idx[0]);
}

static val array_get(val a, long off) {
    if (IS_ARR(a)) return VEC(ARR(a)->data)->elt[off];
    if (IS_VEC(a)) return VEC(a)->elt[off];
    return MKCHR((unsigned char)STR(a)->data[off]);
}
static void array_set(val a, long off, val v) {
    need_mutable(a);
    if (IS_ARR(a)) { VEC(ARR(a)->data)->elt[off] = v; return; }
    if (IS_VEC(a)) { VEC(a)->elt[off] = v; return; }
    need_char(v);
    STR(a)->data[off] = (char)CHRVAL(v);
}

static val bi_aref(int argc, val *argv) {
    if (!is_basic_array(argv[0])) err_domain(argv[0], "<basic-array>");
    return array_get(argv[0], array_offset(argv[0], argc - 1, argv + 1));
}
static val bi_garef(int argc, val *argv) {
    if (!IS_VEC(argv[0]) && !IS_ARR(argv[0])) err_domain(argv[0], "<general-array*>");
    return array_get(argv[0], array_offset(argv[0], argc - 1, argv + 1));
}
static val bi_set_aref(int argc, val *argv) {
    if (!is_basic_array(argv[1])) err_domain(argv[1], "<basic-array>");
    array_set(argv[1], array_offset(argv[1], argc - 2, argv + 2), argv[0]);
    return argv[0];
}
static val bi_set_garef(int argc, val *argv) {
    if (!IS_VEC(argv[1]) && !IS_ARR(argv[1])) err_domain(argv[1], "<general-array*>");
    array_set(argv[1], array_offset(argv[1], argc - 2, argv + 2), argv[0]);
    return argv[0];
}
static val bi_array_dimensions(int argc, val *argv) {
    (void)argc;
    if (!is_basic_array(argv[0])) err_domain(argv[0], "<basic-array>");
    if (IS_VEC(argv[0])) return list1(MKFIX(VEC(argv[0])->n));
    if (IS_STR(argv[0])) return list1(MKFIX(STR(argv[0])->len));
    {
        val r = NIL;
        long i;
        long sp = gc_sp;
        long slot = gc_push(NIL);
        for (i = ARR(argv[0])->rank - 1; i >= 0; i--) { r = mk_cons(VEC(ARR(argv[0])->dims)->elt[i], r); gc_roots[slot] = r; }
        gc_sp = sp;
        return r;
    }
}

/* ---------- sequences (§25) ---------- */

static int is_sequence(val v) { return IS_LIST(v) || IS_VEC(v) || IS_STR(v); }
static long seq_length(val v) {
    if (IS_LIST(v)) { long n = list_length(v); if (n < 0) err_domain(v, "<list>"); return n; }
    if (IS_VEC(v)) return VEC(v)->n;
    return STR(v)->len;
}
static val seq_ref(val v, long i) {
    if (IS_LIST(v)) return list_nth(v, i);
    if (IS_VEC(v)) return VEC(v)->elt[i];
    return MKCHR((unsigned char)STR(v)->data[i]);
}
static void seq_set(val v, long i, val x) {
    need_mutable(v);
    if (IS_LIST(v)) { while (i-- > 0) v = CDR(v); CAR(v) = x; return; }
    if (IS_VEC(v)) { VEC(v)->elt[i] = x; return; }
    need_char(x);
    STR(v)->data[i] = (char)CHRVAL(x);
}
static long seq_index(val seq, val z) {
    long n = seq_length(seq);
    if (!IS_INT(z) || int_sign(z) < 0) err_domain(z, "<integer>");
    if (!IS_FIX(z) || FIXVAL(z) >= n) err_index_out_of_range(seq, z);
    return FIXVAL(z);
}

static val bi_elt(int argc, val *argv) {
    (void)argc;
    if (!is_sequence(argv[0])) err_domain(argv[0], "<list>");
    return seq_ref(argv[0], seq_index(argv[0], argv[1]));
}
static val bi_set_elt(int argc, val *argv) {
    (void)argc;
    if (!is_sequence(argv[1])) err_domain(argv[1], "<list>");
    seq_set(argv[1], seq_index(argv[1], argv[2]), argv[0]);
    return argv[0];
}
static val bi_subseq(int argc, val *argv) {
    long n, z1, z2, i;
    val seq = argv[0];
    (void)argc;
    if (!is_sequence(seq)) err_domain(seq, "<list>");
    n = seq_length(seq);
    if (!IS_INT(argv[1]) || int_sign(argv[1]) < 0) err_domain(argv[1], "<integer>");
    if (!IS_INT(argv[2]) || int_sign(argv[2]) < 0) err_domain(argv[2], "<integer>");
    if (!IS_FIX(argv[1]) || !IS_FIX(argv[2])) err_index_out_of_range(seq, argv[1]);
    z1 = FIXVAL(argv[1]); z2 = FIXVAL(argv[2]);
    if (z1 > n) err_index_out_of_range(seq, argv[1]);
    if (z2 < z1 || z2 > n) err_index_out_of_range(seq, argv[2]);
    if (IS_STR(seq)) return mk_string(STR(seq)->data + z1, z2 - z1);
    if (IS_VEC(seq)) {
        val v = mk_vector(z2 - z1, NIL);
        for (i = z1; i < z2; i++) VEC(v)->elt[i - z1] = VEC(seq)->elt[i];
        return v;
    }
    {
        val r = NIL;
        long sp = gc_sp;
        long slot = gc_push(NIL);
        for (i = z2 - 1; i >= z1; i--) { r = mk_cons(list_nth(seq, i), r); gc_roots[slot] = r; }
        gc_sp = sp;
        return r;
    }
}
static val bi_map_into(int argc, val *argv) {
    val dest = argv[0], fn = argv[1];
    long n, i;
    int j, nseq = argc - 2;
    if (!is_sequence(dest)) err_domain(dest, "<list>");
    if (!IS_FUNC(fn)) err_domain(fn, "<function>");
    n = seq_length(dest);
    for (j = 0; j < nseq; j++) {
        long m;
        if (!is_sequence(argv[2 + j])) err_domain(argv[2 + j], "<list>");
        m = seq_length(argv[2 + j]);
        if (m < n) n = m;
    }
    for (i = 0; i < n; i++) {
        long sp = gc_sp;
        long base = gc_sp;
        val r;
        for (j = 0; j < nseq; j++) gc_push(seq_ref(argv[2 + j], i));
        r = apply(fn, nseq, &gc_roots[base]);
        gc_sp = sp;
        seq_set(dest, i, r);
    }
    return dest;
}

void seq_init(void) {
    defbuiltin("CHAR=", bi_char_eq, 2, 2);
    defbuiltin("CHAR/=", bi_char_ne, 2, 2);
    defbuiltin("CHAR<", bi_char_lt, 2, 2);
    defbuiltin("CHAR>", bi_char_gt, 2, 2);
    defbuiltin("CHAR<=", bi_char_le, 2, 2);
    defbuiltin("CHAR>=", bi_char_ge, 2, 2);
    defbuiltin("CREATE-STRING", bi_create_string, 1, 2);
    defbuiltin("STRING=", bi_string_eq, 2, 2);
    defbuiltin("STRING/=", bi_string_ne, 2, 2);
    defbuiltin("STRING<", bi_string_lt, 2, 2);
    defbuiltin("STRING>", bi_string_gt, 2, 2);
    defbuiltin("STRING<=", bi_string_le, 2, 2);
    defbuiltin("STRING>=", bi_string_ge, 2, 2);
    defbuiltin("CHAR-INDEX", bi_char_index, 2, 3);
    defbuiltin("STRING-INDEX", bi_string_index, 2, 3);
    defbuiltin("STRING-APPEND", bi_string_append, 0, -1);
    defbuiltin("CREATE-VECTOR", bi_create_vector, 1, 2);
    defbuiltin("VECTOR", bi_vector, 0, -1);
    defbuiltin("CREATE-ARRAY", bi_create_array, 1, 2);
    defbuiltin("AREF", bi_aref, 1, -1);
    defbuiltin("GAREF", bi_garef, 1, -1);
    defbuiltin("SET-AREF", bi_set_aref, 2, -1);
    defbuiltin("SET-GAREF", bi_set_garef, 2, -1);
    defbuiltin("ARRAY-DIMENSIONS", bi_array_dimensions, 1, 1);
    defbuiltin("ELT", bi_elt, 2, 2);
    defbuiltin("SET-ELT", bi_set_elt, 3, 3);
    defbuiltin("SUBSEQ", bi_subseq, 3, 3);
    defbuiltin("MAP-INTO", bi_map_into, 2, -1);
}
