/* test_corpus.c -- Comprehensive C feature corpus for s12cc
 *
 * Tests increasingly complex patterns that real programs use.
 * Return code indicates first failing test (0 = all pass).
 *
 * Category 1xx: global variable patterns
 * Category 2xx: array and pointer patterns
 * Category 3xx: bitwise operations
 * Category 4xx: multi-level indirection
 * Category 5xx: complex control flow
 * Category 6xx: function call patterns
 * Category 7xx: struct patterns
 * Category 8xx: char/byte operations
 */

/* Global variables */
int g_a;
int g_b;
int g_arr[16];
char g_buf[32];
int g_count;

/* === Category 1: Global variable patterns === */

int test_global_readwrite(void) {
    /* Basic global read/write */
    g_a = 42;
    g_b = 100;
    if (g_a != 42) return 101;
    if (g_b != 100) return 102;
    if (g_a + g_b != 142) return 103;
    return 0;
}

int test_global_array(void) {
    int i;
    /* Write to global array */
    i = 0;
    while (i < 16) {
        g_arr[i] = i * i;
        i = i + 1;
    }
    /* Read back */
    if (g_arr[0] != 0) return 111;
    if (g_arr[1] != 1) return 112;
    if (g_arr[4] != 16) return 113;
    if (g_arr[15] != 225) return 114;
    return 0;
}

int test_global_accumulate(void) {
    int i;
    /* Accumulate into global */
    g_count = 0;
    i = 0;
    while (i < 10) {
        g_count = g_count + i;
        i = i + 1;
    }
    if (g_count != 45) return 121;
    return 0;
}

int test_global_char_array(void) {
    /* Write and read char array */
    g_buf[0] = 'H';
    g_buf[1] = 'i';
    g_buf[2] = 0;
    if (g_buf[0] != 'H') return 131;
    if (g_buf[1] != 'i') return 132;
    if (g_buf[2] != 0) return 133;
    return 0;
}

/* === Category 2: Array and pointer patterns === */

int test_local_array(void) {
    int arr[8];
    int i;
    i = 0;
    while (i < 8) {
        arr[i] = i + 10;
        i = i + 1;
    }
    if (arr[0] != 10) return 201;
    if (arr[7] != 17) return 202;
    /* Sum */
    i = 0;
    g_a = 0;
    while (i < 8) {
        g_a = g_a + arr[i];
        i = i + 1;
    }
    if (g_a != 108) return 203;
    return 0;
}

int test_ptr_deref(void) {
    int x;
    int *p;
    x = 99;
    p = &x;
    if (*p != 99) return 211;
    *p = 77;
    if (x != 77) return 212;
    return 0;
}

int test_ptr_arithmetic(void) {
    int arr[4];
    int *p;
    arr[0] = 10;
    arr[1] = 20;
    arr[2] = 30;
    arr[3] = 40;
    p = arr;
    if (*p != 10) return 221;
    p = p + 1;
    if (*p != 20) return 222;
    p = p + 2;
    if (*p != 40) return 223;
    return 0;
}

int test_array_via_ptr(void) {
    /* Access array elements via pointer indexing */
    int arr[4];
    int *p;
    arr[0] = 100;
    arr[1] = 200;
    arr[2] = 300;
    arr[3] = 400;
    p = arr;
    if (p[0] != 100) return 231;
    if (p[2] != 300) return 232;
    if (*(p + 3) != 400) return 233;
    return 0;
}

int test_char_ptr(void) {
    char buf[8];
    char *p;
    buf[0] = 'A';
    buf[1] = 'B';
    buf[2] = 'C';
    buf[3] = 0;
    p = buf;
    if (*p != 'A') return 241;
    p = p + 1;
    if (*p != 'B') return 242;
    if (p[1] != 'C') return 243;
    return 0;
}

/* === Category 3: Bitwise operations === */

int test_bitwise_and(void) {
    if ((0xFF & 0x0F) != 0x0F) return 301;
    if ((0xAB & 0xF0) != 0xA0) return 302;
    if ((255 & 0) != 0) return 303;
    return 0;
}

int test_bitwise_or(void) {
    if ((0xA0 | 0x0B) != 0xAB) return 311;
    if ((0 | 0xFF) != 0xFF) return 312;
    return 0;
}

int test_bitwise_xor(void) {
    if ((0xFF ^ 0xFF) != 0) return 321;
    if ((0xFF ^ 0x00) != 0xFF) return 322;
    if ((0xAA ^ 0x55) != 0xFF) return 323;
    return 0;
}

int test_bitwise_not(void) {
    int x;
    x = 0;
    x = ~x;
    if (x != -1) return 331;
    x = ~0xFF;
    /* ~0xFF = 0xFFFFFF00 = -256 */
    if (x != -256) return 332;
    return 0;
}

int test_shifts(void) {
    if ((1 << 0) != 1) return 341;
    if ((1 << 4) != 16) return 342;
    if ((1 << 20) != 1048576) return 343;
    if ((256 >> 4) != 16) return 344;
    if ((1024 >> 10) != 1) return 345;
    return 0;
}

int test_mask_extract(void) {
    /* Extract byte from 32-bit value — common in binary I/O */
    int val;
    int b0;
    int b1;
    int b2;
    int b3;
    val = 0x12345678;
    b0 = val & 0xFF;
    b1 = (val >> 8) & 0xFF;
    b2 = (val >> 16) & 0xFF;
    b3 = (val >> 24) & 0xFF;
    if (b0 != 0x78) return 351;
    if (b1 != 0x56) return 352;
    if (b2 != 0x34) return 353;
    if (b3 != 0x12) return 354;
    return 0;
}

int test_mask_insert(void) {
    /* Build 32-bit value from bytes — used in rd32/wr32 */
    int val;
    int b0;
    int b1;
    int b2;
    int b3;
    b0 = 0x78;
    b1 = 0x56;
    b2 = 0x34;
    b3 = 0x12;
    val = b0 | (b1 << 8) | (b2 << 16) | (b3 << 24);
    if (val != 0x12345678) return 361;
    return 0;
}

/* === Category 4: Multi-level indirection === */

int test_double_ptr(void) {
    int x;
    int *p;
    int **pp;
    x = 42;
    p = &x;
    pp = &p;
    if (**pp != 42) return 401;
    **pp = 99;
    if (x != 99) return 402;
    return 0;
}

int test_array_of_ptrs(void) {
    int a;
    int b;
    int c;
    int *ptrs[3];
    a = 10;
    b = 20;
    c = 30;
    ptrs[0] = &a;
    ptrs[1] = &b;
    ptrs[2] = &c;
    if (*ptrs[0] != 10) return 411;
    if (*ptrs[1] != 20) return 412;
    if (*ptrs[2] != 30) return 413;
    *ptrs[1] = 99;
    if (b != 99) return 414;
    return 0;
}

/* === Category 5: Complex control flow === */

int test_nested_if(void) {
    int x;
    int y;
    int r;
    x = 5;
    y = 10;
    r = 0;
    if (x > 0) {
        if (y > 0) {
            if (x < y) {
                r = 1;
            } else {
                r = 2;
            }
        } else {
            r = 3;
        }
    } else {
        r = 4;
    }
    if (r != 1) return 501;
    return 0;
}

int test_nested_while(void) {
    int i;
    int j;
    int sum;
    sum = 0;
    i = 0;
    while (i < 5) {
        j = 0;
        while (j < 5) {
            sum = sum + 1;
            j = j + 1;
        }
        i = i + 1;
    }
    if (sum != 25) return 511;
    return 0;
}

int test_break_continue(void) {
    int i;
    int sum;
    /* Sum even numbers 0..9, skip odd */
    sum = 0;
    i = 0;
    while (i < 10) {
        if (i % 2 != 0) {
            i = i + 1;
            continue;
        }
        sum = sum + i;
        i = i + 1;
    }
    if (sum != 20) return 521;

    /* Break when sum > 10 */
    sum = 0;
    i = 1;
    while (i < 100) {
        sum = sum + i;
        if (sum > 10) break;
        i = i + 1;
    }
    /* 1+2+3+4+5 = 15 > 10 */
    if (sum != 15) return 522;
    return 0;
}

int test_for_loop(void) {
    int i;
    int sum;
    sum = 0;
    for (i = 1; i <= 10; i = i + 1) {
        sum = sum + i;
    }
    if (sum != 55) return 531;
    return 0;
}

int test_do_while(void) {
    int n;
    int count;
    n = 1;
    count = 0;
    do {
        n = n * 2;
        count = count + 1;
    } while (n < 100);
    /* 2, 4, 8, 16, 32, 64, 128 → 7 iterations */
    if (count != 7) return 541;
    if (n != 128) return 542;
    return 0;
}

int test_short_circuit(void) {
    int x;
    int y;
    x = 5;
    y = 0;
    /* && short-circuits: y=0 so second part shouldn't matter */
    if (y && (x / y > 0)) return 551;
    /* || short-circuits: x=5 so second part shouldn't matter */
    if (!(x || (100 / y > 0))) return 552;
    return 0;
}

int test_ternary(void) {
    int x;
    int r;
    x = 10;
    r = (x > 5) ? 1 : 0;
    if (r != 1) return 561;
    r = (x < 5) ? 1 : 0;
    if (r != 0) return 562;
    return 0;
}

/* === Category 6: Function call patterns === */

int identity(int x) { return x; }
int add2(int a, int b) { return a + b; }
int add3(int a, int b, int c) { return a + b + c; }
int add4(int a, int b, int c, int d) { return a + b + c + d; }

int test_many_args(void) {
    if (identity(42) != 42) return 601;
    if (add2(10, 20) != 30) return 602;
    if (add3(10, 20, 30) != 60) return 603;
    if (add4(10, 20, 30, 40) != 100) return 604;
    return 0;
}

int fib(int n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

int test_recursion(void) {
    if (fib(0) != 0) return 611;
    if (fib(1) != 1) return 612;
    if (fib(10) != 55) return 613;
    return 0;
}

int apply(int (*fn)(int), int x) {
    return fn(x);
}

int square(int x) { return x * x; }
int negate(int x) { return 0 - x; }

int test_fn_ptr(void) {
    if (apply(square, 5) != 25) return 621;
    if (apply(negate, 42) != -42) return 622;
    return 0;
}

int modify_via_ptr(int *p) {
    *p = *p + 1;
    return *p;
}

int test_ptr_param(void) {
    int x;
    x = 10;
    modify_via_ptr(&x);
    if (x != 11) return 631;
    return 0;
}

/* === Category 7: Struct patterns === */

struct Point {
    int x;
    int y;
};

int test_struct_basic(void) {
    struct Point p;
    p.x = 10;
    p.y = 20;
    if (p.x != 10) return 701;
    if (p.y != 20) return 702;
    return 0;
}

int test_struct_ptr(void) {
    struct Point p;
    struct Point *pp;
    p.x = 100;
    p.y = 200;
    pp = &p;
    if (pp->x != 100) return 711;
    if (pp->y != 200) return 712;
    pp->x = 300;
    if (p.x != 300) return 713;
    return 0;
}

int test_struct_sizeof(void) {
    struct Point p;
    /* sizeof(struct Point) should be 8 (two ints) */
    if (sizeof(struct Point) != 8) return 721;
    if (sizeof(p) != 8) return 722;
    return 0;
}

/* === Category 8: Char/byte operations === */

int test_char_sign_extend(void) {
    char c;
    int i;
    c = -1;
    i = c;
    /* char should sign extend to -1 as int */
    if (i != -1) return 801;
    c = 127;
    i = c;
    if (i != 127) return 802;
    return 0;
}

int test_char_unsigned_mask(void) {
    char c;
    int val;
    c = -128;
    /* Mask to get unsigned byte value */
    val = c & 0xFF;
    if (val != 128) return 811;
    c = -1;
    val = c & 0xFF;
    if (val != 255) return 812;
    return 0;
}

int my_strlen(char *s) {
    int n;
    n = 0;
    while (s[n] != 0) {
        n = n + 1;
    }
    return n;
}

int test_string_ops(void) {
    char buf[16];
    buf[0] = 'A';
    buf[1] = 'B';
    buf[2] = 'C';
    buf[3] = 0;
    if (my_strlen(buf) != 3) return 821;
    return 0;
}

/* === Category 9: Compound operations (used heavily in real programs) === */

int test_compound_assign(void) {
    int x;
    x = 10;
    x += 5;
    if (x != 15) return 901;
    x -= 3;
    if (x != 12) return 902;
    x *= 2;
    if (x != 24) return 903;
    x /= 3;
    if (x != 8) return 904;
    x %= 5;
    if (x != 3) return 905;
    x <<= 4;
    if (x != 48) return 906;
    x >>= 2;
    if (x != 12) return 907;
    x &= 0xF;
    if (x != 12) return 908;
    x |= 0x30;
    if (x != 60) return 909;
    x ^= 0xFF;
    if (x != 195) return 910;
    return 0;
}

int test_postfix(void) {
    int x;
    int y;
    x = 5;
    y = x++;
    if (y != 5) return 921;
    if (x != 6) return 922;
    y = x--;
    if (y != 6) return 923;
    if (x != 5) return 924;
    return 0;
}

int test_prefix(void) {
    int x;
    int y;
    x = 5;
    y = ++x;
    if (y != 6) return 931;
    if (x != 6) return 932;
    y = --x;
    if (y != 5) return 933;
    if (x != 5) return 934;
    return 0;
}

/* === Category 10: Patterns from real linker code === */

/* Simulate rd32: read 32-bit LE from char buffer */
int rd32(char *buf, int off) {
    int b0;
    int b1;
    int b2;
    int b3;
    b0 = buf[off] & 255;
    b1 = buf[off + 1] & 255;
    b2 = buf[off + 2] & 255;
    b3 = buf[off + 3] & 255;
    return b0 | (b1 << 8) | (b2 << 16) | (b3 << 24);
}

/* Simulate wr32: write 32-bit LE to char buffer */
void wr32(char *buf, int off, int val) {
    buf[off] = val & 255;
    buf[off + 1] = (val >> 8) & 255;
    buf[off + 2] = (val >> 16) & 255;
    buf[off + 3] = (val >> 24) & 255;
}

int rd16(char *buf, int off) {
    int b0;
    int b1;
    b0 = buf[off] & 255;
    b1 = buf[off + 1] & 255;
    return b0 | (b1 << 8);
}

void wr16(char *buf, int off, int val) {
    buf[off] = val & 255;
    buf[off + 1] = (val >> 8) & 255;
}

int test_rd_wr_32(void) {
    char buf[8];
    int val;
    /* Write 0x12345678 and read it back */
    wr32(buf, 0, 0x12345678);
    val = rd32(buf, 0);
    if (val != 0x12345678) return 1001;

    /* Write at offset 4 */
    wr32(buf, 4, 0xDEADBEEF);
    val = rd32(buf, 4);
    /* 0xDEADBEEF as signed int = -559038737 */
    if (val != -559038737) return 1002;

    /* Write 0 */
    wr32(buf, 0, 0);
    if (rd32(buf, 0) != 0) return 1003;

    /* Write -1 (all ones) */
    wr32(buf, 0, -1);
    if (rd32(buf, 0) != -1) return 1004;

    return 0;
}

int test_rd_wr_16(void) {
    char buf[4];
    wr16(buf, 0, 0x1234);
    if (rd16(buf, 0) != 0x1234) return 1011;
    wr16(buf, 2, 0xABCD);
    if (rd16(buf, 2) != 0xABCD) return 1012;
    return 0;
}

/* Simulate relocation bit manipulation */
int test_reloc_bits(void) {
    int insn;
    int val;
    int offset;
    int target;

    /* HI20 relocation: val = (target + 0x800) >> 12, insert into bits 12..31 */
    target = 0x12345;
    val = (target + 0x800) >> 12;
    insn = 0x00000037;  /* base LUI instruction */
    insn = (insn & 0xFFF) | (val << 12);
    /* val = (0x12345 + 0x800) >> 12 = 0x12B45 >> 12 = 0x12 */
    /* insn = 0x037 | (0x12 << 12) = 0x037 | 0x12000 = 0x12037 */
    if (insn != 0x12037) return 1021;

    /* LO12 relocation: val = target & 0xFFF, insert into bits 20..31 */
    val = target & 0xFFF;
    insn = 0x00000013;  /* base ADDI instruction */
    insn = (insn & 0xFFFFF) | (val << 20);
    /* val = 0x345 */
    /* insn = 0x13 | (0x345 << 20) = 0x34500013 */
    if (insn != 0x34500013) return 1022;

    return 0;
}

/* Simulate page alignment */
int page_align(int addr) {
    return (addr + 4095) & (~4095);
}

int test_page_align(void) {
    if (page_align(0) != 0) return 1031;
    if (page_align(1) != 4096) return 1032;
    if (page_align(4096) != 4096) return 1033;
    if (page_align(4097) != 8192) return 1034;
    if (page_align(0x10000) != 0x10000) return 1035;
    return 0;
}

/* Test global + function interaction (common pattern in linker) */
int g_sym_count;
int g_sym_vals[8];
char *g_sym_names[8];

void add_sym(char *name, int val) {
    g_sym_names[g_sym_count] = name;
    g_sym_vals[g_sym_count] = val;
    g_sym_count = g_sym_count + 1;
}

int find_sym(char *name) {
    int i;
    i = 0;
    while (i < g_sym_count) {
        if (g_sym_names[i][0] == name[0] && g_sym_names[i][1] == name[1]) {
            return g_sym_vals[i];
        }
        i = i + 1;
    }
    return -1;
}

int test_sym_table(void) {
    g_sym_count = 0;
    add_sym("ab", 100);
    add_sym("cd", 200);
    add_sym("ef", 300);
    if (find_sym("ab") != 100) return 1041;
    if (find_sym("cd") != 200) return 1042;
    if (find_sym("ef") != 300) return 1043;
    if (find_sym("zz") != -1) return 1044;
    return 0;
}

/* === Main === */

int main(void) {
    int rc;

    /* Category 1: Globals */
    rc = test_global_readwrite();
    if (rc) return rc;
    rc = test_global_array();
    if (rc) return rc;
    rc = test_global_accumulate();
    if (rc) return rc;
    rc = test_global_char_array();
    if (rc) return rc;

    /* Category 2: Arrays and pointers */
    rc = test_local_array();
    if (rc) return rc;
    rc = test_ptr_deref();
    if (rc) return rc;
    rc = test_ptr_arithmetic();
    if (rc) return rc;
    rc = test_array_via_ptr();
    if (rc) return rc;
    rc = test_char_ptr();
    if (rc) return rc;

    /* Category 3: Bitwise */
    rc = test_bitwise_and();
    if (rc) return rc;
    rc = test_bitwise_or();
    if (rc) return rc;
    rc = test_bitwise_xor();
    if (rc) return rc;
    rc = test_bitwise_not();
    if (rc) return rc;
    rc = test_shifts();
    if (rc) return rc;
    rc = test_mask_extract();
    if (rc) return rc;
    rc = test_mask_insert();
    if (rc) return rc;

    /* Category 4: Multi-level indirection */
    rc = test_double_ptr();
    if (rc) return rc;
    rc = test_array_of_ptrs();
    if (rc) return rc;

    /* Category 5: Control flow */
    rc = test_nested_if();
    if (rc) return rc;
    rc = test_nested_while();
    if (rc) return rc;
    rc = test_break_continue();
    if (rc) return rc;
    rc = test_for_loop();
    if (rc) return rc;
    rc = test_do_while();
    if (rc) return rc;
    rc = test_short_circuit();
    if (rc) return rc;
    rc = test_ternary();
    if (rc) return rc;

    /* Category 6: Function calls */
    rc = test_many_args();
    if (rc) return rc;
    rc = test_recursion();
    if (rc) return rc;
    rc = test_fn_ptr();
    if (rc) return rc;
    rc = test_ptr_param();
    if (rc) return rc;

    /* Category 7: Structs */
    rc = test_struct_basic();
    if (rc) return rc;
    rc = test_struct_ptr();
    if (rc) return rc;
    rc = test_struct_sizeof();
    if (rc) return rc;

    /* Category 8: Char/byte ops */
    rc = test_char_sign_extend();
    if (rc) return rc;
    rc = test_char_unsigned_mask();
    if (rc) return rc;
    rc = test_string_ops();
    if (rc) return rc;

    /* Category 9: Compound operations */
    rc = test_compound_assign();
    if (rc) return rc;
    rc = test_postfix();
    if (rc) return rc;
    rc = test_prefix();
    if (rc) return rc;

    /* Category 10: Real linker patterns */
    rc = test_rd_wr_32();
    if (rc) return rc;
    rc = test_rd_wr_16();
    if (rc) return rc;
    rc = test_reloc_bits();
    if (rc) return rc;
    rc = test_page_align();
    if (rc) return rc;
    rc = test_sym_table();
    if (rc) return rc;

    return 0;
}
