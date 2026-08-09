/* Preprocessor predefs + #error smoke (dialect map 2026-08). */

#ifndef __STDC__
int missing_stdc(void) { return 1; }
#else
int missing_stdc(void) { return 0; }
#endif

#ifndef __S12CC__
int missing_s12cc(void) { return 1; }
#else
int missing_s12cc(void) { return 0; }
#endif

/* Object-like value from #define still works alongside predefs */
#define TEN 10

int line_a(void) {
    return __LINE__;
}

int line_b(void) {
    return __LINE__;
}

int file_nonempty(void) {
    char *f;
    f = __FILE__;
    if (f == 0) return 0;
    if (f[0] == 0) return 0;
    return 1;
}

/* #if defined on a predef */
#if defined(__STDC__)
int stdc_if(void) { return 1; }
#else
int stdc_if(void) { return 0; }
#endif

/* #undef still works */
#define SCRATCH 1
#undef SCRATCH
#ifdef SCRATCH
int scratch_alive(void) { return 1; }
#else
int scratch_alive(void) { return 0; }
#endif

/* #elif chain */
#define PICK 2
#if PICK == 1
int pick_val(void) { return 1; }
#elif PICK == 2
int pick_val(void) { return 2; }
#else
int pick_val(void) { return 0; }
#endif

/* # stringize and ## paste */
#define STR(x) #x
#define PASTE(a, b) a##b
#define PREFIX_ID(name) PASTE(id_, name)

static int id_42 = 42;

int stringize_ok(void) {
    char *s;
    s = STR(hello);
    /* "hello" */
    if (s[0] != 'h') return 0;
    if (s[1] != 'e') return 0;
    if (s[4] != 'o') return 0;
    if (s[5] != 0) return 0;
    return 1;
}

int paste_ok(void) {
    return PREFIX_ID(42);
}

int main(void) {
    int a;
    int b;
    if (missing_stdc()) return 1;
    if (missing_s12cc()) return 2;
    if (TEN != 10) return 3;
    a = line_a();
    b = line_b();
    if (b <= a) return 4;
    if (!file_nonempty()) return 5;
    if (!stdc_if()) return 6;
    if (scratch_alive()) return 7;
    if (pick_val() != 2) return 8;
    if (!stringize_ok()) return 9;
    if (paste_ok() != 42) return 10;
    return 0;
}
