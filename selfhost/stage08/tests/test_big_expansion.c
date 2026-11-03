/* One macro expansion longer than the fixed 8192 bytes pp_exp had: the
 * rest was dropped without a word, so this initializer came out short
 * and the program wrong (xBase report.c: "undeclared identifier
 * 'expr_c'" where the cut fell).  16KB in one __VA_ARGS__. */
#define B1 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
#define B2 B1,B1,B1,B1,B1,B1,B1,B1,B1,B1,B1,B1,B1,B1,B1,B1
#define B3 B2,B2,B2,B2,B2,B2,B2,B2,B2,B2,B2,B2,B2,B2,B2,B2
#define ALL(...) __VA_ARGS__
static const char ones[] = { ALL(B3, B3, 2) };
int main(void) {
    int i, s = 0;
    for (i = 0; i < (int)sizeof ones; i++) s += ones[i];
    return s == 2 * 4096 + 2 && sizeof ones == 8193 ? 0 : 1;
}
