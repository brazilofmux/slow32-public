/* test_duff.c -- Test: goto label inside switch before first case.
 * This is the "Duff's device" pattern that Ragel -G2 generates.
 * If s32-cc crashes on this, we need to restructure the Ragel output.
 */
int fputs(char *s, int f);
int fputc(int c, int f);
void fput_uint(int f, int v);
#define stderr 2

int test_duff(int x) {
    int r;
    r = 0;
    switch (x) {
    lbl_a:
        r = r + 10;
        goto done;
    case 1:
        goto lbl_a;
    case 2:
        r = 20;
        goto done;
    }
done:
    return r;
}

int main(void) {
    int v;
    v = test_duff(1);
    if (v != 10) {
        fputs("FAIL: duff expected 10 got ", stderr);
        fput_uint(stderr, v);
        fputc(10, stderr);
        return 1;
    }
    v = test_duff(2);
    if (v != 20) {
        fputs("FAIL: duff expected 20 got ", stderr);
        fput_uint(stderr, v);
        fputc(10, stderr);
        return 1;
    }
    return 0;
}
