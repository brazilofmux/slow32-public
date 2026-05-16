/* GNU statement expressions: ({ stmts; final_expr; }) returns the
 * value of the trailing expression statement.  Shared-frontend
 * support added under ISSUES.md #47.  Exercises:
 *   - stmt expr as initializer of a local int
 *   - stmt expr in a function's return statement
 *   - nested stmt expressions
 *   - stmt expr with a loop computing the trailing value
 *   - stmt expr modifying outer state (global) — verifies side
 *     effects in the body fire exactly once
 *   - stmt exprs as function arguments (one per arg slot) */

static int g_count = 0;

static int side(int v) {
    g_count = g_count + 1;
    return v;
}

static int max3(int a, int b, int c) {
    return ({
        int t1 = a > b ? a : b;
        int t2 = t1 > c ? t1 : c;
        t2;
    });
}

static int outer3(int a, int b, int c) {
    return a * 100 + b * 10 + c;
}

int main(void) {
    int x = ({ int a = 3; int b = 4; a * 10 + b; });   /* 34 */
    int y = max3(7, 11, 5);                            /* 11 */
    int z = ({
        int s = 0;
        int i;
        for (i = 1; i <= 5; i = i + 1) s = s + i;
        s;
    });                                                /* 15 */

    int nested = ({
        int inner = ({ int t = 7; t + 3; });
        inner * 4;
    });                                                /* 40 */

    g_count = 0;
    int sc = ({ side(2); side(3); side(4); g_count; });   /* 3 */

    int r = outer3(({ int a = 1; a + 1; }),
                   ({ int b = 2; b + 1; }),
                   ({ int c = 3; c + 1; }));
    /* outer3(2, 3, 4) = 234 */

    return (x + y + z + nested + sc + r) & 0xff;
    /* 34 + 11 + 15 + 40 + 3 + 234 = 337 → & 0xff = 81 */
}
