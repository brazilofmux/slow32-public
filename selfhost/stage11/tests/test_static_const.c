/* Test static const int x = N as compile-time constant */

static const int ANSWER = 42;
static const int NEGVAL = -7;

int main(void) {
    int x;

    /* Use as constant in expressions */
    x = ANSWER;
    if (x != 42) return 1;

    /* Use negative value */
    x = NEGVAL;
    if (x != -7) return 2;

    /* Use in array size context (just as a value) */
    x = ANSWER + NEGVAL;
    if (x != 35) return 3;

    return 0;
}
