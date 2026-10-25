/* while and for with a condition are lowered bottom-tested: the
 * condition is lowered twice, once as the entry test and once after
 * the body.  Each evaluation must still happen exactly when the source
 * says: count them through a side effect, take continue (to the step,
 * then the test) and break, run loops of zero trips, nest them, and
 * leave through both exits with two live values (strrchr's shape). */
static int tests_made;

static int below(int i, int n) {
    tests_made = tests_made + 1;
    return i < n;
}

static char *lastof(char *s, int c) {
    char *last;
    last = (char *)0;
    while (*s) {
        if (*s == (char)c) last = s;
        s = s + 1;
    }
    if (c == 0) return s;
    return last;
}

int main(void) {
    int i;
    int j;
    int sum;
    int k;
    char word[12];
    char *p;

    tests_made = 0;
    sum = 0;
    for (i = 0; below(i, 5); i++) sum = sum + i;
    if (sum != 10 || tests_made != 6) return 1;

    tests_made = 0;
    for (i = 9; below(i, 5); i++) sum = sum + 100;
    if (sum != 10 || tests_made != 1) return 2;

    tests_made = 0;
    i = 0;
    while (below(i, 7)) {
        i = i + 1;
        if (i % 2) continue;
        sum = sum + i;
    }
    if (sum != 22 || tests_made != 8) return 3;

    sum = 0;
    for (i = 0; i < 10; i++) {
        if (i == 3) continue;
        if (i == 7) break;
        sum = sum + i;
    }
    if (sum != 18 || i != 7) return 4;

    sum = 0;
    for (i = 0; i < 4; i++)
        for (j = i; j < 4; j++)
            sum = sum + j;
    if (sum != 20) return 5;

    k = 0;
    i = 3;
    while ((i = i - 1) >= 0) k = k + 10;
    if (k != 30 || i != -1) return 6;

    word[0] = 'w'; word[1] = 'o'; word[2] = 'm'; word[3] = 'b';
    word[4] = 'a'; word[5] = 't'; word[6] = 'o'; word[7] = 0;
    p = lastof(word, 'o');
    if (p != word + 6) return 7;
    if (lastof(word, 'q') != (char *)0) return 8;
    if (lastof(word, 0) != word + 7) return 9;
    word[0] = 0;
    if (lastof(word, 'o') != (char *)0) return 10;
    if (lastof(word, 0) != word) return 11;
    return 0;
}
