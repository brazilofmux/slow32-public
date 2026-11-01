/* 7.24.4.5 strxfrm: n == 0 with a null s1 gives the length, the result orders as strcoll, too small an n returns >= n */
#include <stdio.h>
#include <string.h>

static int sgn(int v) { return (v > 0) - (v < 0); }

int main(void) {
    static const char *w[] = { "", "a", "B", "b", "ab", "\x80", "\xff", "hello" };
    char x[8][64], small[4];
    size_t len[8];
    int i, j, bad = 0, badlen = 0;
    for (i = 0; i < 8; i++) {
        len[i] = strxfrm(NULL, w[i], 0);
        if (len[i] >= sizeof x[i]) { printf("too long\n"); return 1; }
        memset(x[i], '#', sizeof x[i]);
        if (strxfrm(x[i], w[i], len[i] + 1) != len[i]) badlen++;
        if (strlen(x[i]) != len[i]) badlen++;
    }
    for (i = 0; i < 8; i++)
        for (j = 0; j < 8; j++)
            if (sgn(strcmp(x[i], x[j])) != sgn(strcoll(w[i], w[j]))) bad++;
    printf("lengths inconsistent %d, order disagreements %d\n", badlen, bad);
    printf("too small: %d\n", strxfrm(small, "hello", 3) >= 3);
    printf("exact fit untouched beyond n: ");
    memset(small, '#', sizeof small);
    strxfrm(small, "a", 2);
    printf("%c\n", small[2]);
    return 0;
}
