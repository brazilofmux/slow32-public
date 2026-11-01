/* 7.24.4.3 strcoll: in the "C" locale it orders as strcmp does, including bytes above 127 */
#include <stdio.h>
#include <string.h>

static int sgn(int v) { return (v > 0) - (v < 0); }

int main(void) {
    static const char *w[] = { "", "a", "B", "b", "ab", "aB", "\x80", "\xff", "Z", "a\xe9" };
    int i, j, bad = 0;
    for (i = 0; i < 10; i++)
        for (j = 0; j < 10; j++)
            if (sgn(strcoll(w[i], w[j])) != sgn(strcmp(w[i], w[j]))) bad++;
    printf("disagreements with strcmp: %d\n", bad);
    return 0;
}
