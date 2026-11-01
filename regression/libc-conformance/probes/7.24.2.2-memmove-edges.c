/* 7.24.2.2 memmove: length 0 changes nothing, s1 == s2 is harmless, disjoint moves and a large overlapping move */
#include <stdio.h>
#include <string.h>

static char big[3000];

int main(void) {
    char a[16] = "abcdefghijklmno";
    int i, bad = 0;
    printf("%d ", memmove(a + 3, a + 1, 0) == a + 3);
    printf("%s ", a);
    memmove(a + 2, a + 2, 5);
    printf("%s ", a);
    memmove(a, a + 8, 4);
    printf("%s\n", a);
    for (i = 0; i < 3000; i++) big[i] = (char)(i % 251);
    memmove(big + 1001, big + 1, 1999);
    for (i = 1001; i < 3000; i++) if (big[i] != (char)((i - 1000) % 251)) bad++;
    for (i = 0; i < 1001; i++) if (big[i] != (char)(i % 251)) bad++;
    memmove(big, big + 3, 2997);
    for (i = 0; i < 998; i++) if (big[i] != (char)((i + 3) % 251)) bad++;
    printf("large overlapping: %d wrong\n", bad);
    return 0;
}
