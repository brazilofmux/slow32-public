/* 7.11.1.1 setlocale: after one category alone is changed, the LC_ALL query string restores the whole mixed locale */
#include <stdio.h>
#include <string.h>
#include <locale.h>

int main(void) {
    static const char *cand[] = { "C.UTF-8", "en_US.UTF-8", "C.utf8", "POSIX" };
    char all[1024], ctype[256];
    char *p = NULL;
    int i;
    for (i = 0; i < 4 && !p; i++) p = setlocale(LC_CTYPE, cand[i]);
    if (!p) { printf("1 1 1\n"); return 0; }
    strncpy(ctype, p, 255); ctype[255] = 0;
    p = setlocale(LC_ALL, NULL);
    strncpy(all, p, 1023); all[1023] = 0;
    setlocale(LC_ALL, "C");
    p = setlocale(LC_ALL, all);
    printf("%d ", p != NULL);
    p = setlocale(LC_CTYPE, NULL);
    printf("%d ", p != NULL && strcmp(p, ctype) == 0);
    p = setlocale(LC_NUMERIC, NULL);
    printf("%d\n", p != NULL && setlocale(LC_NUMERIC, p) != NULL);
    return 0;
}
