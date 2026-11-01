/* 7.21.1 FOPEN_MAX: the minimum number of files that can be open at once, at least eight; that many can be opened */
#include <stdio.h>

int main(void) {
    FILE *f[FOPEN_MAX];
    int n = 0;
    printf("%d\n", FOPEN_MAX >= 8);
    for (int i = 0; i < FOPEN_MAX - 3; i++) {
        char name[16];
        snprintf(name, sizeof name, "om%d.txt", i);
        f[i] = fopen(name, "w");
        if (f[i]) n++;
    }
    printf("%d\n", n == FOPEN_MAX - 3);
    for (int i = 0; i < FOPEN_MAX - 3; i++) {
        char name[16];
        snprintf(name, sizeof name, "om%d.txt", i);
        if (f[i]) fclose(f[i]);
        remove(name);
    }
    return 0;
}
