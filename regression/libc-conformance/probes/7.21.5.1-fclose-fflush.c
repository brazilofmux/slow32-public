/* 7.21.5.1-2 fclose fflush: fclose writes buffered data and returns 0; fflush makes written data visible to another stream; fflush(NULL) flushes all */
#include <stdio.h>
#include <string.h>

static void peek(const char *name) {
    FILE *g = fopen(name, "r");
    char b[32] = "";
    if (g) { if (!fgets(b, sizeof b, g)) b[0] = 0; fclose(g); }
    printf("[%s]", b);
}

int main(void) {
    FILE *f = fopen("fl.txt", "w");
    FILE *h = fopen("fl2.txt", "w");
    fputs("buffered", f);
    fputs("other", h);
    printf("%d ", fflush(f));
    peek("fl.txt");
    fputs("+more", f);
    printf(" %d ", fflush(NULL));
    peek("fl.txt");
    peek("fl2.txt");
    fputs("!", f);
    printf(" %d ", fclose(f));
    peek("fl.txt");
    printf(" %d\n", fclose(h));
    remove("fl.txt");
    remove("fl2.txt");
    return 0;
}
