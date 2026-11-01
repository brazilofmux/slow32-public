/* 7.21.5.4 freopen: closes the stream's file, opens the named one on the same stream object, clears the indicators */
#include <stdio.h>

int main(void) {
    FILE *f = fopen("fa.txt", "w");
    fputs("alpha\n", f);
    fclose(f);
    f = fopen("fb.txt", "w");
    fputs("unflushed", f);
    FILE *g = freopen("fa.txt", "r", f);
    printf("%d\n", g == f);
    char b[16] = "";
    fgets(b, sizeof b, g);
    printf("[%s", b);
    printf("%d %d\n", fgetc(g) == EOF, feof(g) != 0);
    g = freopen("fb.txt", "r", g);
    printf("%d %d ", g != NULL, g ? feof(g) != 0 : -1);
    if (g && fgets(b, sizeof b, g)) printf("[%s]\n", b);
    else printf("nothing\n");
    if (g) fclose(g);
    FILE *s = freopen("fa.txt", "r", stdin);
    printf("%d ", s == stdin);
    if (fgets(b, sizeof b, stdin)) printf("[%s", b);
    remove("fa.txt");
    remove("fb.txt");
    return 0;
}
