/* 7.21.7.2 fgets: reads at most n-1 characters, keeps the newline, NUL-terminates, returns NULL with the array unchanged at immediate EOF */
#include <stdio.h>
#include <string.h>

int main(void) {
    FILE *f = fopen("lines.txt", "w+");
    if (!f) { printf("fopen failed\n"); return 1; }
    fputs("abcdefgh\nxy\n\nlast", f);
    rewind(f);
    char b[16];
    char *p = fgets(b, 5, f);
    printf("%d [%s]\n", p == b, b);
    p = fgets(b, sizeof b, f);
    printf("%d [%s]\n", p == b, b);
    p = fgets(b, sizeof b, f);
    printf("%d [%s]\n", p == b, b);
    p = fgets(b, 4, f);
    printf("%d [%s] %d\n", p == b, b, (int)strlen(b));
    p = fgets(b, sizeof b, f);
    printf("%d [%s] %d\n", p == b, b, feof(f) != 0);
    p = fgets(b, sizeof b, f);
    printf("%d [%s] %d\n", p == NULL, b, feof(f) != 0);
    strcpy(b, "keep");
    p = fgets(b, sizeof b, f);
    printf("%d [%s]\n", p == NULL, b);
    fclose(f);
    remove("lines.txt");
    return 0;
}
