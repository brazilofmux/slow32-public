/* 7.21.5.3 fopen: r fails on a missing file, w truncates, r+ keeps contents, w+ truncates, b variants accepted */
#include <stdio.h>

static void show(const char *name) {
    FILE *f = fopen(name, "rb");
    if (!f) { printf("(none)\n"); return; }
    int c;
    putchar('[');
    while ((c = fgetc(f)) != EOF) putchar(c);
    printf("]\n");
    fclose(f);
}

int main(void) {
    remove("m.txt");
    printf("%d\n", fopen("m.txt", "r") == NULL);
    FILE *f = fopen("m.txt", "w");
    fputs("first", f);
    fclose(f);
    show("m.txt");
    f = fopen("m.txt", "r+");
    fputs("FI", f);
    fclose(f);
    show("m.txt");
    f = fopen("m.txt", "w");
    fputs("xy", f);
    fclose(f);
    show("m.txt");
    f = fopen("m.txt", "w+");
    fclose(f);
    show("m.txt");
    const char *modes[] = { "rb", "wb", "ab", "r+b", "rb+", "w+b", "wb+", "a+b", "ab+" };
    for (int i = 0; i < 9; i++) {
        f = fopen("m.txt", modes[i]);
        printf("%s:%d ", modes[i], f != NULL);
        if (f) fclose(f);
    }
    printf("\n");
    remove("m.txt");
    return 0;
}
