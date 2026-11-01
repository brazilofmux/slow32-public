/* 7.21.7.10 ungetc: one character of pushback is guaranteed, it need not match what was read, ungetc(EOF) fails, and the file is unchanged */
#include <stdio.h>

int main(void) {
    FILE *f = fopen("ung.txt", "w+");
    if (!f) { printf("fopen failed\n"); return 1; }
    fputs("abc", f);
    rewind(f);
    int c = fgetc(f);
    printf("%c %d", c, ungetc('Z', f));
    printf(" %c %c\n", fgetc(f), fgetc(f));
    printf("%d\n", ungetc(EOF, f) == EOF);
    printf("%c\n", fgetc(f));
    printf("%d %d\n", ungetc(0x1a5, f), fgetc(f));
    rewind(f);
    char b[8] = "";
    fgets(b, sizeof b, f);
    printf("[%s]\n", b);
    fclose(f);
    remove("ung.txt");
    return 0;
}
