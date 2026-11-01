/* 7.21.10 feof ferror clearerr: end of file is set only by reading past the end, stays set (fgetc returns EOF), and clearerr resets it */
#include <stdio.h>

int main(void) {
    FILE *f = fopen("eof.txt", "w+");
    fputs("a", f);
    rewind(f);
    printf("%d %d ", feof(f) != 0, ferror(f) != 0);
    printf("%c %d ", fgetc(f), feof(f) != 0);
    printf("%d %d\n", fgetc(f) == EOF, feof(f) != 0);
    FILE *w = fopen("eof.txt", "a");
    fputs("b", w);
    fclose(w);
    printf("%d %d ", fgetc(f) == EOF, feof(f) != 0);
    clearerr(f);
    printf("%d %c %d\n", feof(f) != 0, fgetc(f), ferror(f) != 0);
    fclose(f);
    remove("eof.txt");
    return 0;
}
