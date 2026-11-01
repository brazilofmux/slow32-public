/* 7.21.9.1 and 7.21.9.3 fgetpos fsetpos: a saved position restores reading at the same place; both return zero on success */
#include <stdio.h>

int main(void) {
    FILE *f = fopen("pos.txt", "w+");
    fputs("line one\nline two\n", f);
    rewind(f);
    fpos_t p;
    fgetc(f); fgetc(f);
    printf("%d", fgetpos(f, &p));
    char b[16] = "";
    fgets(b, sizeof b, f);
    printf(" [%s", b);
    printf("%d", fsetpos(f, &p));
    fgets(b, sizeof b, f);
    printf(" [%s", b);
    ungetc('#', f);
    printf("%d %c\n", fsetpos(f, &p), fgetc(f));
    fseek(f, 0, SEEK_END);
    fgetc(f);
    printf("%d ", feof(f) != 0);
    fsetpos(f, &p);
    printf("%d\n", feof(f) != 0);
    fclose(f);
    remove("pos.txt");
    return 0;
}
