/* 7.21.9.2 and 7.21.9.4 fseek ftell: SEEK_SET SEEK_CUR SEEK_END on a binary stream, ftell after writes and reads */
#include <stdio.h>

int main(void) {
    FILE *f = fopen("seek.bin", "wb+");
    if (!f) { printf("fopen failed\n"); return 1; }
    printf("%ld ", ftell(f));
    fputs("0123456789", f);
    printf("%ld\n", ftell(f));
    printf("%d %c %ld\n", fseek(f, 3, SEEK_SET), fgetc(f), ftell(f));
    printf("%d %c %ld\n", fseek(f, 2, SEEK_CUR), fgetc(f), ftell(f));
    printf("%d %c %ld\n", fseek(f, -1, SEEK_CUR), fgetc(f), ftell(f));
    printf("%d %c %ld\n", fseek(f, -2, SEEK_END), fgetc(f), ftell(f));
    printf("%d %ld\n", fseek(f, 0, SEEK_END), ftell(f));
    printf("%d\n", fseek(f, -100, SEEK_SET) != 0);
    fseek(f, 4, SEEK_SET);
    fputc('X', f);
    printf("%ld ", ftell(f));
    rewind(f);
    char b[16] = "";
    fgets(b, sizeof b, f);
    printf("[%s]\n", b);
    fclose(f);
    remove("seek.bin");
    return 0;
}
