/* 7.21.5.3 fopen: on an update stream, a positioning call between reads and writes lets both work on the same file */
#include <stdio.h>

int main(void) {
    FILE *f = fopen("upd.txt", "w+");
    fputs("abcdefgh", f);
    fseek(f, 0, SEEK_SET);
    printf("%c%c", fgetc(f), fgetc(f));
    fseek(f, 0, SEEK_CUR);
    fputs("XY", f);
    fflush(f);
    printf("%c", fgetc(f));
    fseek(f, -1, SEEK_END);
    printf("%c", fgetc(f));
    fseek(f, 0, SEEK_CUR);
    fputs("!!", f);
    rewind(f);
    char b[16] = "";
    fgets(b, sizeof b, f);
    printf(" [%s]\n", b);
    fclose(f);
    remove("upd.txt");
    return 0;
}
