#include <stdio.h>
#include <string.h>

int main(void) {
    FILE *f;
    char buf[8];
    long pos;
    int n;
    int fails = 0;

    f = fopen("ug.txt", "w");
    if (!f) {
        printf("fopen w failed\n");
        return 1;
    }
    fputs("ABCD", f);
    fclose(f);

    f = fopen("ug.txt", "r");
    if (!f) {
        printf("fopen r failed\n");
        return 1;
    }
    if (fgetc(f) != 'A') {
        fails |= 1;
    }
    if (ungetc('Z', f) != 'Z') {
        fails |= 2;
    }
    n = (int)fread(buf, 1, 3, f);
    if (n != 3 || buf[0] != 'Z' || buf[1] != 'B' || buf[2] != 'C') {
        fails |= 4;
    }
    fclose(f);

    f = fopen("ug.txt", "r");
    if (!f) {
        printf("fopen r2 failed\n");
        return 1;
    }
    (void)fgetc(f);
    (void)ungetc('Z', f);
    pos = ftell(f);
    if (pos != 0) {
        fails |= 8;
    }
    if (fseek(f, 0, SEEK_SET) != 0) {
        fails |= 16;
    }
    if (fgetc(f) != 'A') {
        fails |= 32;
    }
    fclose(f);

    /* Clean up (stdio-buffering and stdlib-memstream do the same):
     * this runs with the harness's cwd, so a leftover lands in
     * regression/ as an untracked stray. */
    remove("ug.txt");

    if (fails) {
        printf("FAIL %d\n", fails);
        return 1;
    }
    printf("ok\n");
    return 0;
}
