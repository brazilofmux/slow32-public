/* 7.21.4.3 tmpfile: opens a temporary binary file for update (wb+) that can be written, rewound and read */
#include <stdio.h>

int main(void) {
    FILE *f = tmpfile();
    printf("%d\n", f != NULL);
    if (!f) return 1;
    printf("%d\n", fputs("temp\x01\x00z", f) >= 0);
    fwrite("\0end", 1, 4, f);
    rewind(f);
    int c, n = 0, zero = 0;
    while ((c = fgetc(f)) != EOF) { n++; if (c == 0) zero++; }
    printf("%d %d\n", n, zero);
    printf("%d\n", fclose(f));
    return 0;
}
