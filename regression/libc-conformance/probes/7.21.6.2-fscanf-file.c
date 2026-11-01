/* 7.21.6.2 fscanf: reads from a stream, leaves the first unmatched character unread, returns EOF at end of file */
#include <stdio.h>

int main(void) {
    FILE *f = fopen("scan.txt", "w+");
    if (!f) { printf("fopen failed\n"); return 1; }
    fputs("12 abc 3.5\nx", f);
    rewind(f);
    int a = 0;
    char w[8] = "";
    double d = 0;
    int r = fscanf(f, "%d %7s %lf", &a, w, &d);
    printf("%d: %d %s %g\n", r, a, w, d);
    r = fscanf(f, "%d", &a);
    printf("%d %c\n", r, fgetc(f));
    printf("%d\n", fscanf(f, "%d", &a) == EOF);
    fclose(f);
    remove("scan.txt");
    return 0;
}
