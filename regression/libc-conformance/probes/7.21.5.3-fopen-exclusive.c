/* 7.21.5.3 fopen: a mode with x (wx, wbx, w+x) fails if the file exists and otherwise creates it */
#include <stdio.h>

int main(void) {
    remove("ex.txt");
    FILE *f = fopen("ex.txt", "wx");
    printf("%d\n", f != NULL);
    if (f) { fputs("made", f); fclose(f); }
    f = fopen("ex.txt", "wx");
    printf("%d\n", f == NULL);
    if (f) fclose(f);
    f = fopen("ex.txt", "w+bx");
    printf("%d\n", f == NULL);
    if (f) fclose(f);
    f = fopen("ex.txt", "r");
    char b[8] = "";
    if (f) { fgets(b, sizeof b, f); fclose(f); }
    printf("[%s]\n", b);
    remove("ex.txt");
    return 0;
}
