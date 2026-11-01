/* 7.21.7.2 fgets: with n == 1 no character is read, the array gets just the null character, and s is returned */
#include <stdio.h>

int main(void) {
    FILE *f = fopen("one.txt", "w+");
    fputs("xyz", f);
    rewind(f);
    char b[4] = "abc";
    char *p = fgets(b, 1, f);
    printf("%d %d %c\n", p == b, b[0], fgetc(f));
    fclose(f);
    remove("one.txt");
    return 0;
}
