/* 7.21.7.3-9 fputc fputs putc putchar puts: return values, puts appends a newline, fputs does not, putc writes (unsigned char)c */
#include <stdio.h>

int main(void) {
    int r1 = fputs("no newline", stdout);
    int r2 = puts("|then puts");
    int r3 = putchar('A');
    int r4 = putc('\n', stdout);
    int r5 = fputc(0x142, stdout);
    int r6 = putchar(0x10a);
    printf("%d %d %d %d %d %d\n", r1 >= 0, r2 >= 0, r3, r4, r5, r6);
    int r7 = puts("");
    int r8 = fputs("", stdout);
    printf("%d %d\n", r7 >= 0, r8 >= 0);
    return 0;
}
