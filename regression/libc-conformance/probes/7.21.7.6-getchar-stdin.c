/* 7.21.7.6 getchar: reads stdin, ungetc onto stdin works, fgets on stdin, then EOF and feof(stdin) at the end */
#include <stdio.h>

int main(void) {
    int c = getchar();
    printf("%c ", c);
    printf("%d ", ungetc('F', stdin));
    char b[32] = "";
    fgets(b, sizeof b, stdin);
    printf("[%s", b);
    printf("%c", getchar());
    fgets(b, sizeof b, stdin);
    printf("[%s", b);
    printf("%d %d\n", getchar() == EOF, feof(stdin) != 0);
    return 0;
}
