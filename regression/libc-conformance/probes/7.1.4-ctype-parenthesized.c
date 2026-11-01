/* 7.1.4p1: each <ctype.h> function is a real function, called with its name in parentheses and through its address */
#include <ctype.h>
#include <stdio.h>

typedef int (*cfn)(int);

int main(void) {
    cfn f[] = { &isalnum, &isalpha, &isblank, &iscntrl, &isdigit, &isgraph, &islower,
                &isprint, &ispunct, &isspace, &isupper, &isxdigit, &tolower, &toupper };
    const char *s = "a Z9\t!";
    unsigned i, j;
    for (i = 0; i < sizeof f / sizeof f[0]; i++) {
        for (j = 0; s[j]; j++) {
            int v = f[i]((unsigned char)s[j]);
            printf("%d", i < 12 ? v != 0 : v);
            putchar(j + 1 < 6 ? ',' : '\n');
        }
    }
    printf("%d %d %c %c\n", (isdigit)('3') != 0, (isspace)('x') != 0, (toupper)('q'), (tolower)('Q'));
    return 0;
}
