/* 7.24.5.8 strtok: skips leading and consecutive delimiters, delimiter set may change between calls, null at the end and after */
#include <stdio.h>
#include <string.h>

static void p(const char *t) { printf("[%s]", t ? t : "NULL"); }

int main(void) {
    char a[] = ",,ab,,c;d,,";
    char b[] = "key=val;x=y";
    char c[] = ",,,";
    char d[] = "solo";
    p(strtok(a, ",")); p(strtok(NULL, ",")); p(strtok(NULL, ",")); p(strtok(NULL, ",")); p(strtok(NULL, ","));
    putchar('\n');
    p(strtok(b, "=")); p(strtok(NULL, ";")); p(strtok(NULL, "=")); p(strtok(NULL, ""));
    p(strtok(NULL, "="));
    putchar('\n');
    p(strtok(c, ",")); p(strtok(NULL, ","));
    p(strtok(d, ",")); p(strtok(NULL, ","));
    putchar('\n');
    /* the terminator is written over the delimiter */
    {
        char e[] = "x y";
        strtok(e, " ");
        printf("%d %c\n", e[1] == '\0', e[2]);
    }
    return 0;
}
