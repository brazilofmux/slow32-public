// 6.4.5p5-p6  adjacent literals concatenate; a NUL is appended; embedded escapes and \0
#include <stdio.h>
#include <string.h>
int main(void) {
    const char *s = "ab" "cd"
                    "ef";
    char t[] = "x\0y";
    char hexsplit[] = "\x41" "B";        /* concatenation happens after escapes: 'A','B' */
    printf("%s %d %d %d %s %d\n", s, (int)strlen(s), (int)sizeof t, t[2], hexsplit, (int)sizeof "a" "b");
    return 0;
}
