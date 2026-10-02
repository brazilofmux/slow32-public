/* A prompt is seen before its answer is awaited: output waiting in
 * line-buffered stdout is sent before the library asks the host for
 * input on stdin.  The prompt has no newline, so nothing else sends it;
 * stderr, which is not buffered, marks the moment the input arrived.
 * (Both streams are read as one; the input is libc-tests/stdio_prompt.in.) */
#include <stdio.h>
#include <string.h>

int main(void) {
    char line[64];
    int c;

    printf("name? ");
    if (!fgets(line, 64, stdin)) return 1;
    fprintf(stderr, "[read a line]\n");
    line[strlen(line) - 1] = 0;
    printf("hello, %s\n", line);

    printf("one character? ");
    c = getchar();
    fprintf(stderr, "[read a character]\n");
    printf("%c\n", c);

    printf("the rest? ");
    c = (int)fread(line, 1, 63, stdin);
    fprintf(stderr, "[read to the end]\n");
    line[c] = 0;
    printf("%d characters: %s", c, line);
    return 0;
}
