/* A macro parameter's name inside a string or character literal in the
 * body is text, not a parameter.  regal's TEST_EQ_STR printed
 * "(got '%s', expected '%s')" and `expected` came out as the argument. */
#include <string.h>
#include <stdio.h>
#define SHOW(actual, expected) sprintf(buf, "got %s, expected %s", actual, expected)
#define CH(x) (x == 'x' ? 1 : 0)
int main(void) {
    char buf[64];
    SHOW("a", "b");
    if (strcmp(buf, "got a, expected b") != 0) return 1;
    if (CH('x') != 1 || CH('y') != 0) return 2;
    return 0;
}
