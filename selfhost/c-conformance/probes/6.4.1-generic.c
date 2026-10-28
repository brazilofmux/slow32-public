// 6.4.1p1  _Generic keyword (6.5.1.1) selects by controlling expression type
#include <stdio.h>
#define KIND(x) _Generic((x), int: "int", double: "double", char *: "str", default: "other")
int main(void) {
    char *s = "s";
    printf("%s %s %s %s\n", KIND(1), KIND(1.0), KIND(s), KIND(1L));
    return 0;
}
