// 6.10.3.2p2  # escapes " and \ inside string literals and char constants only
#include <stdio.h>
#define S(x) #x
int main(void) {
    puts(S("hi"));
    puts(S("a\"b\\c"));
    puts(S('"'));
    puts(S('\\'));
    puts(S('\n' "\t"));
    return 0;
}
