// 6.10.3.2p2,5.1.1.2p3  comments in an argument are white space before # sees them
#include <stdio.h>
#define S(x) #x
int main(void) {
    puts(S(a/**/b));
    puts(S(c /* long
               comment */ d));
    puts(S(e // line comment
           f));
    return 0;
}
