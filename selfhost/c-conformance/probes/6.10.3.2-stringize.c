// 6.10.3.2p2  # : inner white space becomes one space, outer is dropped, spellings kept
#include <stdio.h>
#define S(x) #x
int main(void) {
    puts(S(  a   +    b  ));
    puts(S(f( 1 ,2 )));
    puts(S(0x1F 1.5e3 'c'));
    puts(S(a
           b));
    return 0;
}
