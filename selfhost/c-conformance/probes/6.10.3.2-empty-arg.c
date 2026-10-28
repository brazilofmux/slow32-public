// 6.10.3.2p2  # applied to an empty argument gives ""
#include <stdio.h>
#define BR(a, b) "[" #a "|" #b "]"
#define ONE(a) #a
int main(void) {
    printf("%s %s %s %s\n", BR(,), BR(x,), BR(,y), ONE());
    return 0;
}
