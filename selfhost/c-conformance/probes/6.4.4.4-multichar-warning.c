// 6.4.4.4p10  a character constant of more than one character is implementation-defined, and clang warns of it
// expect: warnings
#include <stdio.h>
#define TAG 'rt'
int main(void) {
    int two = 'ok';
    int one = 'k';
    printf("%d %d %d\n", two, one, TAG + 'z');
    return 0;
}
