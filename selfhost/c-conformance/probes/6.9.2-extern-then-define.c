// 6.9.2p1-2  extern declarations followed by a definition with an initializer; size completed later
#include <stdio.h>
extern int x;
extern int tab[];
static int get(void) { return x + tab[2]; }
int x = 40;
int tab[] = { 0, 1, 2, 3 };
extern int x;
int main(void) {
    printf("%d %d\n", get(), (int)(sizeof tab / sizeof tab[0]));
    return 0;
}
