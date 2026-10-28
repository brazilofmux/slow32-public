// 6.8.4.2p5  switch on a _Bool that was assigned a nonzero value matches case 1
#include <stdio.h>
int main(void) {
    _Bool b = 5;
    int r = 0;
    switch (b) { case 1: r = 1; break; default: r = 2; }
    printf("%d\n", r);
    return 0;
}
