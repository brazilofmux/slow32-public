// 6.8.4.2p5  controlling expr is promoted; case values converted to the promoted type
#include <stdio.h>
int main(void) {
    unsigned char uc = 255;
    signed char sc = -1;
    unsigned int u = 0xFFFFFFFFu;
    int r = 0;
    switch (uc) { case -1: r |= 1; break; case 255: r |= 2; break; }
    switch (sc) { case 255: r |= 4; break; case -1: r |= 8; break; }
    switch (u) { case -1: r |= 16; break; default: r |= 32; }
    switch ((short)-2) { case 65534: r |= 64; break; case -2: r |= 128; }
    enum e { A = 3, B = 7 } ev = B;
    switch (ev) { case A: r |= 1024; break; case B: r |= 2048; break; }
    printf("%d\n", r);
    return 0;
}
