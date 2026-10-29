// 6.3.1.3p2-3  a constant changed by an implicit conversion to a narrower integer type: clang warns
// expect: warnings
#include <stdio.h>
static unsigned short us = 70000;
static unsigned short neg = -1;
static signed char sc = 200;
static signed char oct = 0377;
static short hex = 0x8000;
static short dec = 40000;
static unsigned char arr[3] = { 255, 256, -1 };
static struct { char c; unsigned short h; } st = { 300, 65536 };
static unsigned char pass(unsigned char v) { return v; }
static unsigned char back(void) { return 511; }
int main(void) {
    unsigned short x;
    signed char y = 128;
    x = 65535 + 2;
    printf("%u %u %d %d %d %d %u %u %u %d %u %u %d %u %u\n", us, neg, sc, oct, hex, dec,
           arr[0], arr[1], arr[2], st.c, st.h, x, y, pass(258), back());
    return 0;
}
