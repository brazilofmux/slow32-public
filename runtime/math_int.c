#include <stdlib.h>

int iabs(int x) {
    return (x < 0) ? -x : x;
}

int imin(int a, int b) {
    return (a < b) ? a : b;
}

int imax(int a, int b) {
    return (a > b) ? a : b;
}

int isqrt(int n) {
    if (n < 0) return -1;
    if (n == 0) return 0;
    
    int x = n;
    int y = (x + 1) >> 1;
    
    while (y < x) {
        x = y;
        y = (x + n / x) >> 1;
    }
    
    return x;
}

int ipow(int base, int exp) {
    if (exp < 0) return 0;
    
    int result = 1;
    while (exp) {
        if (exp & 1) result *= base;
        base *= base;
        exp >>= 1;
    }
    return result;
}