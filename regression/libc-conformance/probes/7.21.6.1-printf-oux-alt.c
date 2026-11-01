/* 7.21.6.1 fprintf: %o %u %x %X with the # flag, zero values and zero precision */
#include <stdio.h>

int main(void) {
    printf("[%o][%u][%x][%X]\n", 8u, 3000000000u, 0xabcdefu, 0xabcdefu);
    printf("[%#o][%#x][%#X][%#o][%#x][%#X]\n", 0u, 0u, 0u, 8u, 255u, 255u);
    printf("[%.0o][%.0x][%.0u][%#.0o][%#.0x][%5.0x]\n", 0u, 0u, 0u, 0u, 0u, 0u);
    printf("[%#.3o][%#.3o][%#5o][%#-6x][%#08x][%#8.4x]\n", 8u, 01234u, 8u, 255u, 255u, 255u);
    printf("[%#08o][%08X][%-8o][%.5u][%#llx][%#llo]\n", 9u, 0xbeefu, 9u, 42u, 0x123456789abcULL, 01ULL);
    return 0;
}
