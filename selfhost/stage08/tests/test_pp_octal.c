/* A numeric #define body is folded by the preprocessor and re-emitted as
 * its value; it read 0170000 as decimal 170000, so sys/stat.h's
 * S_ISREG() was false for every regular file (regal's template loader
 * found no templates).  #if arithmetic used the same reader. */
#define OCT_MODE 0100000
#define OCT_MASK 0170000
#define OCT_SMALL 017
#define DEC 100000
unsigned int g_mode = 0x81a4;
int main(void) {
    unsigned int m = g_mode;
    if (OCT_MODE != 32768) return 1;
    if (OCT_MASK != 61440) return 2;
    if (OCT_SMALL != 15) return 3;
    if (DEC != 100000) return 4;
    if (((m & OCT_MASK) == OCT_MODE) != 1) return 5;
#if OCT_SMALL != 15
    return 6;
#endif
#if (OCT_MASK & OCT_MODE) != 0100000
    return 7;
#endif
    return 0;
}
