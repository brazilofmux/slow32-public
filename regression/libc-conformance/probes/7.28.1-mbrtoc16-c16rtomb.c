/* 7.28.1 mbrtoc16/c16rtomb: in the "C" locale an ASCII string converts to char16_t and back unchanged; the null character returns 0 */
#include <stdio.h>
#include <string.h>
#include <uchar.h>

int main(void) {
    const char *s = "Az9 ~";
    mbstate_t st;
    char16_t c;
    char out[8];
    size_t r;
    int i;
    memset(&st, 0, sizeof st);
    for (i = 0; i <= (int)strlen(s); i++) {
        r = mbrtoc16(&c, s + i, 1, &st);
        printf("mbrtoc16 '%c' -> %u, value %u\n", s[i] ? s[i] : '0', (unsigned)r, (unsigned)c);
    }
    memset(&st, 0, sizeof st);
    r = c16rtomb(out, (char16_t)'Q', &st);
    printf("c16rtomb Q -> %u [%c]\n", (unsigned)r, out[0]);
    printf("char16_t is unsigned 16+: %d\n", (char16_t)-1 > 0 && sizeof(char16_t) * 8 >= 16);
    return 0;
}
