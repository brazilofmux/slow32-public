/* Pins the two libc holes Doom found in one evening: sscanf existed
 * only as a prototype, and printf ignored precision on integers
 * ("STCFN%.3d" produced STCFN33 and the HUD font failed to load). */
#include <stdio.h>

int main(void) {
    int a = 0, b = 0, c = 0, d = 0;
    char buf[16];

    if (sscanf("  1234", " %d", &a) != 1 || a != 1234) {
        printf("FAIL d\n");
        return 1;
    }
    if (sscanf("0x1A2B", "%i", &b) != 1 || b != 0x1A2B) {
        printf("FAIL i-hex\n");
        return 1;
    }
    if (sscanf("0755", "%i", &c) != 1 || c != 0755) {
        printf("FAIL i-oct\n");
        return 1;
    }
    if (sscanf("dead", "%x", &d) != 1 || d != 0xdead) {
        printf("FAIL x\n");
        return 1;
    }
    snprintf(buf, sizeof(buf), "STCFN%.3d", 33);
    if (buf[5] != '0' || buf[6] != '3' || buf[7] != '3') {
        printf("FAIL precision: %s\n", buf);
        return 1;
    }
    printf("%s %.5d %.0d|\n", buf, -42, 0);
    return 0;
}
