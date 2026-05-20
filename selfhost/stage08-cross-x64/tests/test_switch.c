/* Test switch/case with multiple cases and default */

int classify(int x) {
    switch (x) {
        case 0: return 10;
        case 1: return 20;
        case 2: return 30;
        case 5: return 40;
        default: return 99;
    }
}

int main(int argc, char **argv) {
    int total;
    total = 0;
    total = total + classify(0);   /* 10 */
    total = total + classify(2);   /* 30 */
    total = total + classify(7);   /* 99 → default */
    /* 10 + 30 + 99 = 139, mod 128 = 11 */
    return total % 128;            /* 11 */
}
