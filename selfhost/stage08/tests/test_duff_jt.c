/* GitHub issue 51: nested case labels are fall-through.  Five cases so
 * a jump table is eligible if hl_switch_has_fallthrough only walks
 * top-level siblings (case 0, then the do-while). */
static int duff(int c) {
    int n;
    n = 0;
    switch (c) {
    case 0:
        do {
            n = n + 1;
        case 1:
            n = n + 2;
        case 2:
            n = n + 4;
        case 3:
            n = n + 8;
        case 4:
            n = n + 16;
        } while (0);
    }
    return n;
}

int main(void) {
    if (duff(0) != 31) return 1;
    if (duff(1) != 30) return 2;
    if (duff(2) != 28) return 3;
    if (duff(3) != 24) return 4;
    if (duff(4) != 16) return 5;
    return 0;
}
