/* Negative literals in a global aggregate initializer (selfhost ISSUES-54).
 *
 * `-` is TK-PUNCT, and PARSE-GLOBAL-INIT's P-MINUS arm sat outside the
 * TK-PUNCT branch, so it never ran: {-7, 3} came out as .word 0, .word 7,
 * .word 3. The arm now lives inside the punctuation branch.
 */

int    n[2] = {-7, 3};
int    z[3] = {0, -1, -2147483647};
static int w[2] = {-5, -6};

int main(void) {
    if (n[0] != -7) return 1;
    if (n[1] != 3) return 2;
    if (z[0] != 0) return 3;
    if (z[1] != -1) return 4;
    if (z[2] != -2147483647) return 5;
    if (w[0] != -5) return 6;
    if (w[1] != -6) return 7;
    return 0;
}
