/* Each literal lowers to its own ICONST, so `v[k-5]` written three
 * times used to be three subtractions against three different 5s,
 * three addresses and three loads.  CSE now treats equal constants as
 * one operand inside a block; run-tests.sh checks that pick3 loads
 * v[k-5] once.  The program checks that the merged value is right and
 * that a different literal (k-6) is not merged with it. */
static int tbl[12] = { 31, 4, 15, 9, 26, 5, 35, 8, 97, 93, 23, 84 };

int pick3(int *v, int k) {
    return (v[k - 5] << 2) + (v[k - 5] ^ 6) - (v[k - 5] >> 1);
}

int pick2(int *v, int k) {
    return v[k - 5] * 3 + v[k - 6];
}

int main(void) {
    int k;
    int a;
    for (k = 6; k < 12; k++) {
        a = tbl[k - 5];
        if (pick3(tbl, k) != (a << 2) + (a ^ 6) - (a >> 1)) return 1;
        if (pick2(tbl, k) != a * 3 + tbl[k - 6]) return 2;
    }
    return 0;
}
