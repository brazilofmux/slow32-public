int main(void) {
    int types[6];
    int symbols[6];
    int i;
    int j;
    int paired;
    int npaired;

    /* 2 = HI20, 3 = LO12 */
    types[0] = 2; symbols[0] = 7;
    types[1] = 3; symbols[1] = 7;
    types[2] = 3; symbols[2] = 8;
    types[3] = 2; symbols[3] = 9;
    types[4] = 2; symbols[4] = 8;
    types[5] = 3; symbols[5] = 9;

    npaired = 0;
    for (i = 0; i < 6; i = i + 1) {
        if (types[i] != 3) continue;
        paired = 0;
        for (j = 0; j < 6; j = j + 1) {
            if (types[j] == 2 && symbols[j] == symbols[i]) {
                paired = 1;
                break;
            }
        }
        if (paired) npaired = npaired + 1;
    }

    return (npaired == 3) ? 0 : 1;
}
