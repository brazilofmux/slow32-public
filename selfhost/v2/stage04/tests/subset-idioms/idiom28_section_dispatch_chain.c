int main(void) {
    int secs[6];
    int i;
    int t;
    int d;
    int r;
    int b;

    secs[0] = 1;
    secs[1] = 2;
    secs[2] = 4;
    secs[3] = 3;
    secs[4] = 2;
    secs[5] = 1;

    t = 0;
    d = 0;
    r = 0;
    b = 0;

    for (i = 0; i < 6; i = i + 1) {
        if (secs[i] == 1) {
            t = t + 1;
        } else if (secs[i] == 2) {
            d = d + 2;
        } else if (secs[i] == 3) {
            b = b + 3;
        } else if (secs[i] == 4) {
            r = r + 4;
        }
    }

    if (t != 2) return 1;
    if (d != 4) return 2;
    if (b != 3) return 3;
    if (r != 4) return 4;
    return 0;
}
