/* Mirror of mandel.f: every hot value is a double scalar local. */
int main(void) {
    double x, y, zr, zi, t, s;
    int i, j, k, n = 0;
    for (j = 1; j <= 200; j++) {
        y = (double)j / 100.0 - 1.0;
        for (i = 1; i <= 200; i++) {
            x = (double)i / 100.0 - 1.5;
            zr = 0.0; zi = 0.0;
            for (k = 1; k <= 60; k++) {
                t = zr*zr - zi*zi + x;
                zi = 2.0*zr*zi + y;
                zr = t;
                s = zr*zr + zi*zi;
                if (s > 4.0) goto next;
            }
            n++;
        next: ;
        }
    }
    return (n != 15756) ? 1 : 0;
}
