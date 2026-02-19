#define SEL(c, t, f) ((c) ? (t) : (f))
#define TOINT(x) ((int)(x))

int main(void) {
    char a[4];
    a[0] = 1;
    a[1] = 9;
    a[2] = 3;
    a[3] = 7;
    int v = TOINT(SEL(1, a[1], a[2]));
    int w = TOINT(SEL(0, a[1], a[2]));
    return (v == 9 && w == 3) ? 0 : 1;
}
