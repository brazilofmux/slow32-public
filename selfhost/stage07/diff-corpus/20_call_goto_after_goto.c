static int sink;

static void side(int x) {
    sink = sink + x;
}

static int f(int x) {
    int p;
    int cs;

    p = 0;
    cs = 0;
    if (x) {
tr:
        side(p);
        {
            p = p + 1;
            cs = 15;
            goto out;
        }
        goto st;
st:
        p = p + 100;
    }
out:
    return p + cs;
}

int main(void) {
    if (f(1) != 16) return 1;
    return 0;
}
