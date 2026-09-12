/* GitHub issue 77: block-scope locals reuse frame space.
 * Two sibling 80-int arrays would need 640 bytes of private slots;
 * with reuse the frame stays one array plus a few scalars. */
int sibling(void) {
    int r;
    r = 0;
    {
        int a[80];
        a[0] = 1;
        a[79] = 2;
        r = r + a[0] + a[79];
    }
    {
        int b[80];
        b[0] = 3;
        b[79] = 4;
        r = r + b[0] + b[79];
    }
    return r;
}

int nested(void) {
    int a[40];
    int r;
    a[0] = 5;
    a[39] = 6;
    {
        int b[40];
        b[0] = 7;
        b[39] = 8;
        r = a[0] + a[39] + b[0] + b[39];
    }
    return r;
}

int main(void) {
    if (sibling() != 10) return 1;
    if (nested() != 26) return 2;
    return 0;
}
