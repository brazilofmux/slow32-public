/* GitHub issue 77: block-scope locals reuse frame space.
 * Two sibling 80-int arrays would need 640 bytes of private slots;
 * with reuse the frame stays one array plus a few scalars.  The reads
 * go through an argument's index: with constant indexes the loads
 * forward and the arrays are not kept at all. */
int sibling(int k) {
    int r;
    r = 0;
    {
        int a[80];
        a[0] = 1;
        a[79] = 2;
        r = r + a[k] + a[79 - k];
    }
    {
        int b[80];
        b[0] = 3;
        b[79] = 4;
        r = r + b[k] + b[79 - k];
    }
    return r;
}

int nested(int k) {
    int a[40];
    int r;
    a[0] = 5;
    a[39] = 6;
    {
        int b[40];
        b[0] = 7;
        b[39] = 8;
        r = a[k] + a[39 - k] + b[k] + b[39 - k];
    }
    return r;
}

int main(void) {
    if (sibling(0) != 10) return 1;
    if (nested(0) != 26) return 2;
    return 0;
}
