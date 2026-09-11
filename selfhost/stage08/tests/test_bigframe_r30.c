/* GitHub issue 73: r30 is an allocatable callee-saved register on the
 * omit-fp path.  A frame over 2047 bytes that takes the r30 color pops
 * the stack with li+add through a scratch register in the epilogue;
 * that scratch was r1, the return value.  sqlite3VdbeExec returned its
 * frame size instead of rc on every statement. */
int sink(int *p) { return p[0] + p[3000]; }

int bigframe(int a, int b, int c, int d, int e, int f, int g, int h) {
    int buf[3100];
    int v0, v1, v2, v3, v4, v5, v6, v7, v8, v9;
    int w0, w1, w2, w3, w4, w5, w6, w7, w8, w9;
    int r;
    buf[0] = a; buf[3000] = b;
    v0 = a * 3; v1 = b * 5; v2 = c * 7; v3 = d * 11; v4 = e * 13;
    v5 = f * 17; v6 = g * 19; v7 = h * 23; v8 = a + b; v9 = c + d;
    w0 = a ^ b; w1 = b ^ c; w2 = c ^ d; w3 = d ^ e; w4 = e ^ f;
    w5 = f ^ g; w6 = g ^ h; w7 = h ^ a; w8 = a - b; w9 = c - d;
    r = sink(buf);            /* every v/w value lives across this call */
    r += v0 + v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9;
    r += w0 + w1 + w2 + w3 + w4 + w5 + w6 + w7 + w8 + w9;
    r += sink(buf);
    return r;
}

int main(void) {
    int r;
    r = bigframe(1, 2, 3, 4, 5, 6, 7, 8);
    /* sink twice: 2*(1+2)=6; v: 3+10+21+44+65+102+133+184+3+7=572;
     * w: 3+1+7+1+3+1+15+9-1-1=38 */
    if (r != 6 + 572 + 38) return 1;
    return 0;
}
