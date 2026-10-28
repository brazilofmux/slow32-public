// 6.8.1p1 / 6.8.2p1  a label must be followed by a statement; a label right before } is a syntax error in C11
// expect: reject
int main(void) {
    int n = 1;
    if (n) goto out;
    n = 2;
out:
}
