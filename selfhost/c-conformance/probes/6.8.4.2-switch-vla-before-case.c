// 6.8.4.2p2  a VLA declared at the head of a switch body puts every case label inside its scope
// expect: reject
int f(int n, int k) {
    switch (k) {
        int a[n];
    case 1:
        a[0] = k;
        return a[0];
    }
    return 0;
}
int main(void) { return f(2, 1); }
