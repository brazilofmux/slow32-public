// 6.8.4.2p1  a pointer is not an integer type for switch
// expect: reject
int main(void) {
    int x, *p = &x;
    switch (p) { default: return 1; }
    return 0;
}
