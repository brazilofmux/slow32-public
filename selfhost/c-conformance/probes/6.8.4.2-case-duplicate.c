// 6.8.4.2p3  no two case constants in one switch may have the same value
// expect: reject
int main(void) {
    int x = 1;
    switch (x) { case 1: return 1; case 2 - 1: return 2; }
    return 0;
}
