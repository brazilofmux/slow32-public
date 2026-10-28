// 6.8.4.2p3  a case label whose value is not known at compile time is refused
// expect: reject
int main(void) {
    int x = 1, y = 1;
    switch (x) { case y: return 1; }
    return 0;
}
