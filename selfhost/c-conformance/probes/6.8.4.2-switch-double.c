// 6.8.4.2p1  the controlling expression of switch shall have integer type
// expect: reject
int main(void) {
    double d = 1.0;
    switch (d) { case 1: return 1; }
    return 0;
}
