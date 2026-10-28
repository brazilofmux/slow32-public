// 6.8.6.2p1  a switch body is not a loop body: continue there (outside any loop) is invalid
// expect: reject
int main(void) {
    int x = 1;
    switch (x) { case 1: continue; }
    return 0;
}
