// 6.8.6.2p1  continue shall appear only in or as a loop body
// expect: reject
int main(void) {
    int x = 1;
    if (x) continue;
    return 0;
}
