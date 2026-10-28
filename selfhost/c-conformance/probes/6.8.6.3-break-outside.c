// 6.8.6.3p1  break shall appear only in or as a switch or loop body
// expect: reject
int main(void) {
    int x = 1;
    if (x) break;
    return 0;
}
