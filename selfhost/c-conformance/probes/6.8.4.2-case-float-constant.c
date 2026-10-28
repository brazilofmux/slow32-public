// 6.8.4.2p3  a floating constant is not an integer constant expression for case
// expect: reject
int main(void) {
    int x = 1;
    switch (x) { case 1.5: return 1; }
    return 0;
}
