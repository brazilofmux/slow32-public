// 6.8.6.1p1  the label named by goto shall exist in the enclosing function
// expect: reject
int main(void) {
    goto nowhere;
    return 0;
}
int pad(void) { return 1; }
