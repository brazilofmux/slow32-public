// 6.8.1p3  label names shall be unique within a function (even in different blocks)
// expect: reject
int main(void) {
    int x = 0;
    { here: x++; }
    { here: x++; }
    return x;
}
