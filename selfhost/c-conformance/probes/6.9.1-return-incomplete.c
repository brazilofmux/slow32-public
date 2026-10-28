// 6.9.1p3  the return type of a defined function shall be complete
// expect: reject
struct later;
struct later f(void) { }
int main(void) { return 0; }
