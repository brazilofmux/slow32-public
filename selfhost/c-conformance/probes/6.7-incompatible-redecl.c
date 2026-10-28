// 6.7p4  two declarations of the same object with incompatible types
// expect: reject
extern int x;
extern long x;
int main(void) { return 0; }
