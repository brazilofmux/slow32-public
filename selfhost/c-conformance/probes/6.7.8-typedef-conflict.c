// 6.7p3  a typedef redeclared as a different type
// expect: reject
typedef int T;
typedef long T;
int main(void) { return 0; }
