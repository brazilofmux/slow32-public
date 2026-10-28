// 6.7p3  an identifier with no linkage declared twice in the same scope
// expect: reject
int main(void) { int a = 1; int a = 2; return a; }
