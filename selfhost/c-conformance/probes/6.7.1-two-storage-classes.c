// 6.7.1p2  at most one storage-class specifier (static extern)
// expect: reject
static extern int x;
int main(void) { return 0; }
