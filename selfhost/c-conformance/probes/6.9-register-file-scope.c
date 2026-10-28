// 6.9p2  register shall not appear in an external declaration
// expect: reject
register int x;
int main(void) { return 0; }
