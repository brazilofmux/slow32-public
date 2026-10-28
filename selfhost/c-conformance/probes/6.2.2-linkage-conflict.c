// 6.2.2p7  the same identifier with both internal and external linkage is undefined; clang rejects static after extern
// expect: reject
int v;
static int v;
int main(void) { return v; }
