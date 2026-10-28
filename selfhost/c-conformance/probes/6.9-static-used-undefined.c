// 6.9p3  an internal-linkage function used in an expression shall have exactly one definition
// expect: reject
static int f(void);
int main(void) { return f(); }
