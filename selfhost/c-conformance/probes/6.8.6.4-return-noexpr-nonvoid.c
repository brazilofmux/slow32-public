// 6.8.6.4p1  a return without an expression shall only appear in a void function
// expect: reject
static int f(int x) { if (x) return; return 2; }
int main(void) { return f(0); }
