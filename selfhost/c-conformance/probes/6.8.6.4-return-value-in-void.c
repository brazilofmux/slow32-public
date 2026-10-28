// 6.8.6.4p1  a return with an expression shall not appear in a void function
// expect: reject
static void f(void) { return 1; }
int main(void) { f(); return 0; }
