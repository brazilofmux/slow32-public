// 6.7.6.2p2  an object of static storage duration may not have a variable length array type, even at block scope
// expect: reject
int n = 3;
int f(void) { static int a[n]; return a[0]; }
int main(void) { return f(); }
