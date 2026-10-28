// 6.9.1p6  an old-style definition may declare only names in its parameter list
// expect: reject
int f(a) int a; int b; { return a; }
int main(void) { return f(0); }
