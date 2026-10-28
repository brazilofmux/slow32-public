// 6.9.1p6  no storage class other than register in the declaration list
// expect: reject
int f(a) static int a; { return a; }
int main(void) { return f(0); }
