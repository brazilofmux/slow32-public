// 6.4.1p2  a keyword cannot be used as an identifier
// expect: reject
int main(void) { int while = 1; return while; }
