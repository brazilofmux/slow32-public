// 6.5.16p2/6.7.3  storing through a pointer to const is a constraint violation
// expect: reject
int main(void) { int x = 1; const int *p = &x; *p = 2; return x; }
