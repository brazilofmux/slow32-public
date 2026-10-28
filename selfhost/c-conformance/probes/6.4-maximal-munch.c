// 6.4p4  x+++++y tokenizes as x ++ ++ + y: ++ applied to a non-lvalue (6.5.3.1p1)
// expect: reject
int main(void) { int x = 1, y = 2; return x+++++y; }
