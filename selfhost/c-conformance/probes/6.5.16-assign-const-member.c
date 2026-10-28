// 6.5.16p2  a struct with a const member is not a modifiable lvalue
// expect: reject
struct S { const int a; int b; };
int main(void) { struct S x = {1, 2}, y = {3, 4}; x = y; return x.b; }
