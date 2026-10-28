// 6.5.3.2p1  address of a bit-field may not be taken
// expect: reject
struct s { int a : 3; };
int main(void) { struct s v; int *p = &v.a; return *p; }
