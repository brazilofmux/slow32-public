// 6.7.2.1p4  a zero-width bit-field must be unnamed
// expect: reject
struct s { int a : 0; };
int main(void) { return 0; }
