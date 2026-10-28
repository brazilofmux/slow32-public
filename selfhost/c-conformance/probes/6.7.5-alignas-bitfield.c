// 6.7.5p2  _Alignas may not apply to a bit-field
// expect: reject
struct s { _Alignas(4) int b : 3; };
int main(void) { return 0; }
