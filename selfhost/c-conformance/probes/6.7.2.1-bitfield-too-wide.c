// 6.7.2.1p4  bit-field width may not exceed the width of its type
// expect: reject
struct s { unsigned char c : 9; };
int main(void) { return 0; }
