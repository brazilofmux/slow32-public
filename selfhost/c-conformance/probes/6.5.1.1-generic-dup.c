// 6.5.1.1p2  no two associations may name compatible types
// expect: reject
int main(void) { return _Generic(0, int: 1, signed int: 2); }
