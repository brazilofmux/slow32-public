// 6.6p6  floating constants in an ICE must be immediate operands of casts
// expect: reject
enum { E = 2.5 };
int main(void) { return E; }
