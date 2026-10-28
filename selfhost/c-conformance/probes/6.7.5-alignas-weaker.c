// 6.7.5p5  _Alignas may not specify a weaker alignment than the type's own
// expect: reject
_Alignas(1) int x;
int main(void) { return x; }
