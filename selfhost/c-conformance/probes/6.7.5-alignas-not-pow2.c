// 6.7.5p4  an alignment constant must be a valid (power-of-two) alignment
// expect: reject
_Alignas(3) char x;
int main(void) { return x; }
