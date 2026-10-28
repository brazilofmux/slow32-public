// 6.5.2.1p1  one operand of [] must be a pointer to object type, the other integer
// expect: reject
int main(void) { int a = 1, b = 2; return a[b]; }
