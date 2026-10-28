// 6.7.10p2  a false static assertion is a constraint violation
// expect: reject
_Static_assert(sizeof(char) == 2, "char is not 2 bytes");
int main(void) { return 0; }
