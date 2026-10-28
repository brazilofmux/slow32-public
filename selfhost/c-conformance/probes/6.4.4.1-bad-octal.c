// 6.4.4.1p1  8 is not an octal digit: 09 is not a valid integer constant
// expect: reject
int main(void) { return 09; }
