// 6.7.3p2  restrict may qualify only pointer types to object types
// expect: reject
restrict int x;
int main(void) { return 0; }
