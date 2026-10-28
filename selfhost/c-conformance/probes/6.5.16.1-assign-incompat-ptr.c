// 6.5.16.1p1  pointers to incompatible types may not be assigned without a cast
// expect: reject
int main(void) { double d = 0; int *p; p = &d; return *p; }
