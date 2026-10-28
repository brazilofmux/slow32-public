// 6.4.1p1  _Imaginary is a keyword even where complex is unsupported: it cannot name a variable
// expect: reject
int main(void) { int x = 1; int *_Imaginary = &x; return *_Imaginary; }
