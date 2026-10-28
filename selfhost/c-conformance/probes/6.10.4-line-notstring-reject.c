// 6.10.4p1,p5  the optional file name must be a character string literal
// expect: reject
#line 10 notastring
int main(void) { return 0; }
