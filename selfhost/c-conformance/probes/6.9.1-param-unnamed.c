// 6.9.1p5  in a prototype-form definition each parameter shall have an identifier
// expect: reject
int f(int, int b) { return b; }
int main(void) { return f(1, 0); }
