// 6.5.16.1p1  assignment may not discard qualifiers of the pointed-to type
// expect: reject
int main(void) { const int c = 1; int *p; p = &c; return *p; }
