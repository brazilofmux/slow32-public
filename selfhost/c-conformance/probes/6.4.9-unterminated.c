// 6.4.9p1  a /* comment must be terminated before end of file
// expect: reject
int main(void) { return 0; }
/* never closed
