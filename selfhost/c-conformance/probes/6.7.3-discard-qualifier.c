// 6.5.16.1p1  initializing int * from const int * discards a qualifier
// expect: reject
int main(void) { const int c = 1; int *p = &c; return *p; }
