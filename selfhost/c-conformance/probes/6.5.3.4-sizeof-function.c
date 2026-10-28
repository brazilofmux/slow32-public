// 6.5.3.4p1  sizeof may not be applied to a function type
// expect: reject
int f(void);
int main(void) { return (int)sizeof(int(void)); }
