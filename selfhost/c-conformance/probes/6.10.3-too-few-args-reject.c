// 6.10.3p4  argument count must equal parameter count
// expect: reject
#define F(a, b, c) ((a) + (b) + (c))
int main(void) { return F(1, 2); }
