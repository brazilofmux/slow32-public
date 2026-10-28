// 6.10.3p4  argument count must equal parameter count
// expect: reject
#define F(a, b) ((a) + (b))
int main(void) { return F(1, 2, 3); }
