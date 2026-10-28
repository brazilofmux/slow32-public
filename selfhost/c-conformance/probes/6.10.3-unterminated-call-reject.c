// 6.10.3p4  a ) must terminate the invocation
// expect: reject
#define F(a) a
int main(void) { return F(0; }
