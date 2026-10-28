// 6.10.3p6  parameter names must be unique
// expect: reject
#define F(a, a) a
int main(void) { return 0; }
