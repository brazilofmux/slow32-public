// 6.10.3p2  function-like redefinition with different parameter spelling
// expect: reject
#define F(a) a
#define F(b) b
int main(void) { return F(0); }
