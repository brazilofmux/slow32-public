// 6.10.3.3p1  ## may not begin a replacement list
// expect: reject
#define BAD(x) ## x
int main(void) { return 0; }
