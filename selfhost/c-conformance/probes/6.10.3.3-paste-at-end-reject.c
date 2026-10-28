// 6.10.3.3p1  ## may not end a replacement list
// expect: reject
#define BAD x ##
int main(void) { return 0; }
