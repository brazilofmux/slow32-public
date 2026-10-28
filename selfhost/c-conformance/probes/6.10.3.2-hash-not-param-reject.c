// 6.10.3.2p1  # in a function-like replacement list must precede a parameter
// expect: reject
#define BAD(x) #y
int main(void) { return 0; }
