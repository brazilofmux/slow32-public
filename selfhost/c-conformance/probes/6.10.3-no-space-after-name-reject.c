// 6.10.3p3  white space is required between an object-like macro name and its list
// expect: reject
#define PLUS+1
int main(void) { return 0; }
