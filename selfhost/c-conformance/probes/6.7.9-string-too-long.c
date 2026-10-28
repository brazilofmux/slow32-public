// 6.7.9p2  string literal longer than the array (beyond the null) is an excess initializer
// expect: reject
char s[2] = "abc";
int main(void) { return s[0]; }
