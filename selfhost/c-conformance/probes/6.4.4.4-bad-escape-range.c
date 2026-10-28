// 6.4.4.4p9  an octal/hex escape value must fit the type (unsigned char for plain constants)
// expect: reject
int main(void) { return '\x123'; }
