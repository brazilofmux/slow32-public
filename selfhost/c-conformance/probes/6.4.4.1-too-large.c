// 6.4.4.1p6  an integer constant that cannot be represented in any type (constraint 6.4.4p2)
// expect: reject
int main(void) { return (int)(0x1ffffffffffffffffULL & 1); }
