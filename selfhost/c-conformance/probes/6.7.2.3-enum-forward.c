// 6.7.2.3p3  an enum tag reference must follow its definition (no forward enum)
// expect: reject
enum later x;
enum later { L1 };
int main(void) { return 0; }
