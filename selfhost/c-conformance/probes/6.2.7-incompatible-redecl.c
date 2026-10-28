// 6.2.7p2  two declarations of one object with incompatible types (constraint 6.7p4)
// expect: reject
extern int q;
extern long q;
int main(void) { return 0; }
