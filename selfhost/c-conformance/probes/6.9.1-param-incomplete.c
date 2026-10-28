// 6.9.1p7  after adjustment, parameters of a definition shall have complete object type
// expect: reject
struct later;
int f(struct later x) { return 0; }
int main(void) { return 0; }
