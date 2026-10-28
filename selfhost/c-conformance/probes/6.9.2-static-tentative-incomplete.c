// 6.9.2p3  a tentative definition with internal linkage shall not have incomplete type
// expect: reject
static int arr[];
int main(void) { return 0; }
