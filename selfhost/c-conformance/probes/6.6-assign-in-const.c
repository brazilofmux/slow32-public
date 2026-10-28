// 6.6p3  a constant expression shall not contain assignment, increment, or function call
// expect: reject
static int x;
enum { E = (x = 3) };
int main(void) { return E; }
