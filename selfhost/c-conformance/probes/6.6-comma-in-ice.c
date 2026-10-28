// 6.6p3  a constant expression shall not contain a comma operator (outside unevaluated operands)
// expect: reject
static int a[(1, 2)];
int main(void) { return (int)sizeof a; }
