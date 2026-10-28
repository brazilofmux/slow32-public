// 6.6p6  a case label must be an integer constant expression
// expect: reject
int main(void) { int v = 1, k = 1; switch (v) { case k: return 1; } return 0; }
