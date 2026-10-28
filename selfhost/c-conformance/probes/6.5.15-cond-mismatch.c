// 6.5.15p3  struct and int arms of ?: are not allowed together
// expect: reject
struct S { int a; };
int main(void) { volatile int c = 1; struct S s = {0}; int i = (c ? s : 1).a; return i; }
