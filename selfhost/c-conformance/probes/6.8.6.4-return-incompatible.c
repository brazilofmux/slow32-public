// 6.8.6.4p3 / 6.5.16.1p1  returning a struct from an int function violates the assignment constraints
// expect: reject
struct s { int a; };
static int f(void) { struct s v = { 1 }; return v; }
int main(void) { return f(); }
