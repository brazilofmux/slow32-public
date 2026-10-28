// 6.5.16p2/6.7.3  assigning to a struct with a const member (not a modifiable lvalue)
// expect: reject
struct s { const int k; int m; };
int main(void) { struct s a = { 1, 2 }, b = { 3, 4 }; a = b; return a.m; }
