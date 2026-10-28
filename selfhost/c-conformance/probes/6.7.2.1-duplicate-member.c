// 6.7.2.1/6.2.3  two members with the same name in one struct
// expect: reject
struct s { int a; long a; };
int main(void) { return 0; }
