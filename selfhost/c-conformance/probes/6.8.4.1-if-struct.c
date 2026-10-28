// 6.8.4.1p1  the controlling expression of if shall have scalar type
// expect: reject
struct s { int a; };
int main(void) {
    struct s v = { 1 };
    if (v) return 1;
    return 0;
}
