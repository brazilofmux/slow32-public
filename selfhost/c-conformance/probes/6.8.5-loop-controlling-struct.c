// 6.8.5p2  a loop condition of struct type (not scalar) is refused
// expect: reject
struct s { int a; };
int main(void) {
    struct s v = { 0 };
    while (v) v.a++;
    return 0;
}
