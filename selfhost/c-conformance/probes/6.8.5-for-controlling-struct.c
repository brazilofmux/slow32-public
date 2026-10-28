// 6.8.5p2  for's controlling expression shall have scalar type too
// expect: reject
struct s { int a; };
int main(void) {
    struct s v = { 0 };
    for (; v; ) v.a++;
    return 0;
}
