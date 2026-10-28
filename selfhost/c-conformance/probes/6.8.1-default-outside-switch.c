// 6.8.1p2  a default label outside any switch is a constraint violation
// expect: reject
int main(void) {
    int x = 1;
    while (x) {
    default: x = 0;
    }
    return x;
}
