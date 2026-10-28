// 6.8.1p2  a case label outside any switch is a constraint violation
// expect: reject
int main(void) {
    int x = 1;
    if (x) {
    case 1: x = 2;
    }
    return x;
}
