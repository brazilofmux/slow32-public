// 6.8.6.1p1  a goto cannot name a label of another function
// expect: reject
static int f(void) { there: return 1; }
int main(void) {
    goto there;
    return f();
}
int pad(void) { return 1; }
