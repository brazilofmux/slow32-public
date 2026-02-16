int main(void) {
    int a = 3;
    int b = 5;
    if (!(a < b && b > 0)) return 1;
    if ((a == b) || (a > b)) return 2;
    return 0;
}
