int abi_callee_target(int a, int b, int c, int d) {
    int x;
    int y;
    x = (a + b) ^ (c - d);
    y = (a * 3) + (d * 5);
    return x + y;
}
