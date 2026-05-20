int add(int a, int b) {
    return a + b;
}

int mul(int a, int b) {
    return a * b;
}

int main() {
    int x;
    int y;
    x = add(3, 4);      /* 7 */
    y = mul(x, 6);      /* 42 */
    return y;
}
