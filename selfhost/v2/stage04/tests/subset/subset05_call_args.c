static int ret5(int a, int b, int c, int d, int e) {
    return a + b + c + d + e;
}

int main(void) {
    int x = ret5(1, 2, 3, 4, 5);
    return (x == 15) ? 0 : 1;
}
