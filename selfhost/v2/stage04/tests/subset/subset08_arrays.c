int main(void) {
    int a[4];
    int i;
    int sum = 0;
    for (i = 0; i < 4; i++) a[i] = i + 1;
    for (i = 0; i < 4; i++) sum += a[i];
    return (sum == 10) ? 0 : 1;
}
