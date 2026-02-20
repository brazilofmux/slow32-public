int main(void) {
    int x;
    int *p;
    x = 3;
    p = &x;
    *p = *p + 4;
    return x;
}
