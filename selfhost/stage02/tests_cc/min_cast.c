int main(void) {
    char *p;
    int x;
    p = (char *)0;
    x = (int)p;
    if (x == 0) return 1;
    return 0;
}
