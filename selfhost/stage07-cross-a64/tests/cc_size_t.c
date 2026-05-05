int main(void) {
    size_t n;
    n = sizeof(void *);
    if (n != 8) return 2;
    return 1;
}
