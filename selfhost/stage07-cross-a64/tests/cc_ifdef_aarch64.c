int main(void) {
#ifdef __aarch64__
    return 1;
#else
    return 2;
#endif
}
