// Minimal stdio functions needed for basic operation

extern int putchar(int c);

int puts(const char *s) {
    while (*s) {
        putchar(*s++);
    }
    putchar('\n');
    return 0;
}