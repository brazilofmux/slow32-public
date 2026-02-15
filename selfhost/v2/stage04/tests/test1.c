/* test1.c — Hello world */
int putchar(int c);

void print_str(char *s) {
    while (*s) {
        putchar(*s);
        s = s + 1;
    }
}

int main() {
    print_str("Hello, World!\n");
    return 0;
}
