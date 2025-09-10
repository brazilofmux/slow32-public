// Very simple test to isolate the problem
void debug_char(char c);

void puts(const char *str) {
    while (*str) {
        debug_char(*str++);
    }
}

int main() {
    puts("Hello\n");
    return 0;
}