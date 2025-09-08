// Test printf and varargs
int printf(const char* fmt, ...);

int main() {
    int value = 42;
    char c = 'A';
    short s = 1234;
    
    printf("Integer: %d, Char: %c, Short: %d\n", value, c, s);
    
    return 0;
}