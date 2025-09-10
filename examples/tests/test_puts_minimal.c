// Minimal test case for puts() argument passing bug
int puts(const char *str);

int main() {
    puts("Hello");
    return 0;
}