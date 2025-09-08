// Test simple printf without varargs first
int printf(const char *format, ...);

int main() {
    // This should work - no varargs  
    printf("Hello, World!\n");
    
    // This is the simplest varargs case - one int
    printf("Number: %d\n", 42);
    
    return 0;
}