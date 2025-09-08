void debug_char(char c);

// Use volatile to prevent optimization
void test_ptr_arithmetic(volatile char *str) {
    volatile char *p = str;
    
    // Test basic pointer arithmetic
    debug_char(*p);     // Should print first char
    p = p + 1;          // Increment pointer
    debug_char(*p);     // Should print second char
    p = p + 1;          // Increment pointer  
    debug_char(*p);     // Should print third char
    debug_char('\n');
}

int main() {
    char str[] = "ABC";
    test_ptr_arithmetic(str);
    return 0;
}