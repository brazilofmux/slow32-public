// Test increment operators
void debug_char(char c);

void test_postinc() {
    char str[] = "ABC";
    char *p = str;
    
    // Test post-increment
    debug_char(*p++);  // Should print 'A', p points to 'B'
    debug_char(*p++);  // Should print 'B', p points to 'C'  
    debug_char(*p++);  // Should print 'C', p points to '\0'
    debug_char('\n');
}

void test_preinc() {
    char str[] = "ABC";
    char *p = str;
    
    // Test pre-increment
    debug_char(*p);     // Should print 'A'
    debug_char(*++p);   // Should print 'B'
    debug_char(*++p);   // Should print 'C'
    debug_char('\n');
}

void test_explicit() {
    char str[] = "ABC";
    char *p = str;
    
    // Explicit increment
    debug_char(*p);     // Should print 'A'
    p = p + 1;
    debug_char(*p);     // Should print 'B'
    p = p + 1;
    debug_char(*p);     // Should print 'C'
    debug_char('\n');
}

int main() {
    test_postinc();
    test_preinc();
    test_explicit();
    return 0;
}