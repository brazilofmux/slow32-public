void debug_char(char c);

int main() {
    // Test basic arithmetic operations
    int a = 10;
    int b = 3;
    
    int sum = a + b;      // 13
    int diff = a - b;     // 7
    int prod = a * b;     // 30
    int quot = a / b;     // 3
    int rem = a % b;      // 1
    
    // Verify results
    if (sum == 13 && diff == 7 && prod == 30 && quot == 3 && rem == 1) {
        debug_char('P');
        debug_char('A');
        debug_char('S');
        debug_char('S');
        debug_char('\n');
    } else {
        debug_char('F');
        debug_char('A');
        debug_char('I');
        debug_char('L');
        debug_char('\n');
    }
    
    return 0;
}