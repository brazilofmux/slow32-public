void debug_char(char c);

int main() {
    // Test for loop
    for (int i = 0; i < 3; i++) {
        debug_char('0' + i);  // Should print 012
    }
    
    // Test while loop
    int j = 3;
    while (j < 6) {
        debug_char('0' + j);  // Should print 345
        j++;
    }
    
    // Test do-while loop
    int k = 6;
    do {
        debug_char('0' + k);  // Should print 678
        k++;
    } while (k < 9);
    
    debug_char('\n');
    return 0;
}