void debug_char(char c);

int main() {
    int x = 5;
    int y = 10;
    
    // Test various branch conditions
    if (x < y) {
        debug_char('1');  // Should execute
    } else {
        debug_char('X');
    }
    
    if (x > y) {
        debug_char('X');
    } else {
        debug_char('2');  // Should execute
    }
    
    if (x == 5) {
        debug_char('3');  // Should execute
    }
    
    if (x != y) {
        debug_char('4');  // Should execute
    }
    
    if (x >= 5) {
        debug_char('5');  // Should execute
    }
    
    if (y <= 10) {
        debug_char('6');  // Should execute
    }
    
    debug_char('\n');
    return 0;
}