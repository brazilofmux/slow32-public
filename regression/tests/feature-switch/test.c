void debug_char(char c);

int test_switch(int x) {
    switch (x) {
        case 1: return 10;
        case 2: return 20;
        case 3: return 30;
        case 5: return 50;
        case 7: return 70;
        case 11: return 110;
        case 13: return 130;
        default: return -1;
    }
}

int main() {
    // Test various cases
    if (test_switch(1) != 10) { debug_char('1'); return 1; }
    if (test_switch(2) != 20) { debug_char('2'); return 1; }
    if (test_switch(3) != 30) { debug_char('3'); return 1; }
    if (test_switch(5) != 50) { debug_char('5'); return 1; }
    if (test_switch(7) != 70) { debug_char('7'); return 1; }
    if (test_switch(11) != 110) { debug_char('A'); return 1; }
    if (test_switch(13) != 130) { debug_char('B'); return 1; }
    if (test_switch(4) != -1) { debug_char('4'); return 1; }
    if (test_switch(0) != -1) { debug_char('0'); return 1; }
    
    debug_char('P');
    debug_char('A');
    debug_char('S');
    debug_char('S');
    debug_char('\n');
    return 0;
}
