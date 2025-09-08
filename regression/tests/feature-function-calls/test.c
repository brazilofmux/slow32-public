void debug_char(char c);

int add(int a, int b) {
    return a + b;
}

int add8(int a, int b, int c, int d, int e, int f, int g, int h) {
    return a + b + c + d + e + f + g + h;
}

int add_more(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j) {
    // Tests stack-passed arguments (>8 args)
    return a + b + c + d + e + f + g + h + i + j;
}

int main() {
    int r1 = add(5, 3);  // 8
    int r2 = add8(1, 2, 3, 4, 5, 6, 7, 8);  // 36
    int r3 = add_more(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);  // 55
    
    if (r1 == 8 && r2 == 36 && r3 == 55) {
        debug_char('O');
        debug_char('K');
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