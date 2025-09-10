void debug_char(char c);

void *malloc(unsigned int size);
void free(void *ptr);

int main() {
    // Test 1: Simple malloc
    debug_char('1');
    debug_char('\n');
    
    char *buffer = (char *)malloc(32);
    if (buffer) {
        debug_char('A');
        buffer[0] = 'X';
        buffer[1] = '\0';
        debug_char(buffer[0]);
        free(buffer);
        debug_char('B');
    } else {
        debug_char('N');
    }
    debug_char('\n');
    
    return 0;
}