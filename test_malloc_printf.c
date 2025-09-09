#include <stdio.h>
#include <stdlib.h>

int main() {
    // Test 1: Simple printf
    printf("Testing printf\n");
    
    // Test 2: Printf with format specifier
    printf("Number: %d\n", 42);
    
    // Test 3: Malloc allocation
    char *buffer = (char *)malloc(32);
    if (buffer) {
#if 0
        buffer[0] = 'A';
        buffer[1] = '\0';
        printf("Buffer: %s\n", buffer);
#else
        puts("A\n");
#endif
        free(buffer);
    }
    
    return 0;
}
