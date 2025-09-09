#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    printf("Testing archive linking!\n");
    
    // Test malloc/free
    void *ptr = malloc(100);
    if (ptr) {
        printf("malloc succeeded\n");
        free(ptr);
    }
    
    // Test string functions
    char buffer[100];
    strcpy(buffer, "Hello");
    strcat(buffer, " World");
    printf("String: %s\n", buffer);
    
    return 0;
}