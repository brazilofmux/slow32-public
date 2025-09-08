#include "../runtime/include/string.h"

void putchar(int c);
void puts(const char *s);

void test_string_functions() {
    puts("Testing string functions:");
    
    // Test strlen
    const char *str = "Hello, World!";
    if (strlen(str) == 13) {
        puts("  strlen: PASS");
    } else {
        puts("  strlen: FAIL");
    }
    
    // Test strcpy
    char dest[20];
    strcpy(dest, "Test");
    if (strcmp(dest, "Test") == 0) {
        puts("  strcpy: PASS");
    } else {
        puts("  strcpy: FAIL");
    }
    
    // Test strcat
    strcat(dest, "123");
    if (strcmp(dest, "Test123") == 0) {
        puts("  strcat: PASS");
    } else {
        puts("  strcat: FAIL");
    }
    
    // Test strchr
    const char *found = strchr(str, 'W');
    if (found && *found == 'W') {
        puts("  strchr: PASS");
    } else {
        puts("  strchr: FAIL");
    }
    
    // Test strstr
    const char *substr = strstr(str, "World");
    if (substr && strncmp(substr, "World", 5) == 0) {
        puts("  strstr: PASS");
    } else {
        puts("  strstr: FAIL");
    }
}

void test_memory_functions() {
    puts("Testing memory functions:");
    
    // Test memset
    char buf[10];
    memset(buf, 'A', 5);
    buf[5] = '\0';
    if (strcmp(buf, "AAAAA") == 0) {
        puts("  memset: PASS");
    } else {
        puts("  memset: FAIL");
    }
    
    // Test memcpy
    char src[] = "12345";
    char dst[10];
    memcpy(dst, src, 6);
    if (strcmp(dst, "12345") == 0) {
        puts("  memcpy: PASS");
    } else {
        puts("  memcpy: FAIL");
    }
    
    // Test memmove
    char overlap[10] = "abcdef";
    memmove(overlap + 2, overlap, 4);
    if (memcmp(overlap + 2, "abcd", 4) == 0) {
        puts("  memmove: PASS");
    } else {
        puts("  memmove: FAIL");
    }
    
    // Test memchr
    const char *data = "Find X here";
    void *found = memchr(data, 'X', 11);
    if (found && *(char*)found == 'X') {
        puts("  memchr: PASS");
    } else {
        puts("  memchr: FAIL");
    }
}

int main() {
    puts("SLOW-32 Standard Library Test Suite");
    puts("====================================");
    
    test_string_functions();
    test_memory_functions();
    
    puts("All tests completed!");
    
    // Note: asm("halt") won't work until inline assembly is added to Clang
    // For now, the program will continue running
    return 0;
}