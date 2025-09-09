#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Declare functions not in standard headers
char *itoa(int value, char *str, int base);
char *strdup(const char *s);

// Test the new stdlib functions
int main() {
    printf("Testing new stdlib functions\n");
    printf("============================\n\n");
    
    // Test memmove with overlapping regions
    printf("Testing memmove:\n");
    char buffer[] = "Hello, World!";
    memmove(buffer + 7, buffer, 6);  // Copy "Hello," to "World!" position
    printf("  After memmove: '%s'\n", buffer);
    
    // Test memchr
    printf("\nTesting memchr:\n");
    const char *str = "Find the letter X here";
    char *found = memchr(str, 'X', strlen(str));
    if (found) {
        printf("  Found 'X' at position: %d\n", (int)(found - str));
    }
    
    // Test strcat
    printf("\nTesting strcat:\n");
    char dest[50] = "Hello, ";
    strcat(dest, "World!");
    printf("  Concatenated: '%s'\n", dest);
    
    // Test strchr and strrchr
    printf("\nTesting strchr/strrchr:\n");
    const char *path = "/home/user/file.txt";
    char *first_slash = strchr(path, '/');
    char *last_slash = strrchr(path, '/');
    printf("  First '/' at position: %d\n", (int)(first_slash - path));
    printf("  Last '/' at position: %d\n", (int)(last_slash - path));
    
    // Test strstr
    printf("\nTesting strstr:\n");
    const char *haystack = "The quick brown fox jumps";
    const char *needle = "brown";
    char *substr = strstr(haystack, needle);
    if (substr) {
        printf("  Found '%s' at position: %d\n", needle, (int)(substr - haystack));
    }
    
    // Test strdup
    printf("\nTesting strdup:\n");
    const char *original = "Duplicate me!";
    char *copy = strdup(original);
    if (copy) {
        printf("  Original: '%s'\n", original);
        printf("  Copy: '%s'\n", copy);
        free(copy);
    }
    
    // Test strtoul
    printf("\nTesting strtoul:\n");
    const char *num_str = "42 0xFF 0777";
    char *endptr;
    unsigned long val1 = strtoul(num_str, &endptr, 10);
    printf("  Decimal 42: %lu\n", val1);
    unsigned long val2 = strtoul(endptr, &endptr, 16);
    printf("  Hex 0xFF: %lu (255)\n", val2);
    unsigned long val3 = strtoul(endptr, &endptr, 8);
    printf("  Octal 0777: %lu (511)\n", val3);
    
    // Test itoa
    printf("\nTesting itoa:\n");
    char buffer2[33];
    itoa(255, buffer2, 10);
    printf("  255 in base 10: %s\n", buffer2);
    itoa(255, buffer2, 16);
    printf("  255 in base 16: %s\n", buffer2);
    itoa(255, buffer2, 2);
    printf("  255 in base 2: %s\n", buffer2);
    
    // Test calloc
    printf("\nTesting calloc:\n");
    int *arr = calloc(5, sizeof(int));
    if (arr) {
        printf("  Allocated 5 ints (should be zero): ");
        for (int i = 0; i < 5; i++) {
            printf("%d ", arr[i]);
        }
        printf("\n");
        free(arr);
    }
    
    // Test realloc
    printf("\nTesting realloc:\n");
    int *nums = malloc(3 * sizeof(int));
    if (nums) {
        nums[0] = 10; nums[1] = 20; nums[2] = 30;
        printf("  Original array: %d %d %d\n", nums[0], nums[1], nums[2]);
        
        nums = realloc(nums, 5 * sizeof(int));
        if (nums) {
            nums[3] = 40; nums[4] = 50;
            printf("  Expanded array: ");
            for (int i = 0; i < 5; i++) {
                printf("%d ", nums[i]);
            }
            printf("\n");
            free(nums);
        }
    }
    
    printf("\nAll tests completed!\n");
    return 0;
}