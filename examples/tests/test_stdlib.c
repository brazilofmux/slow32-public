#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// Comparison function for qsort
int compare_ints(const void *a, const void *b) {
    return *(int*)a - *(int*)b;
}

int main() {
    printf("Testing new stdlib functions:\n\n");
    
    // Test qsort
    printf("1. Testing qsort:\n");
    int arr[] = {5, 2, 8, 1, 9, 3, 7};
    int n = sizeof(arr) / sizeof(arr[0]);
    
    printf("   Before: ");
    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
    
    qsort(arr, n, sizeof(int), compare_ints);
    
    printf("   After:  ");
    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n\n");
    
    // Test bsearch
    printf("2. Testing bsearch:\n");
    int key = 7;
    int *result = (int*)bsearch(&key, arr, n, sizeof(int), compare_ints);
    if (result) {
        printf("   Found %d at index %ld\n", key, result - arr);
    } else {
        printf("   %d not found\n", key);
    }
    printf("\n");
    
    // Test strtol
    printf("3. Testing strtol:\n");
    char *endptr;
    long val;
    
    val = strtol("123", &endptr, 10);
    printf("   \"123\" base 10 = %ld\n", val);
    
    val = strtol("0xFF", &endptr, 16);
    printf("   \"0xFF\" base 16 = %ld\n", val);
    
    val = strtol("101010", &endptr, 2);
    printf("   \"101010\" base 2 = %ld\n", val);
    printf("\n");
    
    // Test ctype functions
    printf("4. Testing ctype functions:\n");
    char test_char = 'A';
    printf("   '%c': isalpha=%d, isupper=%d, tolower='%c'\n", 
           test_char, isalpha(test_char), isupper(test_char), tolower(test_char));
    
    test_char = '5';
    printf("   '%c': isdigit=%d, isalpha=%d\n", 
           test_char, isdigit(test_char), isalpha(test_char));
    printf("\n");
    
    // Test string functions
    printf("5. Testing string functions:\n");
    char str[] = "Hello,World,Test";
    char *token;
    char *saveptr;
    
    printf("   Tokenizing \"%s\" by comma:\n", str);
    token = strtok_r(str, ",", &saveptr);
    while (token) {
        printf("   - %s\n", token);
        token = strtok_r(NULL, ",", &saveptr);
    }
    printf("\n");
    
    // Test div functions
    printf("6. Testing div functions:\n");
    div_t d = div(17, 5);
    printf("   17 / 5 = %d remainder %d\n", d.quot, d.rem);
    
    printf("\nAll tests completed!\n");
    return 0;
}