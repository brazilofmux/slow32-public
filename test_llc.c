#include <stdio.h>

int main() {
    printf("Testing LLVM backend for SLOW-32!\n");
    
    int x = 10;
    int y = 20;
    int sum = x + y;
    
    printf("Sum of %d + %d = %d\n", x, y, sum);
    
    // Test a simple loop
    int total = 0;
    for (int i = 1; i <= 5; i++) {
        total += i;
    }
    printf("Sum of 1 to 5 = %d\n", total);
    
    return 0;
}