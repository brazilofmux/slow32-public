#include <stdio.h>

int main(void) {
    double a = 1.0, b = 2.0, c = 3.0, d = 4.0;
    
    // Test with 1 double
    printf("1 arg: %.1f\n", a);
    
    // Test with 2 doubles
    printf("2 args: %.1f %.1f\n", a, b);
    
    // Test with 3 doubles
    printf("3 args: %.1f %.1f %.1f\n", a, b, c);
    
    // Test with 4 doubles - this is where it might fail
    printf("4 args: %.1f %.1f %.1f %.1f\n", a, b, c, d);
    
    // Print each individually to verify values
    printf("verify: a=%.1f b=%.1f c=%.1f d=%.1f\n", a, b, c, d);
    
    return 0;
}
