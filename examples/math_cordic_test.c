#include <stdio.h>
#include <math.h>

int main() {
    double angles[] = {0.0, 0.78539816, 1.57079633, 3.14159265, -1.0};
    const char *names[] = {"0", "pi/4", "pi/2", "pi", "-1"};

    printf("CORDIC Math Test\n");
    printf("----------------\n");

    for (int i = 0; i < 5; i++) {
        double a = angles[i];
        printf("Angle: %s (%f)\n", names[i], a);
        printf("  sin: %f\n", sin(a));
        printf("  cos: %f\n", cos(a));
        printf("  tan: %f\n", tan(a));
        printf("\n");
    }

    printf("Atan2 Test:\n");
    printf("  atan2(1, 1)   = %f (expected 0.785398)\n", atan2(1.0, 1.0));
    printf("  atan2(1, 0)   = %f (expected 1.570796)\n", atan2(1.0, 0.0));
    printf("  atan2(0, -1)  = %f (expected 3.141592)\n", atan2(0.0, -1.0));
    printf("\n");

        printf("Exp Test:\n");

        printf("  exp(0)   = %f (expected 1.0)\n", exp(0.0));

        printf("  exp(1)   = %f (expected 2.718282)\n", exp(1.0));

        printf("  exp(-1)  = %f (expected 0.367879)\n", exp(-1.0));

        printf("  exp(10)  = %f (expected 22026.465795)\n", exp(10.0));

        printf("  exp(-5)  = %f (expected 0.006738)\n", exp(-5.0));

    

        printf("\nFull Circle Atan2 Test:\n");

        printf("  atan2(-1, -1) = %f (expected -2.356194)\n", atan2(-1.0, -1.0));

        printf("  atan2(-1, 1)  = %f (expected -0.785398)\n", atan2(-1.0, 1.0));

    

        return 0;

    }

    