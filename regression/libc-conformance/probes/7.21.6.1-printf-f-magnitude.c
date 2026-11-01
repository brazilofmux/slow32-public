/* 7.21.6.1 fprintf: %f of very large and very small doubles prints every integer digit exactly */
#include <stdio.h>

int main(void) {
    printf("%f\n", 1e300);
    printf("%f\n", 1.7976931348623157e308);
    printf("%.0f\n", 9007199254740993.0);
    printf("%.0f %.0f\n", 1e22, 99999999999999991611392.0);
    printf("%f %f\n", 1e-300, 2.2250738585072014e-308);
    printf("%.330f\n", 4.9406564584124654e-324);
    printf("%.60f\n", 2.2250738585072014e-308);
    return 0;
}
