/* 7.12p6 FP_INFINITE etc.: the five number-classification macros are distinct integer constants */
#include <math.h>
#include <stdio.h>

int main(void) {
    int c[5] = { FP_INFINITE, FP_NAN, FP_NORMAL, FP_SUBNORMAL, FP_ZERO };
    int distinct = 1;
    for (int i = 0; i < 5; i++)
        for (int j = i + 1; j < 5; j++)
            if (c[i] == c[j]) distinct = 0;
    printf("distinct %d\n", distinct);
    return 0;
}
