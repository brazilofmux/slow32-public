/*
 * Diagnostic: short int array indexing
 *
 * dtoa's Lhint[] is "static short int Lhint[2098]".
 * Accessing Lhint[1074] requires the compiler to scale the index by 2
 * (sizeof(short) = 2). If the backend incorrectly scales by 4
 * (sizeof(int)), the wrong element is read.
 *
 * Also tests signed vs unsigned 16-bit loads (LDH vs LDHU).
 */
#include <stdio.h>

/* Large enough array to test index scaling at various offsets */
short int arr[2100];

int main() {
    int i;

    /* Fill array with index values so we can verify which element we read */
    for (i = 0; i < 2100; i++) {
        arr[i] = (short)(i & 0x7FFF);  /* mod 32768 to stay in short range */
    }

    /* Test 1: Basic short array access at small indices */
    printf("t1a: %d\n", (int)arr[0]);
    printf("t1b: %d\n", (int)arr[1]);
    printf("t1c: %d\n", (int)arr[10]);

    /* Test 2: Access at index 1074 (the dtoa Lhint index for 1.25) */
    printf("t2: %d\n", (int)arr[1074]);

    /* Test 3: Access at large indices - stride errors would show here */
    printf("t3a: %d\n", (int)arr[500]);
    printf("t3b: %d\n", (int)arr[1000]);
    printf("t3c: %d\n", (int)arr[2000]);

    /* Test 4: Variable index access */
    int idx = 1074;
    printf("t4: %d\n", (int)arr[idx]);

    /* Test 5: Pointer arithmetic with short* */
    short *p = arr;
    printf("t5a: %d\n", (int)p[1074]);
    printf("t5b: %d\n", (int)*(p + 1074));

    /* Test 6: Signed values via short */
    arr[0] = -1;
    arr[1] = 32767;
    arr[2] = -32768;
    arr[3] = 342;
    printf("t6a: %d\n", (int)arr[0]);
    printf("t6b: %d\n", (int)arr[1]);
    printf("t6c: %d\n", (int)arr[2]);
    printf("t6d: %d\n", (int)arr[3]);

    /* Test 7: Adjacent elements to verify stride */
    arr[99] = 99;
    arr[100] = 100;
    arr[101] = 101;
    printf("t7a: %d\n", (int)arr[99]);
    printf("t7b: %d\n", (int)arr[100]);
    printf("t7c: %d\n", (int)arr[101]);

    /* Test 8: sizeof verification */
    printf("t8: sizeof_short=%d sizeof_int=%d\n",
           (int)sizeof(short), (int)sizeof(int));

    return 0;
}
