/* 7.20.3: PTRDIFF_MIN, PTRDIFF_MAX and SIZE_MAX from <stdint.h> alone, meeting their minima and matching their types */
#include <stdint.h>
#include <stdio.h>

int main(void) {
    printf("%d %d %d\n", PTRDIFF_MIN <= -65535, PTRDIFF_MAX >= 65535, SIZE_MAX >= 65535);
    printf("%d %d\n", sizeof(SIZE_MAX) == sizeof(sizeof 0), (SIZE_MAX * 0 - 1) > 0);
    return 0;
}
