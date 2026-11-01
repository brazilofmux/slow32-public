/* 7.8.1p4-p5: SCNd32, SCNu32 and SCNx32 read into int32_t and uint32_t */
#include <inttypes.h>
#include <stdio.h>

int main(void) {
    int32_t a = 0;
    uint32_t c = 0, d = 0;
    int n = sscanf("-2147483648 4294967295 ff", "%" SCNd32 " %" SCNu32 " %" SCNx32, &a, &c, &d);
    printf("%d %ld %lu %lu\n", n, (long)a, (unsigned long)c, (unsigned long)d);
    return 0;
}
