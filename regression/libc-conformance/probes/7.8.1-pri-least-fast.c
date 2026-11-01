/* 7.8.1p2-p3, p6: the PRI macros for the least and fast types */
#include <inttypes.h>
#include <stdio.h>

int main(void) {
    printf("%" PRIdLEAST8 " %" PRIdLEAST16 " %" PRIdLEAST32 " %" PRIdLEAST64 "\n",
           (int_least8_t)-128, (int_least16_t)-32768, (int_least32_t)INT32_MIN, (int_least64_t)INT64_MIN);
    printf("%" PRIuLEAST8 " %" PRIuLEAST16 " %" PRIuLEAST32 " %" PRIuLEAST64 "\n",
           (uint_least8_t)255, (uint_least16_t)65535, (uint_least32_t)UINT32_MAX, (uint_least64_t)UINT64_MAX);
    printf("%" PRIdFAST8 " %" PRIdFAST16 " %" PRIdFAST32 " %" PRIdFAST64 "\n",
           (int_fast8_t)-128, (int_fast16_t)-32768, (int_fast32_t)INT32_MIN, (int_fast64_t)INT64_MIN);
    printf("%" PRIxFAST8 " %" PRIxFAST16 " %" PRIxFAST32 " %" PRIxFAST64 "\n",
           (uint_fast8_t)255, (uint_fast16_t)65535, (uint_fast32_t)UINT32_MAX, (uint_fast64_t)UINT64_MAX);
    return 0;
}
