// 6.4.4.1p5  type of an unsuffixed constant observed without _Generic: sizeof and signedness
#include <stdio.h>
int main(void) {
    printf("%d %d %d %d %d\n",
           (int)sizeof 2147483648,          /* long long here (long is 32-bit): 8 */
           (int)sizeof 0x80000000,          /* unsigned int: 4 */
           0x80000000 > 0,                  /* unsigned: 1 */
           -2147483648 < 0,                 /* -(long long 2147483648): 1 */
           (int)sizeof 4294967296);          /* 8 */
    printf("%d %d %lld\n", -1 < 0xffffffff, 0x7fffffff + 0 > 0, -2147483648);
    return 0;
}
