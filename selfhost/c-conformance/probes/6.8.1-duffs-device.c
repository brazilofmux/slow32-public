// 6.8.1p2  case labels may sit inside nested statements of the switch body (Duff's device)
#include <stdio.h>
static void copy(int *to, const int *from, int count) {
    int n = (count + 3) / 4;
    switch (count % 4) {
    case 0: do { *to++ = *from++;
    case 3:      *to++ = *from++;
    case 2:      *to++ = *from++;
    case 1:      *to++ = *from++;
            } while (--n > 0);
    }
}
int main(void) {
    int a[11], b[11], i, s = 0;
    for (i = 0; i < 11; i++) { a[i] = i * 3 + 1; b[i] = -1; }
    for (int c = 1; c <= 11; c++) {
        copy(b, a, c);
        for (i = 0; i < 11; i++) s = s * 7 + b[i];
    }
    printf("%d %d %d\n", b[0], b[10], s);
    return 0;
}
