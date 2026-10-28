// 6.2.8p1-p4  _Alignof of basic and struct types; max_align_t
#include <stdio.h>
#include <stddef.h>
struct s1 { char c; double d; };
struct s2 { char c; short h; };
int main(void) {
    printf("%d %d %d %d %d %d %d\n", (int)_Alignof(char), (int)_Alignof(short), (int)_Alignof(int),
           (int)_Alignof(long long), (int)_Alignof(double), (int)_Alignof(struct s1),
           (int)offsetof(struct s2, h));
    printf("%d %d\n", (int)_Alignof(max_align_t) >= (int)_Alignof(double), (int)_Alignof(int[5]));
    return 0;
}
