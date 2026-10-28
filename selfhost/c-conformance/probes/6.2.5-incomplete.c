// 6.2.5p22  an array of unknown size / struct of unknown content completed later
#include <stdio.h>
extern int arr[];
struct later;
struct later *lp;
struct later { int a, b; };
int arr[4] = { 1, 2, 3, 4 };
int main(void) {
    struct later l = { 5, 6 };
    lp = &l;
    printf("%d %d %d\n", (int)sizeof arr, lp->b, arr[3]);
    return 0;
}
