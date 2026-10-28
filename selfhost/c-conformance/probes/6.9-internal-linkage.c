// 6.9p3-5  a static function declared first, used, then defined later; extern declarations never used need no definition
#include <stdio.h>
static int helper(int);
static int counter;
extern int never_defined(void);
extern int never_defined_obj;
int main(void) {
    counter = helper(4);
    printf("%d %d\n", counter, (int)sizeof never_defined_obj);
    return 0;
}
static int helper(int x) { return x * x + counter; }
