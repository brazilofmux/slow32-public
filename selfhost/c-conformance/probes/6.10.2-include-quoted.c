// 6.10.2p3,p6  "..." include relative to the includer, nested include, guard twice
#include <stdio.h>
#include "inc/6.10.2-a.h"
#include "inc/6.10.2-guard.h"
#include "inc/6.10.2-guard.h"
int main(void) {
    int total = 0;
#include "inc/6.10.2-body.h"
#include "inc/6.10.2-body.h"
    printf("%d %d %d %d\n", FROM_A, FROM_B, guarded_value(), total);
    return 0;
}
