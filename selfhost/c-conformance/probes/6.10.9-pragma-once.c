// 6.10.9p1  _Pragma("once") in a header behaves as #pragma once
#include <stdio.h>
#include "inc/6.10.9-once.h"
#include "inc/6.10.9-once.h"
int main(void) { printf("%d\n", pragma_op_value()); return 0; }
