// 6.10.6p1  #pragma once (implementation-defined, universally supported) in a header
#include <stdio.h>
#include "inc/6.10.2-once.h"
#include "inc/6.10.2-once.h"
int main(void) { printf("%d\n", once_value()); return 0; }
