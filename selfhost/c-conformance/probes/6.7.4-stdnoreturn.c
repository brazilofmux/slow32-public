// 7.23/6.7.4p8  <stdnoreturn.h> noreturn macro
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
noreturn static void stop(void) { puts("stop"); exit(0); }
int main(void) { stop(); }
