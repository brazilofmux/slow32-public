/* Test #include "file.h" */
#include "test_include.h"

int main(void) {
    int r;
    r = add_values(20, 22);
    if (r != MAGIC) return 1;
    r = add_values(0, 0);
    if (r != 0) return 2;
    return 0;
}
