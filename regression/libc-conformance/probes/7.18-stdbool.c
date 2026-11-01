/* 7.18: bool is _Bool; true, false and __bool_true_false_are_defined are integer constants 1, 0, 1 usable in #if */
#include <stdbool.h>
#include <stdio.h>

#if true == 1 && false == 0 && __bool_true_false_are_defined == 1
#define PP 1
#else
#define PP 0
#endif

int main(void) {
    bool a = 256, b = 0.25, c = 0;
    printf("#if %d\n", PP);
    printf("_Bool %d\n", _Generic(a, _Bool: 1, default: 0));
    printf("true/false int %d %d\n", _Generic(true, int: 1, default: 0), _Generic(false, int: 1, default: 0));
    printf("values %d %d %d\n", a, b, c);
    printf("macros %d %d %d\n",
#ifdef bool
           1,
#else
           0,
#endif
#ifdef true
           1,
#else
           0,
#endif
#ifdef false
           1
#else
           0
#endif
    );
    return 0;
}
