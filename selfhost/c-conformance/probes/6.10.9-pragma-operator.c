// 6.10.9p1  _Pragma("...") acts as a #pragma, directly and from a macro, and vanishes
#include <stdio.h>
#define DO_PRAGMA(x) _Pragma(#x)
#define LISTING(x) DO_PRAGMA(probe_listing on #x)
_Pragma("probe_unknown_pragma")
_Pragma("STDC FP_CONTRACT OFF")
LISTING(..\listing.dir)
DO_PRAGMA(STDC FENV_ACCESS OFF)
int main(void) {
    int v = 1;
    _Pragma("probe_other")
    v += 2;
    printf("%d\n", v);
    return 0;
}
