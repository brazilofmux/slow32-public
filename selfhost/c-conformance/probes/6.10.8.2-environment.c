// 6.10.8.2p1  environment macros, if defined, have the documented forms
#include <stdio.h>
int main(void) {
    int ok = 1;
#ifdef __STDC_ISO_10646__
    ok &= (__STDC_ISO_10646__ >= 199712L);
#endif
#ifdef __STDC_MB_MIGHT_NEQ_WC__
    ok &= (__STDC_MB_MIGHT_NEQ_WC__ == 1);
#endif
#ifdef __STDC_UTF_16__
    ok &= (__STDC_UTF_16__ == 1);
#endif
#ifdef __STDC_UTF_32__
    ok &= (__STDC_UTF_32__ == 1);
#endif
    puts(ok ? "ok" : "bad");
    return 0;
}
