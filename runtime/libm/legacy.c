/* isnan, isinf and isfinite as functions.  C makes them macros, and the
 * <math.h> both libraries now share does; but objects built against the
 * old header call them by name, and the DBT intercepts these names. */
#include <math.h>

int (isnan)(double x) { return isnan(x); }
int (isinf)(double x) { return isinf(x); }
int (isfinite)(double x) { return isfinite(x); }
