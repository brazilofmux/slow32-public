/* Quoted includes resolve against the including file's own directory (C's
   rule), not only the top-level source directory and -I.  outer.h lives in
   inc_sib/sub/ and includes "inner.h" beside it; -I names inc_sib, not sub. */
#include <sub/outer.h>

int main(void) {
    return FROM_INNER && FROM_OUTER;
}
