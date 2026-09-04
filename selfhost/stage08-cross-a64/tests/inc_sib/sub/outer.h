/* A header in a subdirectory including a sibling with the quoted form --
   libutf's include/utf/nfc.h -> "utf_types.h".  No -I names sub/, so this
   resolves only if quoted includes search the including file's directory. */
#include "inner.h"
#define FROM_OUTER 1
