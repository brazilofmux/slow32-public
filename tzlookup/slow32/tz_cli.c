/* tz -- command-line ZIP/phone -> IANA timezone lookup (SLOW-32 port).
 *
 *   tz --zip 94089 --phone "+1 (408) 555-1212"
 *   tz --zip 10001
 *   tz --phone 3125551212
 */
#include <stdio.h>
#include <string.h>

#include "tzlookup.h"

static void usage(const char *prog) {
  fprintf(stderr,
          "usage: %s [--zip ZIP] [--phone PHONE]\n"
          "  At least one of --zip / --phone is required.\n",
          prog);
}

int main(int argc, char **argv) {
  const char *zip = NULL;
  const char *phone = NULL;
  for (int i = 1; i < argc; ++i) {
    if (!strcmp(argv[i], "--zip") && i + 1 < argc) zip = argv[++i];
    else if (!strcmp(argv[i], "--phone") && i + 1 < argc) phone = argv[++i];
    else { usage(argv[0]); return 2; }
  }
  if (!zip && !phone) { usage(argv[0]); return 2; }

  const char *tz = tz_lookup(zip, phone);
  if (!tz) {
    printf("unknown\n");
    return 1;
  }
  printf("%s\n", tz);
  return 0;
}
