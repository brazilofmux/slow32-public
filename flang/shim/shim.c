/* What flang-rt wants and libc_mmio lacks: setenv, unsetenv, gethostname, mkstemp, times, getuid, getgid.  A proof's stand-ins; see docs/plans/flang.md. */
#include <sys/times.h>
#include <stdlib.h>
int setenv(const char *a, const char *b, int c) { return -1; }
int unsetenv(const char *a) { return -1; }
int gethostname(char *b, unsigned n) { if (n) b[0] = 0; return 0; }
#include <fcntl.h>
#include <string.h>
int mkstemp(char *t) {
  static unsigned n = 0;
  size_t l = strlen(t);
  if (l < 6 || strcmp(t + l - 6, "XXXXXX")) return -1;
  for (int i = 0; i < 1000; i++) {
    unsigned v = ++n;
    for (int k = 1; k <= 6; k++) { t[l - k] = (char)('a' + v % 26); v /= 26; }
    int fd = open(t, O_RDWR | O_CREAT | O_EXCL, 0600);
    if (fd >= 0) return fd;
  }
  return -1;
}
clock_t times(struct tms *t) { return (clock_t)-1; }
unsigned getuid(void){return 0;} unsigned getgid(void){return 0;}
void _ZN7Fortran7runtime2io19RegisterRPCHandlersEv(void) {}
