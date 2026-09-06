#ifndef SLOW32_UTIME_H
#define SLOW32_UTIME_H
struct utimbuf { long actime, modtime; };
static inline int utime(const char *p, const struct utimbuf *t) { (void)p; (void)t; return 0; }
#endif
