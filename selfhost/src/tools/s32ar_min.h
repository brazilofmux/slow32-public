/* s32ar_min.h -- cc-min compatible header for s32-ar-port.c */

/* FILE, the streams, NULL, EOF and SEEK_* are <stdio.h>'s (stage08's: the
 * build passes its include directory to whichever compiler builds this) */
#include <stdio.h>

/* S32O object format constants (for symbol index building) */
#define S32O_MAGIC 0x5333324F
#define S32O_BIND_GLOBAL 0x01

/* Libc function prototypes */
int strcmp(char *a, char *b);
int strlen(char *s);
char *strchr(char *s, int c);
char *memcpy(char *dst, char *src, int n);
char *memset(char *dst, int c, int n);

