/* s32ar_min.h -- cc-min compatible header for s32-ar-port.c */

/* Standard file handles (linker resolves against libc globals) */

/* Constants */
#define NULL 0
#define EOF -1
#define SEEK_SET 0
#define SEEK_END 2

/* S32O object format constants (for symbol index building) */
#define S32O_MAGIC 0x5333324F
#define S32O_BIND_GLOBAL 0x01

/* Libc function prototypes */
int strcmp(char *a, char *b);
int strlen(char *s);
char *strchr(char *s, int c);
char *memcpy(char *dst, char *src, int n);
char *memset(char *dst, int c, int n);

int fdopen_path(char *path, char *mode);
int fdclose(int f);
int fdputc(int c, int f);
int fdputs(char *s, int f);
int fdgetc(int f);
int fdseek(int f, int off, int whence);
int fdtell(int f);
int fdwrite(char *buf, int sz, int count, int f);
int fdread(char *buf, int sz, int count, int f);
int fdputuint(int f, int v);
