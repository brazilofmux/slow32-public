/* s32ar_min.h -- cc-min compatible header for s32-ar-port.c */

/* Standard file handles (linker resolves against libc globals) */
int stdout;
int stderr;

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

int fopen(char *path, char *mode);
int fclose(int f);
int fputc(int c, int f);
int fputs(char *s, int f);
int fgetc(int f);
int fseek(int f, int off, int whence);
int ftell(int f);
int fwrite(char *buf, int sz, int count, int f);
int fread(char *buf, int sz, int count, int f);
int fput_uint(int f, int v);
