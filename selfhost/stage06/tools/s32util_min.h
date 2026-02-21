/* s32util_min.h -- minimal libc + format constants for selfhost utility tools */

int stdout;
int stderr;

#define NULL 0
#define EOF -1
#define SEEK_SET 0
#define SEEK_END 2

#define S32O_MAGIC 0x5333324F
#define S32X_MAGIC 0x53333258

#define S32_ENDIAN_LITTLE 0x01
#define S32_MACHINE_SLOW32 0x32

#define S32O_FLAG_PIC      0x0001
#define S32O_FLAG_DEBUG    0x0002
#define S32O_FLAG_STRIPPED 0x0004

#define S32X_FLAG_W_XOR_X    0x0001
#define S32X_FLAG_HAS_EVT    0x0002
#define S32X_FLAG_HAS_TSR    0x0004
#define S32X_FLAG_HAS_DEBUG  0x0008
#define S32X_FLAG_STRIPPED   0x0010
#define S32X_FLAG_PIC        0x0020
#define S32X_FLAG_COMPRESSED 0x0040
#define S32X_FLAG_MMIO       0x0080

#define S32_SEC_NULL   0x0000
#define S32_SEC_CODE   0x0001
#define S32_SEC_DATA   0x0002
#define S32_SEC_BSS    0x0003
#define S32_SEC_RODATA 0x0004
#define S32_SEC_EVT    0x0010
#define S32_SEC_TSR    0x0011
#define S32_SEC_DEBUG  0x0020
#define S32_SEC_SYMTAB 0x0021
#define S32_SEC_STRTAB 0x0022

#define S32_SEC_FLAG_EXEC  0x0001
#define S32_SEC_FLAG_WRITE 0x0002
#define S32_SEC_FLAG_READ  0x0004
#define S32_SEC_FLAG_ALLOC 0x0008

#define S32O_SYM_NOTYPE  0x00
#define S32O_SYM_FUNC    0x01
#define S32O_SYM_OBJECT  0x02
#define S32O_SYM_SECTION 0x03

#define S32O_BIND_LOCAL  0x00
#define S32O_BIND_GLOBAL 0x01
#define S32O_BIND_WEAK   0x02

#define S32O_REL_NONE       0x0000
#define S32O_REL_32         0x0001
#define S32O_REL_HI20       0x0002
#define S32O_REL_LO12       0x0003
#define S32O_REL_BRANCH     0x0004
#define S32O_REL_JAL        0x0005
#define S32O_REL_CALL       0x0006
#define S32O_REL_PCREL_HI20 0x0007
#define S32O_REL_PCREL_LO12 0x0008

#define SIZEOF_S32O_HEADER  40
#define SIZEOF_S32O_SECTION 32
#define SIZEOF_S32O_SYMBOL  16
#define SIZEOF_S32O_RELOC   16
#define SIZEOF_S32X_HEADER  64
#define SIZEOF_S32X_SECTION 28

char *memcpy(char *dst, char *src, int n);
char *memset(char *dst, int c, int n);
int strcmp(char *a, char *b);
int strlen(char *s);

int fopen(char *path, char *mode);
int fclose(int f);
int fseek(int f, int off, int whence);
int ftell(int f);
int fgetc(int f);
int fread(char *buf, int sz, int count, int f);
int fwrite(char *buf, int sz, int count, int f);
int fputc(int c, int f);
int fputs(char *s, int f);
int fput_uint(int f, int v);

char *malloc(int n);
void free(char *p);
