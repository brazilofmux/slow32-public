/* s32_formats_min.h -- cc-min compatible header for s32-as-port.c */

/* Standard file descriptors */

/* S32O object format magic */
#define S32O_MAGIC 0x5333324F

/* Struct sizes (cc-min has no sizeof(struct)) */
#define SIZEOF_S32O_HEADER  40
#define SIZEOF_S32O_SECTION 32
#define SIZEOF_S32O_SYMBOL  16
#define SIZEOF_S32O_RELOC   16

/* Section types */
#define S32_SEC_NULL   0x0000
#define S32_SEC_CODE   0x0001
#define S32_SEC_DATA   0x0002
#define S32_SEC_BSS    0x0003
#define S32_SEC_RODATA 0x0004

/* Section flags */
#define S32_SEC_FLAG_EXEC  0x0001
#define S32_SEC_FLAG_WRITE 0x0002
#define S32_SEC_FLAG_READ  0x0004
#define S32_SEC_FLAG_ALLOC 0x0008

/* Symbol types */
#define S32O_SYM_NOTYPE  0x00
#define S32O_SYM_FUNC    0x01
#define S32O_SYM_OBJECT  0x02
#define S32O_SYM_SECTION 0x03

/* Symbol binding */
#define S32O_BIND_LOCAL  0x00
#define S32O_BIND_GLOBAL 0x01
#define S32O_BIND_WEAK   0x02

/* Relocation types */
#define S32O_REL_NONE       0x0000
#define S32O_REL_32         0x0001
#define S32O_REL_HI20       0x0002
#define S32O_REL_LO12       0x0003
#define S32O_REL_BRANCH     0x0004
#define S32O_REL_JAL        0x0005
#define S32O_REL_CALL       0x0006
#define S32O_REL_PCREL_HI20 0x0007
#define S32O_REL_PCREL_LO12 0x0008

/* Endian / machine */
#define S32_ENDIAN_LITTLE 0x01
#define S32_MACHINE_SLOW32 0x32

/* NULL */
#define NULL 0

/* Libc function prototypes */
int strcmp(char *a, char *b);
int strncmp(char *a, char *b, int n);
char *strchr(char *s, int c);
int strlen(char *s);
char *memcpy(char *dst, char *src, int n);
char *memset(char *dst, int c, int n);
int strtol(char *s, char **endp, int base);

int fdopen_path(char *path, char *mode);
int fdclose(int f);
int fdputc(int c, int f);
int fdwrite(char *buf, int sz, int count, int f);
int fdread(char *buf, int sz, int count, int f);
char *fdgets(char *buf, int sz, int f);
int fdputs(char *s, int f);
int fdtell(int f);
int fdputuint(int f, int v);
