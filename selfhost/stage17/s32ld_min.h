/* s32ld_min.h -- cc-min compatible header for s32-ld-port.c */

/* Standard file handles (linker resolves against libc globals) */
int stdout;
int stderr;

/* Constants */
#define NULL 0
#define EOF -1
#define SEEK_SET 0
#define SEEK_END 2

/* .s32o / .s32x / .s32a magic numbers (little-endian) */
#define S32O_MAGIC 0x5333324F
#define S32X_MAGIC 0x53333258
#define S32A_MAGIC 0x53333241

/* Section types */
#define SEC_CODE   1
#define SEC_DATA   2
#define SEC_BSS    3
#define SEC_RODATA 4

/* Section flags for .s32x */
#define SF_EXEC  0x01
#define SF_WRITE 0x02
#define SF_READ  0x04
#define SF_ALLOC 0x08
#define SF_XRA   0x0D
#define SF_WRA   0x0E
#define SF_RA    0x0C

/* Relocation types */
#define REL_32         1
#define REL_HI20       2
#define REL_LO12       3
#define REL_BRANCH     4
#define REL_JAL        5
#define REL_CALL       6
#define REL_PCREL_HI20 7
#define REL_PCREL_LO12 8
#define REL_ABS32      32

/* Symbol binding */
#define BIND_LOCAL  0
#define BIND_GLOBAL 1
#define BIND_WEAK   2

/* Instruction encoding masks */
#define MASK5   0x1F
#define MASK6   0x3F
#define MASK7   0x7F
#define MASK8   0xFF
#define MASK10  0x3FF
#define MASK12  0xFFF
#define MASK20  0xFFFFF
#define HALF12  0x800

/* Store opcodes (S-format detection) */
#define OP_STB 0x38
#define OP_STH 0x39
#define OP_STW 0x3A

/* B-type and S-type instruction masks */
#define S_MASK 0xFE000F80
#define B_MASK 0x01FFF07F

/* Memory layout constants */
#define STACK_BASE    0x0FFFFFF0
#define STACK_SIZE    262144
#define MEM_SIZE      0x10000000
#define HEAP_GAP      0x800000
#define CODE_LIMIT_MIN 0x100000
#define PAGE_MASK     0xFFFFF000

/* .s32x header: 64 bytes */
#define S32X_HDR_SZ 64
/* .s32x section table entry: 28 bytes */
#define S32X_SEC_SZ 28
/* .s32o section table entry: 32 bytes */
#define S32O_SEC_SZ 32
/* .s32o symbol table entry: 16 bytes */
#define S32O_SYM_SZ 16
/* .s32o relocation entry: 16 bytes */
#define S32O_REL_SZ 16

/* Machine and format constants */
#define S32_MACHINE  0x32
#define S32_ENDIAN   0x01
#define S32X_FLAG_WXORX 0x01
#define S32X_FLAG_MMIO  0x80

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
