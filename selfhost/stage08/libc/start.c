/* Selfhost bootstrap libc: C runtime start with MMIO argv
 *
 * Provides __slow32_start that fetches argc/argv from the emulator's
 * MMIO args protocol, then calls main(argc, argv).
 *
 * This is linked as LINK-OBJ before mmio_minimal.s32o so that
 * __slow32_start here takes precedence over the dummy version there.
 * exit() remains defined in mmio_minimal.s (halt instruction).
 *
 * MMIO protocol:
 *   s32_mmio_request(0x60, 8, 0, 0)  -> ARGS_INFO: data[0..3]=argc, [4..7]=total_bytes
 *   s32_mmio_request(0x61, nbytes, 0, 0) -> ARGS_DATA: data[0..n]=NUL-separated argv
 *
 * NOTE: cc.fth treats "extern" declarations as definitions, so we cannot
 * use "extern char __mmio_base;" directly. Instead we use __get_mmio_data()
 * from mmio_minimal.s which returns &__mmio_base + 16384.
 *
 * Written for cc-min subset-C compatibility.
 */

int main(int argc, char **argv);
void exit(int status);
void __stdio_init(void);
int s32_mmio_request(int opcode, int length, int offset, int fd);
char *memcpy(char *dst, const char *src, unsigned int n);
char *__get_mmio_data(void);
char *malloc(int size);

#define MMIO_ARGS_INFO_SIZE 16
#define MMIO_OP_ARGS_INFO 96
#define MMIO_OP_ARGS_DATA 97

/* One ARGS_DATA request may move at most S32_MMIO_DATA_CAPACITY (48KB)
 * bytes, so a larger blob is fetched in chunks.  The 4th argument of
 * s32_mmio_request lands in the descriptor's `status` word, which
 * ARGS_DATA reads as the SOURCE offset into the host's argv blob. */
#define MMIO_ARGS_CHUNK 32768

/* argv used to be a fixed 4KB blob and 32 slots, and ANY overflow --
 * too many arguments or too many bytes -- fell through silently with
 * argc = 0, so the program saw no arguments at all and said nothing
 * about why.  "s32-ar rc lib.s32a *.o" over 50 files printed its usage
 * banner.  Both are now sized from what the host actually staged. */
static char *args_blob;
static char **args_argv;
static char *empty_argv[1];

static int rd32_at(char *p) {
    int v;
    v = p[0] & 255;
    v = v | ((p[1] & 255) << 8);
    v = v | ((p[2] & 255) << 16);
    v = v | ((p[3] & 255) << 24);
    return v;
}

void __slow32_start(void) {
    int argc;
    int status;
    char *data_buf;
    int arg_count;
    int total;
    int i;
    int offset;
    int copied;
    int chunk;

    __stdio_init();

    argc = 0;
    empty_argv[0] = (char *)0;
    args_argv = empty_argv;

    data_buf = __get_mmio_data();

    /* Query args info */
    status = s32_mmio_request(MMIO_OP_ARGS_INFO, MMIO_ARGS_INFO_SIZE, 0, 0);
    if (status == 0) {
        arg_count = rd32_at(data_buf);
        total = rd32_at(data_buf + 4);

        if (arg_count > 0 && total > 0) {
            args_blob = malloc(total);
            args_argv = (char **)malloc((arg_count + 1) * 4);
            if (args_blob == (char *)0 || args_argv == (char **)0) {
                args_argv = empty_argv;
            } else {
                /* Fetch the blob, in as many chunks as it takes. */
                copied = 0;
                while (copied < total) {
                    chunk = total - copied;
                    if (chunk > MMIO_ARGS_CHUNK) chunk = MMIO_ARGS_CHUNK;
                    status = s32_mmio_request(MMIO_OP_ARGS_DATA, chunk, 0, copied);
                    if (status != 0) break;
                    memcpy(args_blob + copied, data_buf, (unsigned int)chunk);
                    copied = copied + chunk;
                }

                if (copied == total) {
                    /* Parse NUL-separated strings into argv array */
                    offset = 0;
                    i = 0;
                    while (i < arg_count) {
                        args_argv[i] = args_blob + offset;
                        while (offset < total && args_blob[offset] != 0)
                            offset = offset + 1;
                        if (offset < total)
                            offset = offset + 1;
                        i = i + 1;
                    }
                    args_argv[arg_count] = (char *)0;
                    argc = arg_count;
                } else {
                    args_argv = empty_argv;
                }
            }
        }
    }

    i = main(argc, args_argv);
    exit(i);
}

/* getenv stub: stage08's MMIO bootstrap exposes argv but no envp,
 * so environment lookups always fail.  Returning NULL matches the
 * standard "name not present" semantics, which leaves debug gates
 * like `if (getenv("CC_X64_PROMO_DEBUG"))` correctly disabled. */
char *getenv(const char *name) {
    (void)name;
    return (char *)0;
}
