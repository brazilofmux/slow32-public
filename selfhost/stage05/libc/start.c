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

#define ARGS_BLOB_SIZE 4096
#define MAX_ARGC 32
#define MAX_ARGV_SLOTS 33
#define MMIO_ARGS_INFO_SIZE 16
#define MMIO_OP_ARGS_INFO 96
#define MMIO_OP_ARGS_DATA 97

static char args_blob[ARGS_BLOB_SIZE];
static char *args_argv[MAX_ARGV_SLOTS];

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

    __stdio_init();

    argc = 0;
    args_argv[0] = (char *)0;

    data_buf = __get_mmio_data();

    /* Query args info */
    status = s32_mmio_request(MMIO_OP_ARGS_INFO, MMIO_ARGS_INFO_SIZE, 0, 0);
    if (status == 0) {
        arg_count = rd32_at(data_buf);
        total = rd32_at(data_buf + 4);

        if (arg_count > 0 && total > 0 && total <= ARGS_BLOB_SIZE && arg_count <= MAX_ARGC) {
            /* Fetch the args blob */
            status = s32_mmio_request(MMIO_OP_ARGS_DATA, total, 0, 0);
            if (status == 0) {
                memcpy(args_blob, data_buf, (unsigned int)total);

                /* Parse NUL-separated strings into argv array */
                offset = 0;
                for (i = 0; i < arg_count; i = i + 1) {
                    args_argv[i] = args_blob + offset;
                    while (offset < total && args_blob[offset] != 0)
                        offset = offset + 1;
                    if (offset < total)
                        offset = offset + 1;
                }
                args_argv[arg_count] = (char *)0;
                argc = arg_count;
            }
        }
    }

    i = main(argc, args_argv);
    exit(i);
}
