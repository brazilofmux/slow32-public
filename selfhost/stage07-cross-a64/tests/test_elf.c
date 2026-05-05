/* test_elf.c — smoke test for elf_writer.h.
 *
 * Hand-emit a 3-instruction "exit(42)" program with a64_encode and wrap it
 * in an ELF executable via elf_writer. The runner then exec's the binary
 * and checks that it returns 42.
 *
 *   AArch64 Linux exit(42):
 *     mov w8, #93        ; SYS_exit
 *     mov w0, #42        ; status
 *     svc #0
 */

/* host libc shims — declare what we need without dragging in headers that
 * might collide with elf_writer.h's bare open()/write()/close() prototypes. */
int open(char *path, int flags, int mode);
int close(int fd);
int write(int fd, char *buf, int len);

#include "../a64_encode.h"
#include "../elf_writer.h"

#include <stdio.h>

int main(void) {
    /* Build code. */
    a64_off = 0;
    a64_mov_w_imm(A64_X8, 93);     /* SYS_exit */
    a64_mov_w_imm(A64_X0, 42);     /* status */
    a64_svc(0);

    /* Wrap as ELF. */
    elf_init();
    elf_set_text(a64_buf, a64_off);
    elf_set_entry(0);
    if (elf_write_file("out/test_exit42") < 0) {
        fprintf(stderr, "elf_write_file failed\n");
        return 1;
    }
    printf("wrote out/test_exit42 (%d code bytes)\n", a64_off);
    return 0;
}
