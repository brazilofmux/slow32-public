/* test_crt0.c — emit a crt0.o using crt0_emit.h.
 *
 * The runner script links it with:
 *   - crt0_main.o      (host-gcc-built; defines main(argc, argv) -> argc)
 *   - crt0_stubs.o     (host-gcc-built; defines a no-op __save_envp)
 *
 * Then it runs the binary with several arg counts and verifies the exit
 * code matches argc.
 */

int open(char *path, int flags, int mode);
int close(int fd);
int read(int fd, char *buf, int len);
int write(int fd, char *buf, int len);

#include "../a64_encode.h"
#include "cg_stubs.h"
#include "../obj_writer.h"
#include "../crt0_emit.h"

#include <stdio.h>

int main(void) {
    if (emit_crt0_object("out/crt0.o") < 0) {
        fprintf(stderr, "emit_crt0_object failed\n");
        return 1;
    }
    printf("wrote out/crt0.o (%d code bytes, %d patches)\n",
           a64_off, cg_ncpatches);
    return 0;
}
