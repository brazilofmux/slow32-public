/* Minimal crt0: _start calls main and exits via syscall.
 * Compiled with cc-x64 -c to produce crt0.o for linker testing.
 * Note: argc/argv not wired up (passed as 0). */

int main();
int __syscall();

void _start(void) {
    int ret;
    ret = main(0, 0);
    __syscall(60, ret, 0, 0, 0, 0, 0);
}
