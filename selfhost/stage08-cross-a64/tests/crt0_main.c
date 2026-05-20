/* crt0_main.c — tiny main() for the crt0 smoke test.
 *
 * Returns argc, so the driver can verify that:
 *   - _start correctly read argc from the kernel-set-up stack
 *   - _start correctly passed it as main's first argument
 *   - main's return value was correctly handed to sys_exit.
 */

int main(int argc, char **argv) {
    (void)argv;
    return argc;
}
