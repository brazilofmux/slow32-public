/* cc_hello.c — minimal smoke test for cc-a64.
 *
 * Compile-only mode: produces a .o that the test runner links with crt0.o
 * and runs.  Pass = exit code 42.
 */

int main(void) {
    return 42;
}
