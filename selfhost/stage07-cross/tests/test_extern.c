/* Test: external function calls and string literals.
 * When compiled with -c, these become unresolved symbols + relocations. */

int write(int fd, char *buf, int len);
int strlen(char *s);

int main(int argc, char **argv) {
    char *msg;
    msg = "Hello, world!\n";
    write(1, msg, strlen(msg));
    return 0;
}
