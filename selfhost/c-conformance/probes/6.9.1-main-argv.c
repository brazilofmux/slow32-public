// 6.9.1 / 5.1.2.2.1  main(int argc, char *argv[]) with argv[argc] a null pointer
#include <stdio.h>
int main(int argc, char *argv[]) {
    printf("%d %d\n", argc >= 1, argv[argc] == 0);
    return 0;
}
