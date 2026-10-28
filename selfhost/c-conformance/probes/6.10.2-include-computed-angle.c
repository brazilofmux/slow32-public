// 6.10.2p4  #include of macro-replaced pp-tokens (<...> form)
#define SYSHDR <stdio.h>
#define STR2(x) #x
#define STR(x) STR2(x)
#include SYSHDR
#include STR(inc/6.10.2-v1.h)
int main(void) {
    printf("%d\n", CHOSEN);
    return 0;
}
