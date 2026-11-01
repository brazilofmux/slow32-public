/* 7.11.1.1 setlocale: "" selects the native environment (here LC_ALL=C), after which "C" is still honoured */
#include <stdio.h>
#include <locale.h>

int main(void) {
    char *p = setlocale(LC_ALL, "");
    printf("native %d ", p != NULL);
    printf("C again %d ", setlocale(LC_ALL, "C") != NULL);
    printf("decimal point [%s]\n", localeconv()->decimal_point);
    return 0;
}
