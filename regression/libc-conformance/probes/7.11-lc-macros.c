/* 7.11 locale.h: the LC_ category macros are distinct integer constant expressions usable in #if */
#include <stdio.h>
#include <locale.h>

#if LC_ALL == LC_CTYPE || LC_ALL == LC_NUMERIC || LC_COLLATE == LC_TIME || LC_MONETARY == LC_NUMERIC
#error not distinct
#endif

int main(void) {
    int v[6] = { LC_ALL, LC_COLLATE, LC_CTYPE, LC_MONETARY, LC_NUMERIC, LC_TIME };
    int i, j, same = 0;
    for (i = 0; i < 6; i++)
        for (j = i + 1; j < 6; j++)
            if (v[i] == v[j]) same++;
    printf("equal pairs %d, NULL defined %d\n", same, NULL == (void *)0);
    return 0;
}
