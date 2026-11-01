/* 7.21.1 stdio.h macros: BUFSIZ at least 256, EOF negative, the SEEK and _IO macros distinct */
#include <stdio.h>

int main(void) {
    printf("%d %d\n", BUFSIZ >= 256, EOF < 0);
    printf("%d %d\n", SEEK_SET != SEEK_CUR && SEEK_CUR != SEEK_END && SEEK_SET != SEEK_END,
           _IOFBF != _IOLBF && _IOLBF != _IONBF && _IOFBF != _IONBF);
    printf("%d %d\n", FILENAME_MAX > 0, sizeof(fpos_t) > 0);
    return 0;
}
