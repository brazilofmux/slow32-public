/* pictest -- drive pic_analyse over a list of pictures, one per line, and
 * print what it synthesised.  The expected file is checked against the
 * 1985 text by hand (PICTURE clause, II-20..II-30), not against GnuCOBOL.
 * Lines beginning with '#' are comments. */
#include <stdio.h>
#include <string.h>
#include "picture.h"

int main(int argc, char **argv)
{
    FILE *f = argc > 1 ? fopen(argv[1], "r") : stdin;
    if (!f) { perror(argv[1]); return 1; }
    char line[512];
    while (fgets(line, sizeof line, f)) {
        char *e = line + strlen(line);
        while (e > line && (e[-1] == '\n' || e[-1] == '\r' || e[-1] == ' ')) *--e = 0;
        if (!line[0] || line[0] == '#') continue;
        PicInfo pi;
        if (pic_analyse(line, &pi) < 0) {
            printf("%-20s ERROR %s\n", line, pi.err);
            continue;
        }
        printf("%-20s %-20s bytes=%d digits=%d scale=%d signed=%d",
               line, pic_category_name(pi.category), pi.bytes, pi.digits,
               pi.scale, pi.is_signed);
        if (pi.floating) printf(" floating=%c", pi.floating);
        if (pi.edited) printf(" pat=%s", pi.pat);
        printf("\n");
    }
    return 0;
}
