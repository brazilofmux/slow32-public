/* scredit_test.c -- the screen editor's core, on the host.
 *
 *   scredit_test PICTURE VALUE KEYS
 *
 * prints the field and the cursor after each key, in the form
 * tests/adischeck.sh prints what Micro Focus's ADIS does with the same
 * picture and keys, so the two can be compared line for line
 * (tests/scredit-differential.sh).  KEYS as adischeck's: characters, and
 * {ENTER} {BS} {DEL} {INS} {LEFT} {RIGHT} {HOME} {END} {^X} ...  One
 * field, so the verdicts that leave it are played as ADIS plays them on a
 * one-field screen. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "scredit.h"

static se_field F; static se_state S;

static void show(const char *key, int cur)
{
    char img[SE_MAXW + 1];
    se_image(&F, &S, img, cur); img[F.width] = 0;
    printf("%-8s [%s]  %d\n", key, img, se_cursor(&F, &S) + 1);
}

int main(int argc, char **argv)
{
    if (argc != 4) { fprintf(stderr, "usage: scredit_test PICTURE VALUE KEYS\n"); return 2; }
    char mask[SE_MAXW + 1];
    int w = se_expand_picture(argv[1], mask, SE_MAXW);
    if (w < 0) { fprintf(stderr, "scredit_test: not a text picture: %s\n", argv[1]); return 2; }
    se_field_init(&F, mask, w, 0, '_');
    /* the value moved to the picture: left-justified into the data positions */
    char image[SE_MAXW]; memset(image, ' ', sizeof image);
    for (int i = 0; i < w; i++) if (!F.cls[i]) image[i] = F.lit[i];
    for (int k = 0; k < F.nd && argv[2][k]; k++) image[F.col[k]] = argv[2][k];
    memset(&S, 0, sizeof S);
    se_enter(&F, &S, image, 0);
    show("(start)", 1);
    for (const char *p = argv[3]; *p; ) {
        char name[16]; int fn = 0, ch = 0;
        if (*p == '{') {
            const char *q = strchr(p, '}'); if (!q) break;
            snprintf(name, sizeof name, "%.*s", (int)(q - p - 1), p + 1); p = q + 1;
            for (char *c = name; *c; c++) if (*c >= 'a' && *c <= 'z') *c = (char)(*c - 32);
            if (!strcmp(name, "ENTER")) {
                char t[SE_MAXW + 1]; se_text(&F, &S, t); t[w] = 0;
                printf("ITEM=[%s]\n", t);
                return 0;
            }
            fn = !strcmp(name, "LEFT") ? SE_LEFT : !strcmp(name, "RIGHT") ? SE_RIGHT : !strcmp(name, "END") ? SE_END
               : !strcmp(name, "BS") ? SE_BACKSPACE : !strcmp(name, "DEL") ? SE_DELETE : !strcmp(name, "INS") ? SE_INSERT_TOGGLE
               : !strcmp(name, "^X") ? SE_CLEAR_FIELD : !strcmp(name, "^Z") ? SE_CLEAR_EOF : !strcmp(name, "^A") ? SE_UNDO
               : !strcmp(name, "^O") ? SE_INSERT_SPACE : !strcmp(name, "^R") ? SE_RESTORE_CHAR : !strcmp(name, "^F") ? SE_CHANGE_CASE
               : !strcmp(name, "HOME") ? -1 : 0;
            if (!fn) { fprintf(stderr, "scredit_test: what is {%s}?\n", name); return 2; }
        } else { name[0] = *p; name[1] = 0; fn = SE_CHAR; ch = (unsigned char)*p++; }
        if (fn == -1) se_reenter_keep(&F, &S);               /* Home: the first field's first position */
        else {
            int v = se_key(&F, &S, fn, ch);
            (void)v;                                          /* one field: a move out of it goes nowhere */
        }
        show(name, 1);
    }
    printf("(keys ran out)\n");
    return 0;
}
