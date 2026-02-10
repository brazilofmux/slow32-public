#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "screen.h"
#include "command.h"
#include "util.h"
#include "memvar.h"
#include "expr.h"

#ifdef __slow32__
#include <term.h>
#define HAS_TERM 1
#else
#define HAS_TERM 0
#endif

static screen_state_t scr;
static int print_row, print_col;   /* printer position tracking */

void screen_init(void) {
    memset(&scr, 0, sizeof(scr));
    scr.fg_color = 7; /* white */
    scr.bg_color = 0; /* black */
    /* Start in fallback mode. Full terminal mode can be enabled
       later if needed (term service code compiles in but isn't
       activated by default — interactive @GET/READ with terminal
       requires explicit term_init). */
    scr.term_available = 0;
}

/* ---- Apply PICTURE mask to a string ---- */
static void apply_picture(char *out, const char *value, const char *picture, int outsize) {
    int i;
    int vi = 0;
    int vlen = strlen(value);
    int plen = strlen(picture);

    if (plen == 0 || picture[0] == '\0') {
        str_copy(out, value, outsize);
        return;
    }

    for (i = 0; i < plen && i < outsize - 1; i++) {
        char pc = picture[i];
        char vc = (vi < vlen) ? value[vi] : ' ';

        switch (pc) {
        case '!': /* uppercase */
            if (vc >= 'a' && vc <= 'z') vc -= 32;
            out[i] = vc;
            vi++;
            break;
        case '9': /* digits only */
            out[i] = (vc >= '0' && vc <= '9') ? vc : ' ';
            vi++;
            break;
        case 'A': /* alpha only */
            out[i] = ((vc >= 'A' && vc <= 'Z') || (vc >= 'a' && vc <= 'z')) ? vc : ' ';
            vi++;
            break;
        case 'X': /* any char */
            out[i] = vc;
            vi++;
            break;
        case '#': /* digits/spaces/sign */
            out[i] = (vc >= '0' && vc <= '9' || vc == ' ' || vc == '+' || vc == '-') ? vc : ' ';
            vi++;
            break;
        default:
            /* literal character in picture */
            out[i] = pc;
            break;
        }
    }
    out[i] = '\0';
}

/* ---- Position cursor (fallback: newlines and spaces) ---- */
static void goto_pos(int row, int col) {
#if HAS_TERM
    if (scr.term_available) {
        term_gotoxy(row, col);
        return;
    }
#endif
    /* Fallback: print newline then spaces */
    printf("\n");
    {
        int i;
        for (i = 1; i < col; i++) putchar(' ');
    }
}

/* ---- @SAY ---- */
void screen_say(int row, int col, const char *expr_str, const char *picture) {
    expr_ctx_t *ctx = cmd_get_expr_ctx();
    value_t val;
    char buf[256];
    char formatted[256];

    if (expr_eval_str(ctx, expr_str, &val) != 0) {
        if (ctx->error) printf("Error: %s\n", ctx->error);
        return;
    }

    val_to_string(&val, buf, sizeof(buf));

    if (picture && picture[0]) {
        apply_picture(formatted, buf, picture, sizeof(formatted));
    } else {
        str_copy(formatted, buf, sizeof(formatted));
    }

    if (cmd_get_device() == 1) {
        /* PRINT mode: advance with newlines/spaces to requested position */
        while (print_row < row) {
            printf("\n");
            print_row++;
            print_col = 0;
        }
        while (print_col < col) {
            putchar(' ');
            print_col++;
        }
        printf("%s", formatted);
        print_col += strlen(formatted);
        /* Console echo controlled by SET CONSOLE */
    } else {
        goto_pos(row, col);
        printf("%s", formatted);
    }
    scr.last_row = row;
    scr.last_col = col + strlen(formatted);
}

/* ---- @GET ---- */
void screen_get(int row, int col, const char *varname, const char *picture,
                const value_t *range_lo, const value_t *range_hi) {
    memvar_store_t *store = cmd_get_memvar_store();
    value_t val;

    if (scr.ngets >= MAX_GETS) {
        printf("Too many GETs.\n");
        return;
    }

    scr.gets[scr.ngets].row = row;
    scr.gets[scr.ngets].col = col;
    str_copy(scr.gets[scr.ngets].varname, varname, MEMVAR_NAMELEN);
    if (picture)
        str_copy(scr.gets[scr.ngets].picture, picture, sizeof(scr.gets[scr.ngets].picture));
    else
        scr.gets[scr.ngets].picture[0] = '\0';

    if (range_lo && range_hi) {
        scr.gets[scr.ngets].has_range = 1;
        scr.gets[scr.ngets].range_lo = *range_lo;
        scr.gets[scr.ngets].range_hi = *range_hi;
    } else {
        scr.gets[scr.ngets].has_range = 0;
    }

    /* Get current value for display */
    if (memvar_find(store, varname, &val) == 0) {
        val_to_string(&val, scr.gets[scr.ngets].initial, sizeof(scr.gets[scr.ngets].initial));
        scr.gets[scr.ngets].type = val.type;
        scr.gets[scr.ngets].width = strlen(scr.gets[scr.ngets].initial);
        if (scr.gets[scr.ngets].width < 10) scr.gets[scr.ngets].width = 10;
    } else {
        scr.gets[scr.ngets].initial[0] = '\0';
        scr.gets[scr.ngets].type = VAL_CHAR;
        scr.gets[scr.ngets].width = 10;
    }

    /* Display the field */
    goto_pos(row, col);
    printf("%s", scr.gets[scr.ngets].initial);
    scr.last_row = row;
    scr.last_col = col + strlen(scr.gets[scr.ngets].initial);

    scr.ngets++;
}

/* ---- READ ---- */
void screen_read(void) {
    memvar_store_t *store = cmd_get_memvar_store();
    int i;

#if HAS_TERM
    if (scr.term_available) {
        /* Full-screen READ with terminal support */
        term_set_raw(1);

        for (i = 0; i < scr.ngets; i++) {
            char buf[256];
            int pos, len, key;

            str_copy(buf, scr.gets[i].initial, sizeof(buf));
            len = strlen(buf);
            pos = len;

            term_gotoxy(scr.gets[i].row, scr.gets[i].col);
            printf("%-*s", scr.gets[i].width, buf);
            term_gotoxy(scr.gets[i].row, scr.gets[i].col + pos);

            for (;;) {
                key = term_getkey();
                if (key == '\r' || key == '\n' || key == -1) break;
                if (key == 27) { /* Escape - cancel */
                    str_copy(buf, scr.gets[i].initial, sizeof(buf));
                    break;
                }
                if (key == 8 || key == 127) { /* Backspace */
                    if (pos > 0) {
                        pos--;
                        memmove(buf + pos, buf + pos + 1, len - pos);
                        len--;
                        buf[len] = '\0';
                        term_gotoxy(scr.gets[i].row, scr.gets[i].col);
                        printf("%-*s", scr.gets[i].width, buf);
                        term_gotoxy(scr.gets[i].row, scr.gets[i].col + pos);
                    }
                    continue;
                }
                if (key >= 32 && key < 127 && len < 255) {
                    /* Apply PICTURE mask if any */
                    if (scr.gets[i].picture[0] && pos < (int)strlen(scr.gets[i].picture)) {
                        char pc = scr.gets[i].picture[pos];
                        if (pc == '9' && (key < '0' || key > '9')) continue;
                        if (pc == 'A' && !((key >= 'A' && key <= 'Z') || (key >= 'a' && key <= 'z'))) continue;
                        if (pc == '!' && key >= 'a' && key <= 'z') key -= 32;
                    }
                    memmove(buf + pos + 1, buf + pos, len - pos + 1);
                    buf[pos] = (char)key;
                    pos++;
                    len++;
                    buf[len] = '\0';
                    term_gotoxy(scr.gets[i].row, scr.gets[i].col);
                    printf("%-*s", scr.gets[i].width, buf);
                    term_gotoxy(scr.gets[i].row, scr.gets[i].col + pos);
                }
            }

            /* Store value back */
            {
                value_t v;
                trim_right(buf);
                if (scr.gets[i].type == VAL_NUM) {
                    v = val_num(atof(buf));
                } else {
                    v = val_str(buf);
                }
                /* RANGE validation (match line-mode behavior) */
                if (scr.gets[i].has_range && v.type == VAL_NUM) {
                    double n = v.num;
                    double lo = scr.gets[i].range_lo.num;
                    double hi = scr.gets[i].range_hi.num;
                    if (n < lo || n > hi) {
                        term_set_raw(0);
                        printf("Range: %g to %g\n", lo, hi);
                        term_set_raw(1);
                        continue;  /* reject, keep current value */
                    }
                }
                memvar_set(store, scr.gets[i].varname, &v);
            }
        }

        term_set_raw(0);
    } else
#endif
    {
        /* Fallback: line-mode input for each GET */
        for (i = 0; i < scr.ngets; i++) {
            char line[256];
            value_t v;

            printf("%s=", scr.gets[i].varname);
            if (scr.gets[i].initial[0])
                printf("[%s] ", scr.gets[i].initial);

            if (read_line(line, sizeof(line)) < 0 || line[0] == '\0') {
                /* Keep current value */
                continue;
            }

            trim_right(line);
            if (scr.gets[i].type == VAL_NUM) {
                v = val_num(atof(line));
            } else {
                v = val_str(line);
            }

            /* RANGE validation */
            if (scr.gets[i].has_range && v.type == VAL_NUM) {
                double n = v.num;
                double lo = scr.gets[i].range_lo.num;
                double hi = scr.gets[i].range_hi.num;
                if (n < lo || n > hi) {
                    printf("Range: %g to %g\n", lo, hi);
                    continue;  /* reject, keep current value */
                }
            }

            memvar_set(store, scr.gets[i].varname, &v);
        }
    }

    scr.ngets = 0;
}

/* ---- @CLEAR TO ---- */
void screen_clear_region(int r1, int c1, int r2, int c2) {
#if HAS_TERM
    if (scr.term_available) {
        int r, w;
        w = c2 - c1 + 1;
        if (w <= 0) return;
        for (r = r1; r <= r2; r++) {
            int i;
            term_gotoxy(r, c1);
            for (i = 0; i < w; i++) putchar(' ');
        }
        term_gotoxy(r1, c1);
        return;
    }
#endif
    /* Fallback: print blank lines */
    {
        int r;
        (void)c1; (void)c2;
        for (r = r1; r <= r2; r++) printf("\n");
    }
}

/* ---- @TO (box drawing) ---- */
void screen_box(int r1, int c1, int r2, int c2, int dbl) {
    /* Box characters: single vs double */
    char h, v, tl, tr, bl, br;

    if (dbl) {
        h = '='; v = '|'; tl = '+'; tr = '+'; bl = '+'; br = '+';
    } else {
        h = '-'; v = '|'; tl = '+'; tr = '+'; bl = '+'; br = '+';
    }

#if HAS_TERM
    if (scr.term_available) {
        int r, c;
        /* Top */
        term_gotoxy(r1, c1);
        putchar(tl);
        for (c = c1 + 1; c < c2; c++) putchar(h);
        putchar(tr);
        /* Sides */
        for (r = r1 + 1; r < r2; r++) {
            term_gotoxy(r, c1); putchar(v);
            term_gotoxy(r, c2); putchar(v);
        }
        /* Bottom */
        term_gotoxy(r2, c1);
        putchar(bl);
        for (c = c1 + 1; c < c2; c++) putchar(h);
        putchar(br);
        return;
    }
#endif
    /* Fallback: skip box drawing silently */
    (void)r1; (void)c1; (void)r2; (void)c2;
    (void)h; (void)v; (void)tl; (void)tr; (void)bl; (void)br;
}

/* ---- CLEAR ---- */
void screen_clear(void) {
#if HAS_TERM
    if (scr.term_available) {
        term_clear(0);
        term_gotoxy(1, 1);
        return;
    }
#endif
    /* Fallback: print 24 newlines */
    {
        int i;
        for (i = 0; i < 24; i++) printf("\n");
    }
    scr.last_row = 0;
    scr.last_col = 0;
}

/* ---- CLEAR GETS ---- */
void screen_clear_gets(void) {
    scr.ngets = 0;
}

/* ---- SET COLOR TO ---- */
void screen_set_color(const char *spec) {
    /* Parse color spec: "fg/bg" or "fg"
       Colors: W(hite), N(black), R(ed), G(reen), B(lue),
               GR(gray), BG(bright green), etc.
       For simplicity, map single-letter codes to ANSI colors. */
    const char *p = skip_ws(spec);
    int fg = 7, bg = 0;

    /* Parse foreground */
    switch (*p) {
    case 'N': case 'n': fg = 0; p++; break; /* black */
    case 'B': case 'b': fg = 4; p++; break; /* blue */
    case 'G': case 'g': fg = 2; p++; break; /* green */
    case 'R': case 'r': fg = 1; p++; break; /* red */
    case 'W': case 'w': fg = 7; p++; break; /* white */
    default: p++; break;
    }

    /* Check for '+' (high intensity - just bump by 8 conceptually) */
    if (*p == '+') p++;

    /* Parse background after '/' */
    if (*p == '/') {
        p++;
        switch (*p) {
        case 'N': case 'n': bg = 0; p++; break;
        case 'B': case 'b': bg = 4; p++; break;
        case 'G': case 'g': bg = 2; p++; break;
        case 'R': case 'r': bg = 1; p++; break;
        case 'W': case 'w': bg = 7; p++; break;
        default: p++; break;
        }
    }

    scr.fg_color = fg;
    scr.bg_color = bg;

#if HAS_TERM
    if (scr.term_available)
        term_set_color(fg, bg);
#endif
    (void)p;
}

/* ---- Parse @ command ---- */
void screen_at_cmd(const char *line) {
    const char *p = skip_ws(line);
    int row, col;

    /* Skip @ */
    if (*p != '@') return;
    p++;
    p = skip_ws(p);

    /* Parse row */
    row = atoi(p);
    while (*p >= '0' && *p <= '9') p++;
    p = skip_ws(p);
    if (*p == ',') p++;
    p = skip_ws(p);

    /* Parse col */
    col = atoi(p);
    while (*p >= '0' && *p <= '9') p++;
    p = skip_ws(p);

    /* Sub-command */
    if (str_imatch(p, "SAY")) {
        char picture[64] = "";
        const char *pic_pos;
        char expr_str[256];

        p = skip_ws(p + 3);

        /* Find PICTURE clause */
        pic_pos = p;
        {
            const char *f = p;
            const char *found = NULL;
            while (*f) {
                if (str_imatch(f, "PICTURE")) {
                    found = f;
                    break;
                }
                /* Skip string literals */
                if (*f == '"' || *f == '\'') {
                    char q = *f++;
                    while (*f && *f != q) f++;
                    if (*f) f++;
                    continue;
                }
                f++;
            }
            if (found) {
                int len = (int)(found - p);
                if (len > (int)sizeof(expr_str) - 1) len = (int)sizeof(expr_str) - 1;
                memcpy(expr_str, p, len);
                expr_str[len] = '\0';
                trim_right(expr_str);
                found = skip_ws(found + 7);
                /* Picture is quoted string */
                if (*found == '"' || *found == '\'') {
                    char q = *found++;
                    int i = 0;
                    while (*found && *found != q && i < 63)
                        picture[i++] = *found++;
                    picture[i] = '\0';
                }
            } else {
                str_copy(expr_str, p, sizeof(expr_str));
                trim_right(expr_str);
            }
        }

        screen_say(row, col, expr_str, picture);

    } else if (str_imatch(p, "GET")) {
        char varname[MEMVAR_NAMELEN];
        char picture[64] = "";
        int i = 0;
        int has_range = 0;
        value_t range_lo, range_hi;

        p = skip_ws(p + 3);

        /* Parse variable name */
        while (is_ident_char(*p) && i < MEMVAR_NAMELEN - 1)
            varname[i++] = *p++;
        varname[i] = '\0';

        p = skip_ws(p);
        if (str_imatch(p, "PICTURE")) {
            p = skip_ws(p + 7);
            if (*p == '"' || *p == '\'') {
                char q = *p++;
                i = 0;
                while (*p && *p != q && i < 63)
                    picture[i++] = *p++;
                picture[i] = '\0';
                if (*p) p++;  /* skip closing quote */
            }
        }

        p = skip_ws(p);
        if (str_imatch(p, "RANGE")) {
            expr_ctx_t *ctx = cmd_get_expr_ctx();
            p = skip_ws(p + 5);
            if (expr_eval(ctx, &p, &range_lo) == 0) {
                p = skip_ws(p);
                if (*p == ',') p++;
                p = skip_ws(p);
                if (expr_eval(ctx, &p, &range_hi) == 0) {
                    has_range = 1;
                }
            }
        }

        screen_get(row, col, varname, picture,
                   has_range ? &range_lo : NULL,
                   has_range ? &range_hi : NULL);

    } else if (str_imatch(p, "CLEAR")) {
        int r2 = 24, c2 = 79;
        p = skip_ws(p + 5);
        if (str_imatch(p, "TO")) {
            p = skip_ws(p + 2);
            r2 = atoi(p);
            while (*p >= '0' && *p <= '9') p++;
            p = skip_ws(p);
            if (*p == ',') p++;
            p = skip_ws(p);
            c2 = atoi(p);
        }
        screen_clear_region(row, col, r2, c2);

    } else if (str_imatch(p, "TO")) {
        int r2, c2;
        int dbl = 0;
        p = skip_ws(p + 2);
        r2 = atoi(p);
        while (*p >= '0' && *p <= '9') p++;
        p = skip_ws(p);
        if (*p == ',') p++;
        p = skip_ws(p);
        c2 = atoi(p);
        while (*p >= '0' && *p <= '9') p++;
        p = skip_ws(p);
        if (str_imatch(p, "DOUBLE")) dbl = 1;
        screen_box(row, col, r2, c2, dbl);

    } else {
        printf("Syntax: @ row,col SAY|GET|CLEAR TO|TO ...\n");
    }
}

int screen_get_row(void) { return scr.last_row; }
int screen_get_col(void) { return scr.last_col; }
int screen_get_prow(void) { return print_row; }
int screen_get_pcol(void) { return print_col; }

void screen_eject(void) {
    printf("\n--- PAGE ---\n");
    print_row = 0;
    print_col = 0;
}
