#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <dirent.h>
#include <unistd.h>
#include "func.h"
#include "date.h"
#include "command.h"
#include "screen.h"
#include "program.h"
#include "util.h"
#include "menu.h"

extern double floor(double x);
extern double fmod(double x, double y);

/* ---- Low-level File I/O (Clipper/FoxPro) ---- */
#define MAX_LL_FILES 16
static FILE *ll_files[MAX_LL_FILES];
static int ll_error = 0;

static int fn_fcreate(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_CHAR) {
        ctx->error = "FCREATE requires (filename)";
        return -1;
    }
    for (int i = 0; i < MAX_LL_FILES; i++) {
        if (!ll_files[i]) {
            ll_files[i] = fopen(args[0].str, "wb+");
            if (ll_files[i]) {
                ll_error = 0;
                *result = val_num((double)i + 1);
            } else {
                ll_error = errno;
                *result = val_num(-1);
            }
            return 0;
        }
    }
    ll_error = -1;
    *result = val_num(-1);
    return 0;
}

static int fn_fopen(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_CHAR) {
        ctx->error = "FOPEN requires (filename [, mode])";
        return -1;
    }
    const char *mode = "rb+";
    if (nargs >= 2 && args[1].type == VAL_NUM) {
        int m = (int)args[1].num;
        if (m == 0) mode = "rb";
        else if (m == 1) mode = "wb";
        else if (m == 2) mode = "rb+";
    }
    for (int i = 0; i < MAX_LL_FILES; i++) {
        if (!ll_files[i]) {
            ll_files[i] = fopen(args[0].str, mode);
            if (ll_files[i]) {
                ll_error = 0;
                *result = val_num((double)i + 1);
            } else {
                ll_error = errno;
                *result = val_num(-1);
            }
            return 0;
        }
    }
    ll_error = -1;
    *result = val_num(-1);
    return 0;
}

static int fn_fclose(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_NUM) {
        ctx->error = "FCLOSE requires (handle)";
        return -1;
    }
    int h = (int)args[0].num - 1;
    if (h >= 0 && h < MAX_LL_FILES && ll_files[h]) {
        fclose(ll_files[h]);
        ll_files[h] = NULL;
        ll_error = 0;
        *result = val_logic(1);
    } else {
        *result = val_logic(0);
    }
    return 0;
}

static int fn_fread(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 2 || args[0].type != VAL_NUM || args[1].type != VAL_NUM) {
        ctx->error = "FREAD requires (handle, count)";
        return -1;
    }
    int h = (int)args[0].num - 1;
    int count = (int)args[1].num;
    if (count > 255) count = 255;
    if (h >= 0 && h < MAX_LL_FILES && ll_files[h] && count > 0) {
        char *buf = (char *)malloc(count + 1);
        if (!buf) { *result = val_str(""); return 0; }
        size_t n = fread(buf, 1, count, ll_files[h]);
        buf[n] = '\0';
        *result = val_str(buf);
        free(buf);
        ll_error = ferror(ll_files[h]) ? 1 : 0;
    } else {
        *result = val_str("");
    }
    return 0;
}

static int fn_fwrite(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 2 || args[0].type != VAL_NUM || args[1].type != VAL_CHAR) {
        ctx->error = "FWRITE requires (handle, string [, count])";
        return -1;
    }
    int h = (int)args[0].num - 1;
    int count = (int)strlen(args[1].str);
    if (nargs >= 3 && args[2].type == VAL_NUM) {
        int req = (int)args[2].num;
        if (req < count) count = req;
    }
    if (count < 0) count = 0;
    if (h >= 0 && h < MAX_LL_FILES && ll_files[h]) {
        size_t n = fwrite(args[1].str, 1, count, ll_files[h]);
        *result = val_num((double)n);
        ll_error = (n < (size_t)count) ? 1 : 0;
    } else {
        *result = val_num(0);
    }
    return 0;
}

static int fn_fseek(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 2 || args[0].type != VAL_NUM || args[1].type != VAL_NUM) {
        ctx->error = "FSEEK requires (handle, offset [, origin])";
        return -1;
    }
    int h = (int)args[0].num - 1;
    long offset = (long)args[1].num;
    int origin = SEEK_SET;
    if (nargs >= 3 && args[2].type == VAL_NUM) {
        int o = (int)args[2].num;
        if (o == 1) origin = SEEK_CUR;
        else if (o == 2) origin = SEEK_END;
    }
    if (h >= 0 && h < MAX_LL_FILES && ll_files[h]) {
        fseek(ll_files[h], offset, origin);
        *result = val_num((double)ftell(ll_files[h]));
        ll_error = 0;
    } else {
        *result = val_num(-1);
    }
    return 0;
}

static int fn_ferror(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_num((double)ll_error);
    return 0;
}

void ll_close_all(void) {
    for (int i = 0; i < MAX_LL_FILES; i++) {
        if (ll_files[i]) {
            fclose(ll_files[i]);
            ll_files[i] = NULL;
        }
    }
}

/* ---- Inline sqrt via Newton's method (hardware sqrt HALTs on slow32) ---- */
static double my_sqrt(double x) {
    double guess, prev;
    int i;
    if (x <= 0.0) return 0.0;
    guess = x;
    /* Rough initial guess */
    if (x > 1.0) guess = x / 2.0;
    for (i = 0; i < 30; i++) {
        prev = guess;
        guess = (guess + x / guess) / 2.0;
        /* Check convergence */
        if (guess == prev) break;
    }
    return guess;
}

static double my_fabs(double x) {
    return x < 0.0 ? -x : x;
}

/* ---- Status functions ---- */

static int fn_eof(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)args; (void)nargs;
    if (!ctx->db || !dbf_is_open(ctx->db))
        *result = val_logic(1);
    else
        *result = val_logic(ctx->eof_flag);
    return 0;
}

static int fn_bof(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)args; (void)nargs;
    *result = val_logic(ctx->bof_flag);
    return 0;
}

static int fn_recno(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)args; (void)nargs;
    if (!ctx->db || !dbf_is_open(ctx->db))
        *result = val_num(0);
    else
        *result = val_num((double)ctx->db->current_record);
    return 0;
}

static int fn_reccount(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)args; (void)nargs;
    if (!ctx->db || !dbf_is_open(ctx->db))
        *result = val_num(0);
    else
        *result = val_num((double)ctx->db->record_count);
    return 0;
}

static int fn_deleted(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)args; (void)nargs;
    if (!ctx->db || !dbf_is_open(ctx->db) || ctx->db->current_record == 0)
        *result = val_logic(0);
    else
        *result = val_logic(ctx->db->record_buf[0] == '*');
    return 0;
}

static int fn_found(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)args; (void)nargs;
    *result = val_logic(ctx->found);
    return 0;
}

/* ---- String functions ---- */

static int fn_substr(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    int start, len, slen;
    (void)ctx;
    if (nargs < 2 || args[0].type != VAL_CHAR || args[1].type != VAL_NUM) {
        ctx->error = "SUBSTR requires (string, start [, length])";
        return -1;
    }
    slen = strlen(args[0].str);
    start = (int)args[1].num - 1; /* 1-based to 0-based */
    if (start < 0) start = 0;
    if (start >= slen) { *result = val_str(""); return 0; }

    len = slen - start;
    if (nargs >= 3 && args[2].type == VAL_NUM) {
        int req = (int)args[2].num;
        if (req < len) len = req;
    }
    if (len < 0) len = 0;

    {
        char buf[256];
        if (len > (int)sizeof(buf) - 1) len = (int)sizeof(buf) - 1;
        memcpy(buf, args[0].str + start, len);
        buf[len] = '\0';
        *result = val_str(buf);
    }
    return 0;
}

static int fn_trim(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char buf[256];
    int len;
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_CHAR) {
        ctx->error = "TRIM requires (string)";
        return -1;
    }
    str_copy(buf, args[0].str, sizeof(buf));
    len = strlen(buf);
    while (len > 0 && buf[len-1] == ' ') len--;
    buf[len] = '\0';
    *result = val_str(buf);
    return 0;
}

static int fn_ltrim(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    const char *p;
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_CHAR) {
        ctx->error = "LTRIM requires (string)";
        return -1;
    }
    p = args[0].str;
    while (*p == ' ') p++;
    *result = val_str(p);
    return 0;
}

static int fn_alltrim(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char buf[256];
    int len;
    const char *p;
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_CHAR) {
        ctx->error = "ALLTRIM requires (string)";
        return -1;
    }
    p = args[0].str;
    while (*p == ' ') p++;
    str_copy(buf, p, sizeof(buf));
    len = strlen(buf);
    while (len > 0 && buf[len - 1] == ' ') len--;
    buf[len] = '\0';
    *result = val_str(buf);
    return 0;
}

static int fn_upper(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char buf[256];
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_CHAR) {
        ctx->error = "UPPER requires (string)";
        return -1;
    }
    str_copy(buf, args[0].str, sizeof(buf));
    str_upper(buf);
    *result = val_str(buf);
    return 0;
}

static int fn_lower(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char buf[256];
    int i;
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_CHAR) {
        ctx->error = "LOWER requires (string)";
        return -1;
    }
    str_copy(buf, args[0].str, sizeof(buf));
    for (i = 0; buf[i]; i++)
        if (buf[i] >= 'A' && buf[i] <= 'Z') buf[i] += 32;
    *result = val_str(buf);
    return 0;
}

static int fn_at(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    const char *p;
    (void)ctx;
    if (nargs < 2 || args[0].type != VAL_CHAR || args[1].type != VAL_CHAR) {
        ctx->error = "AT requires (search, string)";
        return -1;
    }
    p = strstr(args[1].str, args[0].str);
    *result = val_num(p ? (double)(p - args[1].str + 1) : 0.0);
    return 0;
}

static int fn_len(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_CHAR) {
        ctx->error = "LEN requires (string)";
        return -1;
    }
    *result = val_num((double)strlen(args[0].str));
    return 0;
}

static int fn_space(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char buf[256];
    int n, i;
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_NUM) {
        ctx->error = "SPACE requires (number)";
        return -1;
    }
    n = (int)args[0].num;
    if (n < 0) n = 0;
    if (n > 255) n = 255;
    for (i = 0; i < n; i++) buf[i] = ' ';
    buf[n] = '\0';
    *result = val_str(buf);
    return 0;
}

static int fn_replicate(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char buf[256];
    int n, slen, i, pos;
    (void)ctx;
    if (nargs < 2 || args[0].type != VAL_CHAR || args[1].type != VAL_NUM) {
        ctx->error = "REPLICATE requires (string, count)";
        return -1;
    }
    n = (int)args[1].num;
    slen = strlen(args[0].str);
    pos = 0;
    for (i = 0; i < n && pos + slen < (int)sizeof(buf); i++) {
        memcpy(buf + pos, args[0].str, slen);
        pos += slen;
    }
    buf[pos] = '\0';
    *result = val_str(buf);
    return 0;
}

static int fn_padr(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char buf[256];
    int width, slen, i;
    char pad = ' ';
    (void)ctx;
    if (nargs < 2 || args[0].type != VAL_CHAR || args[1].type != VAL_NUM) {
        ctx->error = "PADR requires (string, width [, char])";
        return -1;
    }
    width = (int)args[1].num;
    if (width < 0) width = 0;
    if (width > (int)sizeof(buf) - 1) width = (int)sizeof(buf) - 1;
    if (nargs >= 3 && args[2].type == VAL_CHAR && args[2].str[0]) pad = args[2].str[0];

    slen = strlen(args[0].str);
    if (slen >= width) {
        memcpy(buf, args[0].str, width);
        buf[width] = '\0';
    } else {
        memcpy(buf, args[0].str, slen);
        for (i = slen; i < width; i++) buf[i] = pad;
        buf[width] = '\0';
    }
    *result = val_str(buf);
    return 0;
}

static int fn_padl(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char buf[256];
    int width, slen, i, padlen;
    char pad = ' ';
    (void)ctx;
    if (nargs < 2 || args[0].type != VAL_CHAR || args[1].type != VAL_NUM) {
        ctx->error = "PADL requires (string, width [, char])";
        return -1;
    }
    width = (int)args[1].num;
    if (width < 0) width = 0;
    if (width > (int)sizeof(buf) - 1) width = (int)sizeof(buf) - 1;
    if (nargs >= 3 && args[2].type == VAL_CHAR && args[2].str[0]) pad = args[2].str[0];

    slen = strlen(args[0].str);
    if (slen >= width) {
        memcpy(buf, args[0].str + (slen - width), width);
        buf[width] = '\0';
    } else {
        padlen = width - slen;
        for (i = 0; i < padlen; i++) buf[i] = pad;
        memcpy(buf + padlen, args[0].str, slen);
        buf[width] = '\0';
    }
    *result = val_str(buf);
    return 0;
}

static int fn_padc(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char buf[256];
    int width, slen, i, lpad, rpad;
    char pad = ' ';
    (void)ctx;
    if (nargs < 2 || args[0].type != VAL_CHAR || args[1].type != VAL_NUM) {
        ctx->error = "PADC requires (string, width [, char])";
        return -1;
    }
    width = (int)args[1].num;
    if (width < 0) width = 0;
    if (width > (int)sizeof(buf) - 1) width = (int)sizeof(buf) - 1;
    if (nargs >= 3 && args[2].type == VAL_CHAR && args[2].str[0]) pad = args[2].str[0];

    slen = strlen(args[0].str);
    if (slen >= width) {
        lpad = (slen - width) / 2;
        memcpy(buf, args[0].str + lpad, width);
        buf[width] = '\0';
    } else {
        lpad = (width - slen) / 2;
        rpad = width - slen - lpad;
        for (i = 0; i < lpad; i++) buf[i] = pad;
        memcpy(buf + lpad, args[0].str, slen);
        for (i = 0; i < rpad; i++) buf[lpad + slen + i] = pad;
        buf[width] = '\0';
    }
    *result = val_str(buf);
    return 0;
}

static int fn_left(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char buf[256];
    int n, slen;
    (void)ctx;
    if (nargs < 2 || args[0].type != VAL_CHAR || args[1].type != VAL_NUM) {
        ctx->error = "LEFT requires (string, count)";
        return -1;
    }
    n = (int)args[1].num;
    slen = strlen(args[0].str);
    if (n > slen) n = slen;
    if (n < 0) n = 0;
    if (n > (int)sizeof(buf) - 1) n = (int)sizeof(buf) - 1;
    memcpy(buf, args[0].str, n);
    buf[n] = '\0';
    *result = val_str(buf);
    return 0;
}

static int fn_right(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char buf[256];
    int n, slen, start;
    (void)ctx;
    if (nargs < 2 || args[0].type != VAL_CHAR || args[1].type != VAL_NUM) {
        ctx->error = "RIGHT requires (string, count)";
        return -1;
    }
    n = (int)args[1].num;
    slen = strlen(args[0].str);
    if (n > slen) n = slen;
    if (n < 0) n = 0;
    start = slen - n;
    if (n > (int)sizeof(buf) - 1) n = (int)sizeof(buf) - 1;
    memcpy(buf, args[0].str + start, n);
    buf[n] = '\0';
    *result = val_str(buf);
    return 0;
}

static int fn_str(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char buf[64];
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_NUM) {
        ctx->error = "STR requires (number [, width [, decimals]])";
        return -1;
    }
    {
        int width = 10;
        int dec = 0;
        if (nargs >= 2 && args[1].type == VAL_NUM) width = (int)args[1].num;
        if (nargs >= 3 && args[2].type == VAL_NUM) dec = (int)args[2].num;
        if (width > (int)sizeof(buf) - 1) width = (int)sizeof(buf) - 1;
        if (dec > 0)
            snprintf(buf, sizeof(buf), "%*.*f", width, dec, args[0].num);
        else {
            double n = args[0].num;
            int rounded = (n >= 0.0) ? (int)(n + 0.5) : (int)(n - 0.5);
            snprintf(buf, sizeof(buf), "%*d", width, rounded);
        }
    }
    *result = val_str(buf);
    return 0;
}

static int fn_val(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_CHAR) {
        ctx->error = "VAL requires (string)";
        return -1;
    }
    *result = val_num(atof(args[0].str));
    return 0;
}

static int fn_chr(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char buf[2];
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_NUM) {
        ctx->error = "CHR requires (number)";
        return -1;
    }
    buf[0] = (char)(int)args[0].num;
    buf[1] = '\0';
    *result = val_str(buf);
    return 0;
}

static int fn_asc(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_CHAR || args[0].str[0] == '\0') {
        ctx->error = "ASC requires (string)";
        return -1;
    }
    *result = val_num((double)(unsigned char)args[0].str[0]);
    return 0;
}

static int fn_type(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 1) {
        *result = val_str("U");
        return 0;
    }
    switch (args[0].type) {
    case VAL_NUM:   *result = val_str("N"); break;
    case VAL_CHAR:  *result = val_str("C"); break;
    case VAL_DATE:  *result = val_str("D"); break;
    case VAL_LOGIC: *result = val_str("L"); break;
    default:        *result = val_str("U"); break;
    }
    return 0;
}

/* ---- Numeric functions ---- */

static int fn_int(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_NUM) {
        ctx->error = "INT requires (number)";
        return -1;
    }
    *result = val_num((double)(int)args[0].num);
    return 0;
}

static int fn_round(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    double n, p;
    (void)ctx;
    if (nargs < 2 || args[0].type != VAL_NUM || args[1].type != VAL_NUM) {
        ctx->error = "ROUND requires (number, decimals)";
        return -1;
    }
    n = args[0].num;
    {
        int dec = (int)args[1].num;
        int i;
        p = 1.0;
        for (i = 0; i < dec; i++) p *= 10.0;
        *result = val_num(floor(n * p + 0.5) / p);
    }
    return 0;
}

static int fn_abs(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_NUM) {
        ctx->error = "ABS requires (number)";
        return -1;
    }
    *result = val_num(my_fabs(args[0].num));
    return 0;
}

static int fn_mod(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 2 || args[0].type != VAL_NUM || args[1].type != VAL_NUM) {
        ctx->error = "MOD requires (number, divisor)";
        return -1;
    }
    if (args[1].num == 0.0) {
        ctx->error = "Division by zero in MOD";
        return -1;
    }
    *result = val_num(fmod(args[0].num, args[1].num));
    return 0;
}

static int fn_sqrt(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_NUM) {
        ctx->error = "SQRT requires (number)";
        return -1;
    }
    *result = val_num(my_sqrt(args[0].num));
    return 0;
}

static int fn_max(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 2 || args[0].type != VAL_NUM || args[1].type != VAL_NUM) {
        ctx->error = "MAX requires (number, number)";
        return -1;
    }
    *result = val_num(args[0].num > args[1].num ? args[0].num : args[1].num);
    return 0;
}

static int fn_min(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 2 || args[0].type != VAL_NUM || args[1].type != VAL_NUM) {
        ctx->error = "MIN requires (number, number)";
        return -1;
    }
    *result = val_num(args[0].num < args[1].num ? args[0].num : args[1].num);
    return 0;
}

/* ---- Date functions ---- */

static int fn_date(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_date(date_today());
    return 0;
}

static int fn_dtoc(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char buf[16];
    if (nargs < 1 || args[0].type != VAL_DATE) {
        ctx->error = "DTOC requires (date)";
        return -1;
    }
    date_to_display(args[0].date, buf, cmd_get_date_format(), cmd_get_century());
    *result = val_str(buf);
    return 0;
}

static int fn_ctod(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_CHAR) {
        ctx->error = "CTOD requires (string)";
        return -1;
    }
    *result = val_date(date_from_display(args[0].str, cmd_get_date_format()));
    return 0;
}

static int fn_day(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    int y, m, d;
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_DATE) {
        ctx->error = "DAY requires (date)";
        return -1;
    }
    date_from_jdn(args[0].date, &y, &m, &d);
    *result = val_num((double)d);
    return 0;
}

static int fn_month(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    int y, m, d;
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_DATE) {
        ctx->error = "MONTH requires (date)";
        return -1;
    }
    date_from_jdn(args[0].date, &y, &m, &d);
    *result = val_num((double)m);
    return 0;
}

static int fn_year(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    int y, m, d;
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_DATE) {
        ctx->error = "YEAR requires (date)";
        return -1;
    }
    date_from_jdn(args[0].date, &y, &m, &d);
    *result = val_num((double)y);
    return 0;
}

static int fn_dow(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_DATE) {
        ctx->error = "DOW requires (date)";
        return -1;
    }
    *result = val_num((double)date_dow(args[0].date));
    return 0;
}

static int fn_cdow(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_DATE) {
        ctx->error = "CDOW requires (date)";
        return -1;
    }
    *result = val_str(date_dow_name(date_dow(args[0].date)));
    return 0;
}

static int fn_cmonth(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    int y, m, d;
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_DATE) {
        ctx->error = "CMONTH requires (date)";
        return -1;
    }
    date_from_jdn(args[0].date, &y, &m, &d);
    *result = val_str(date_month_name(m));
    return 0;
}

static int fn_empty(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 1) {
        *result = val_logic(1);
        return 0;
    }
    switch (args[0].type) {
    case VAL_CHAR: {
        const char *p = args[0].str;
        while (*p == ' ') p++;
        *result = val_logic(*p == '\0');
        break;
    }
    case VAL_NUM:
        *result = val_logic(args[0].num == 0.0);
        break;
    case VAL_LOGIC:
        *result = val_logic(args[0].logic == 0);
        break;
    case VAL_DATE:
        *result = val_logic(args[0].date == 0);
        break;
    default:
        *result = val_logic(1);
        break;
    }
    return 0;
}

/* ---- IIF (inline if) ---- */

static int fn_iif(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 3) {
        ctx->error = "IIF requires (cond, true_val, false_val)";
        return -1;
    }
    /* Evaluate condition: numeric non-zero or logical true */
    int cond = 0;
    if (args[0].type == VAL_LOGIC) cond = args[0].logic;
    else if (args[0].type == VAL_NUM) cond = (args[0].num != 0.0);
    *result = cond ? args[1] : args[2];
    return 0;
}

/* ---- FILE (test file existence) ---- */

static int fn_file(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_CHAR) {
        ctx->error = "FILE requires (filename)";
        return -1;
    }
    {
        FILE *f = fopen(args[0].str, "r");
        if (!f) {
            char up[256];
            str_copy(up, args[0].str, sizeof(up));
            str_upper(up);
            f = fopen(up, "r");
        }
        if (f) {
            fclose(f);
            *result = val_logic(1);
        } else {
            *result = val_logic(0);
        }
    }
    return 0;
}

/* ---- DTOS (date to string YYYYMMDD for indexing) ---- */

static int fn_dtos(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    int y, m, d;
    char buf[16];
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_DATE) {
        ctx->error = "DTOS requires (date)";
        return -1;
    }
    date_from_jdn(args[0].date, &y, &m, &d);
    buf[0] = '0' + (y / 1000) % 10;
    buf[1] = '0' + (y / 100) % 10;
    buf[2] = '0' + (y / 10) % 10;
    buf[3] = '0' + y % 10;
    buf[4] = '0' + (m / 10);
    buf[5] = '0' + (m % 10);
    buf[6] = '0' + (d / 10);
    buf[7] = '0' + (d % 10);
    buf[8] = '\0';
    *result = val_str(buf);
    return 0;
}

/* ---- Screen position ---- */

static int fn_row(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_num((double)screen_get_row());
    return 0;
}

static int fn_col(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_num((double)screen_get_col());
    return 0;
}

static int fn_prow(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_num((double)screen_get_prow());
    return 0;
}

static int fn_pcol(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_num((double)screen_get_pcol());
    return 0;
}

/* ---- BAR() / PROMPT() — popup menu result ---- */
static int fn_bar(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_num((double)menu_last_bar());
    return 0;
}

static int fn_prompt_func(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_str(menu_last_prompt());
    return 0;
}

static int fn_pad(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_str(menu_last_pad());
    return 0;
}

static int fn_popup_func(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_str(menu_last_popup());
    return 0;
}

/* ---- STUFF(str, start, delete, insert) ---- */
static int fn_stuff(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char buf[512];
    const char *s;
    int start, del, slen, ilen;
    (void)ctx;
    if (nargs < 4 || args[0].type != VAL_CHAR || args[3].type != VAL_CHAR) {
        *result = val_str("");
        return 0;
    }
    s = args[0].str;
    start = (int)args[1].num;
    del = (int)args[2].num;
    slen = strlen(s);
    ilen = strlen(args[3].str);
    if (start < 1) start = 1;
    if (start > slen + 1) start = slen + 1;
    if (del < 0) del = 0;
    if (start - 1 + del > slen) del = slen - (start - 1);
    /* Copy prefix */
    if (start - 1 > 0 && start - 1 < (int)sizeof(buf))
        memcpy(buf, s, start - 1);
    /* Copy insert string */
    if (start - 1 + ilen < (int)sizeof(buf))
        memcpy(buf + start - 1, args[3].str, ilen);
    /* Copy suffix */
    {
        int suffix_start = start - 1 + del;
        int suffix_len = slen - suffix_start;
        if (suffix_len > 0 && start - 1 + ilen + suffix_len < (int)sizeof(buf))
            memcpy(buf + start - 1 + ilen, s + suffix_start, suffix_len);
        buf[start - 1 + ilen + (suffix_len > 0 ? suffix_len : 0)] = '\0';
    }
    *result = val_str(buf);
    return 0;
}

/* ---- TRANSFORM(expr, picture) ---- */
static int fn_transform(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char buf[256], formatted[256];
    (void)ctx;
    if (nargs < 2) { *result = val_str(""); return 0; }
    val_to_string(&args[0], buf, sizeof(buf));
    /* Reuse apply_picture logic inline */
    {
        const char *value = buf;
        const char *picture = (args[1].type == VAL_CHAR) ? args[1].str : "";
        int plen = strlen(picture);
        int vlen = strlen(value);
        int vi = 0, i;
        if (plen == 0) {
            *result = val_str(buf);
            return 0;
        }
        for (i = 0; i < plen && i < (int)sizeof(formatted) - 1; i++) {
            char pc = picture[i];
            char vc = (vi < vlen) ? value[vi] : ' ';
            switch (pc) {
            case '!': if (vc >= 'a' && vc <= 'z') vc -= 32; formatted[i] = vc; vi++; break;
            case '9': formatted[i] = (vc >= '0' && vc <= '9') ? vc : ' '; vi++; break;
            case 'A': formatted[i] = ((vc >= 'A' && vc <= 'Z') || (vc >= 'a' && vc <= 'z')) ? vc : ' '; vi++; break;
            case 'X': formatted[i] = vc; vi++; break;
            case '#': formatted[i] = (vc >= '0' && vc <= '9' || vc == ' ' || vc == '+' || vc == '-') ? vc : ' '; vi++; break;
            default: formatted[i] = pc; break;
            }
        }
        formatted[i] = '\0';
    }
    *result = val_str(formatted);
    return 0;
}

/* ---- ISALPHA / ISUPPER / ISLOWER ---- */
static int fn_isalpha(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_CHAR || args[0].str[0] == '\0') {
        *result = val_logic(0); return 0;
    }
    { char c = args[0].str[0];
      *result = val_logic((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')); }
    return 0;
}

static int fn_isupper(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_CHAR || args[0].str[0] == '\0') {
        *result = val_logic(0); return 0;
    }
    { char c = args[0].str[0]; *result = val_logic(c >= 'A' && c <= 'Z'); }
    return 0;
}

static int fn_islower(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_CHAR || args[0].str[0] == '\0') {
        *result = val_logic(0); return 0;
    }
    { char c = args[0].str[0]; *result = val_logic(c >= 'a' && c <= 'z'); }
    return 0;
}

/* ---- VERSION / OS ---- */
static int fn_curdir(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char buf[256];
    (void)ctx; (void)args; (void)nargs;
    if (getcwd(buf, sizeof(buf))) {
        *result = val_str(buf);
    } else {
        *result = val_str("");
    }
    return 0;
}

static int fn_adir(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    int count = 0;
    DIR *dir;
    struct dirent *ent;
    const char *path = ".";
    const char *pattern = NULL;

    (void)ctx;
    if (nargs >= 1 && args[0].type == VAL_CHAR) pattern = args[0].str;

    dir = opendir(path);
    if (!dir) {
        *result = val_num(0);
        return 0;
    }

    while ((ent = readdir(dir)) != NULL) {
        if (ent->d_name[0] == '.' && (ent->d_name[1] == '\0' ||
            (ent->d_name[1] == '.' && ent->d_name[2] == '\0')))
            continue;  /* skip . and .. */
        if (pattern) {
            if (str_like(ent->d_name, pattern)) count++;
        } else {
            count++;
        }
    }
    closedir(dir);

    *result = val_num((double)count);
    return 0;
}

static int fn_version(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_str("dBASE III Clone");
    return 0;
}

static int fn_os(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_str("SLOW-32");
    return 0;
}

/* ---- INKEY / LASTKEY / READKEY ---- */
static int fn_inkey(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    double timeout;
    (void)ctx;
    /* INKEY() = poll, INKEY(0) = wait forever, INKEY(n) = wait n seconds */
    if (nargs >= 1 && args[0].type == VAL_NUM) {
        timeout = args[0].num;
        if (timeout == 0.0) timeout = -1.0;  /* dBase: INKEY(0) = wait forever */
    } else {
        timeout = 0.0;  /* INKEY() with no args = poll */
    }
    *result = val_num((double)screen_inkey(timeout));
    return 0;
}

static int fn_lastkey(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_num((double)screen_lastkey());
    return 0;
}

static int fn_readkey(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_num((double)screen_readkey());
    return 0;
}

/* ---- ERROR / MESSAGE / LINENO / PROGRAM ---- */
static int fn_error(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_num((double)prog_get_error_code());
    return 0;
}

static int fn_message(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_str(prog_get_error_message());
    return 0;
}

static int fn_lineno(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_num((double)prog_get_lineno());
    return 0;
}

static int fn_program(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_str(prog_get_program_name());
    return 0;
}

/* ---- Dispatch table ---- */

typedef int (*func_impl_t)(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);

typedef struct {
    const char *name;
    func_impl_t fn;
} func_entry_t;

static const func_entry_t func_table[] = {
    /* Status */
    { "EOF",       fn_eof },
    { "BOF",       fn_bof },
    { "RECNO",     fn_recno },
    { "RECCOUNT",  fn_reccount },
    { "DELETED",   fn_deleted },
    { "FOUND",     fn_found },
    /* String */
    { "SUBSTR",    fn_substr },
    { "TRIM",      fn_trim },
    { "RTRIM",     fn_trim },     /* alias */
    { "LTRIM",     fn_ltrim },
    { "ALLTRIM",   fn_alltrim },
    { "UPPER",     fn_upper },
    { "LOWER",     fn_lower },
    { "AT",        fn_at },
    { "LEN",       fn_len },
    { "SPACE",     fn_space },
    { "REPLICATE", fn_replicate },
    { "PADL",      fn_padl },
    { "PADR",      fn_padr },
    { "PADC",      fn_padc },
    { "LEFT",      fn_left },
    { "RIGHT",     fn_right },
    { "STR",       fn_str },
    { "VAL",       fn_val },
    { "CHR",       fn_chr },
    { "ASC",       fn_asc },
    { "TYPE",      fn_type },
    /* Numeric */
    { "INT",       fn_int },
    { "ROUND",     fn_round },
    { "ABS",       fn_abs },
    { "MOD",       fn_mod },
    { "SQRT",      fn_sqrt },
    { "MAX",       fn_max },
    { "MIN",       fn_min },
    /* Date */
    { "DATE",      fn_date },
    { "DTOC",      fn_dtoc },
    { "CTOD",      fn_ctod },
    { "DAY",       fn_day },
    { "MONTH",     fn_month },
    { "YEAR",      fn_year },
    { "DOW",       fn_dow },
    { "CDOW",      fn_cdow },
    { "CMONTH",    fn_cmonth },
    { "DTOS",      fn_dtos },
    /* Screen */
    { "ROW",       fn_row },
    { "COL",       fn_col },
    { "PROW",      fn_prow },
    { "PCOL",      fn_pcol },
    /* Popup menu */
    { "BAR",       fn_bar },
    { "PROMPT",    fn_prompt_func },
    /* Menu bar */
    { "PAD",       fn_pad },
    { "POPUP",     fn_popup_func },
    /* String manipulation */
    { "STUFF",     fn_stuff },
    { "TRANSFORM", fn_transform },
    { "ISALPHA",   fn_isalpha },
    { "ISUPPER",   fn_isupper },
    { "ISLOWER",   fn_islower },
    /* Directory and File Services */
    { "ADIR",      fn_adir },
    { "CURDIR",    fn_curdir },
    { "FILE",      fn_file },
    /* Low-level File I/O */
    { "FOPEN",     fn_fopen },
    { "FCREATE",   fn_fcreate },
    { "FCLOSE",    fn_fclose },
    { "FREAD",     fn_fread },
    { "FWRITE",    fn_fwrite },
    { "FSEEK",     fn_fseek },
    { "FERROR",    fn_ferror },
    /* Misc */
    { "EMPTY",     fn_empty },
    { "IIF",       fn_iif },
    /* System */
    { "VERSION",   fn_version },
    { "OS",        fn_os },
    /* Keyboard */
    { "INKEY",     fn_inkey },
    { "LASTKEY",   fn_lastkey },
    { "READKEY",   fn_readkey },
    /* Debug */
    { "ERROR",     fn_error },
    { "MESSAGE",   fn_message },
    { "LINENO",    fn_lineno },
    { "PROGRAM",   fn_program },
    { NULL, NULL }
};

static udf_callback_t udf_callback;

void func_set_udf_callback(udf_callback_t cb) {
    udf_callback = cb;
}

int func_call(expr_ctx_t *ctx, const char *name, value_t *args, int nargs, value_t *result) {
    const func_entry_t *e;
    char upper[64];
    int i;

    /* Uppercase the name for comparison */
    for (i = 0; name[i] && i < (int)sizeof(upper) - 1; i++) {
        char c = name[i];
        if (c >= 'a' && c <= 'z') c -= 32;
        upper[i] = c;
    }
    upper[i] = '\0';

    for (e = func_table; e->name; e++) {
        if (strcmp(upper, e->name) == 0)
            return e->fn(ctx, args, nargs, result);
    }

    /* Try user-defined function */
    if (udf_callback && udf_callback(upper, args, nargs, result) == 0)
        return 0;

    {
        snprintf(ctx->err_msg, sizeof(ctx->err_msg), "Unknown function: %s", name);
        ctx->error = ctx->err_msg;
    }
    return -1;
}
