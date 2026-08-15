#include "sheet.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    sheet_t *sh;
    const char *p;
    int err;
} parser_t;

static void skip(parser_t *p) {
    while (*p->p == ' ' || *p->p == '\t') {
        p->p++;
    }
}

static void seterr(parser_t *p, int err) {
    if (p->err == SERR_OK) {
        p->err = err;
    }
}

static int icmp_n(const char *a, const char *b) {
    while (*a && *b) {
        int ca = toupper((unsigned char)*a);
        int cb = toupper((unsigned char)*b);
        if (ca != cb) {
            return ca - cb;
        }
        a++;
        b++;
    }
    return (unsigned char)toupper((unsigned char)*a) -
           (unsigned char)toupper((unsigned char)*b);
}

int parse_cell_name(const char *s, const char **end, int *col, int *row) {
    const char *p = s;
    int c;
    int r = 0;

    if (*p == '$') {
        p++;
    }
    if (!isalpha((unsigned char)*p)) {
        return 0;
    }
    c = toupper((unsigned char)*p) - 'A';
    p++;
    /* v1: single column letter A-Z */
    if (isalpha((unsigned char)*p) && *p != '$' && !isdigit((unsigned char)*p)) {
        return 0;
    }
    if (*p == '$') {
        p++;
    }
    if (!isdigit((unsigned char)*p)) {
        return 0;
    }
    while (isdigit((unsigned char)*p)) {
        r = r * 10 + (*p - '0');
        p++;
        if (r > SHEET_ROWS + 1) {
            break;
        }
    }
    if (c < 0 || c >= SHEET_COLS || r < 1 || r > SHEET_ROWS) {
        if (end) {
            *end = p;
        }
        *col = -1;
        *row = -1;
        return 1; /* looked like a ref, but out of range */
    }
    *col = c;
    *row = r - 1;
    if (end) {
        *end = p;
    }
    return 1;
}

void format_cell_name(int col, int row, char *buf, int cap) {
    if (cap < 4) {
        if (cap > 0) {
            buf[0] = '\0';
        }
        return;
    }
    buf[0] = (char)('A' + col);
    snprintf(buf + 1, (size_t)(cap - 1), "%d", row + 1);
}

static int parse_ref(parser_t *p, int *col, int *row) {
    const char *end = p->p;
    skip(p);
    if (!parse_cell_name(p->p, &end, col, row)) {
        return 0;
    }
    p->p = end;
    if (*col < 0) {
        seterr(p, SERR_REF);
        return 0;
    }
    return 1;
}

static double parse_expr(parser_t *p);

static double eval_cell(parser_t *p, int col, int row) {
    int err = SERR_OK;
    double v = sheet_eval(p->sh, col, row, &err);
    if (err != SERR_OK) {
        seterr(p, err);
    }
    return v;
}

enum {
    FN_SUM = 0,
    FN_AVG,
    FN_MIN,
    FN_MAX,
    FN_COUNT
};

typedef struct {
    double acc;
    int n;
    int first;
} fold_t;

static void fold_init(fold_t *f) {
    f->acc = 0.0;
    f->n = 0;
    f->first = 1;
}

static void fold_add(fold_t *f, int which, double v) {
    f->n++;
    if (which == FN_SUM || which == FN_AVG) {
        f->acc += v;
    } else if (which == FN_MIN) {
        if (f->first || v < f->acc) {
            f->acc = v;
        }
    } else if (which == FN_MAX) {
        if (f->first || v > f->acc) {
            f->acc = v;
        }
    }
    f->first = 0;
}

static double fold_finish(const fold_t *f, int which) {
    if (which == FN_COUNT) {
        return (double)f->n;
    }
    if (which == FN_AVG) {
        return f->n ? f->acc / (double)f->n : 0.0;
    }
    if ((which == FN_MIN || which == FN_MAX) && f->n == 0) {
        return 0.0;
    }
    return f->acc;
}

static void fold_range(parser_t *p, fold_t *f, int which,
                       int c1, int r1, int c2, int r2) {
    int c, r;
    int cmin = c1 < c2 ? c1 : c2;
    int cmax = c1 > c2 ? c1 : c2;
    int rmin = r1 < r2 ? r1 : r2;
    int rmax = r1 > r2 ? r1 : r2;

    for (r = rmin; r <= rmax; r++) {
        for (c = cmin; c <= cmax; c++) {
            cell_t *cell = sheet_cell(p->sh, c, r);
            int err = SERR_OK;
            double v;
            if (!cell || cell->kind == CELL_EMPTY || cell->kind == CELL_LABEL) {
                continue;
            }
            v = sheet_eval(p->sh, c, r, &err);
            if (err != SERR_OK) {
                seterr(p, err);
                return;
            }
            fold_add(f, which, v);
        }
    }
}

static double parse_primary(parser_t *p);

static double parse_func(parser_t *p) {
    char name[16];
    int nlen = 0;
    int which;
    fold_t fold;
    int c1, r1, c2, r2;
    const char *save;

    skip(p);
    while (isalpha((unsigned char)*p->p) && nlen < 15) {
        name[nlen++] = (char)toupper((unsigned char)*p->p);
        p->p++;
    }
    name[nlen] = '\0';
    skip(p);
    if (*p->p != '(') {
        seterr(p, SERR_PARSE);
        return 0.0;
    }
    p->p++;

    if (icmp_n(name, "SUM") == 0) {
        which = FN_SUM;
    } else if (icmp_n(name, "AVG") == 0 || icmp_n(name, "AVERAGE") == 0) {
        which = FN_AVG;
    } else if (icmp_n(name, "MIN") == 0) {
        which = FN_MIN;
    } else if (icmp_n(name, "MAX") == 0) {
        which = FN_MAX;
    } else if (icmp_n(name, "COUNT") == 0) {
        which = FN_COUNT;
    } else {
        seterr(p, SERR_NAME);
        return 0.0;
    }

    fold_init(&fold);
    for (;;) {
        skip(p);
        save = p->p;
        if (parse_ref(p, &c1, &r1)) {
            skip(p);
            if (*p->p == ':') {
                p->p++;
                if (!parse_ref(p, &c2, &r2)) {
                    seterr(p, SERR_PARSE);
                    return 0.0;
                }
                fold_range(p, &fold, which, c1, r1, c2, r2);
            } else {
                fold_add(&fold, which, eval_cell(p, c1, r1));
            }
        } else {
            p->p = save;
            fold_add(&fold, which, parse_expr(p));
        }
        if (p->err != SERR_OK) {
            return 0.0;
        }
        skip(p);
        if (*p->p == ',') {
            p->p++;
            continue;
        }
        break;
    }
    skip(p);
    if (*p->p != ')') {
        seterr(p, SERR_PARSE);
        return 0.0;
    }
    p->p++;
    return fold_finish(&fold, which);
}

static double parse_primary(parser_t *p) {
    skip(p);
    if (*p->p == '(') {
        double v;
        p->p++;
        v = parse_expr(p);
        skip(p);
        if (*p->p == ')') {
            p->p++;
        } else {
            seterr(p, SERR_PARSE);
        }
        return v;
    }
    if (*p->p == '@') {
        p->p++;
        return parse_func(p);
    }
    {
        int col, row;
        const char *save = p->p;
        if (parse_ref(p, &col, &row)) {
            return eval_cell(p, col, row);
        }
        p->p = save;
    }
    if (isdigit((unsigned char)*p->p) ||
        (*p->p == '.' && isdigit((unsigned char)p->p[1]))) {
        char *end = NULL;
        double v = strtod(p->p, &end);
        if (end == p->p) {
            seterr(p, SERR_PARSE);
            return 0.0;
        }
        p->p = end;
        return v;
    }
    seterr(p, SERR_PARSE);
    return 0.0;
}

static double parse_unary(parser_t *p) {
    skip(p);
    if (*p->p == '-') {
        p->p++;
        return -parse_unary(p);
    }
    if (*p->p == '+') {
        p->p++;
        return parse_unary(p);
    }
    return parse_primary(p);
}

static double parse_term(parser_t *p) {
    double left = parse_unary(p);
    for (;;) {
        skip(p);
        if (*p->p == '*') {
            p->p++;
            left *= parse_unary(p);
        } else if (*p->p == '/') {
            double rhs;
            p->p++;
            rhs = parse_unary(p);
            if (rhs == 0.0) {
                seterr(p, SERR_DIV0);
                return 0.0;
            }
            left /= rhs;
        } else {
            break;
        }
    }
    return left;
}

static double parse_expr(parser_t *p) {
    double left = parse_term(p);
    for (;;) {
        skip(p);
        if (*p->p == '+') {
            p->p++;
            left += parse_term(p);
        } else if (*p->p == '-') {
            p->p++;
            left -= parse_term(p);
        } else {
            break;
        }
    }
    return left;
}

int formula_eval(sheet_t *sh, const char *expr, double *out, int *err) {
    parser_t p;
    double v;

    p.sh = sh;
    p.p = expr;
    p.err = SERR_OK;
    skip(&p);
    if (*p.p == '=') {
        p.p++;
    }
    v = parse_expr(&p);
    skip(&p);
    if (p.err == SERR_OK && *p.p != '\0') {
        p.err = SERR_PARSE;
    }
    if (out) {
        *out = (p.err == SERR_OK) ? v : 0.0;
    }
    if (err) {
        *err = p.err;
    }
    return p.err;
}
