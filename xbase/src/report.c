#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "ast.h"
#include "report.h"
#include "date.h"
#include "util.h"
#include "func.h"
#include "memvar.h"

void frm_init(frm_def_t *def) {
    memset(def, 0, sizeof(*def));
    def->page_width = 80;
    def->lines_per_page = 58;
    def->left_margin = 8;
    def->right_margin = 0;
}

/* ---- Binary .FRM reader ---- */

static uint16_t read_le16(const unsigned char *p) {
    return p[0] | (p[1] << 8);
}

static void write_le16(unsigned char *p, uint16_t v) {
    p[0] = v & 0xFF;
    p[1] = (v >> 8) & 0xFF;
}

/* The expression table, as dBASE III wrote it and Clipper's FRMBACK.PRG
 * reads it (CA's SYS sources): lengths at 4 + idx*2 (each counting the
 * NUL that ends the text), offsets at 114 + idx*2, the texts at 224,
 * NUL-terminated; pointers to expressions are 0-based, 65535 for none.
 * (Ours were 1-based with lengths that left the NUL out: Clipper read
 * every expression one character short and one entry off.) */
#define FRM_NONE 0xFFFF

static void extract_expr(const unsigned char *buf, int idx, char *out, int outsize) {
    uint16_t len, off;
    out[0] = '\0';
    if (idx == FRM_NONE || idx >= FRM_MAX_EXPRS) return;
    len = read_le16(buf + 4 + idx * 2);
    off = read_le16(buf + 114 + idx * 2);
    if (len == 0 || off == FRM_NONE) return;
    if (len > 0 && buf[224 + off + len - 1] == '\0') len--;   /* the NUL counted */
    if (off + len > FRM_EXPR_BUF_SIZE) return;
    {
        int n = (len < outsize - 1) ? len : outsize - 1;
        memcpy(out, buf + 224 + off, n);
        out[n] = '\0';
        trim_right(out);
    }
}

static int yes_byte(unsigned char c) { return c == 'Y' || c == 'y' || c == 1; }

int frm_read(const char *filename, frm_def_t *def) {
    unsigned char buf[FRM_FILE_SIZE];
    FILE *fp;
    int i;

    fp = fopen(filename, "rb");
    if (!fp) return -1;

    memset(buf, 0, sizeof(buf));
    if (fread(buf, 1, FRM_FILE_SIZE, fp) < FRM_FILE_SIZE) {
        fclose(fp);
        return -1;
    }
    fclose(fp);

    /* Verify marker */
    if (read_le16(buf) != 0x0002) return -1;

    frm_init(def);

    /* The parameter block at 1964: expression pointers for the page
     * heading, group, sub-group and their headings; then the numbers;
     * then Y/N bytes for double spacing, summary and group eject, and
     * an option byte (1 plain, 2 eject after, 4 no eject before) */
    extract_expr(buf, read_le16(buf + 1964 + 0), def->title, sizeof(def->title));
    extract_expr(buf, read_le16(buf + 1964 + 2), def->group_expr, sizeof(def->group_expr));
    extract_expr(buf, read_le16(buf + 1964 + 4), def->subgroup_expr, sizeof(def->subgroup_expr));
    extract_expr(buf, read_le16(buf + 1964 + 6), def->group_header, sizeof(def->group_header));
    extract_expr(buf, read_le16(buf + 1964 + 8), def->subgroup_header, sizeof(def->subgroup_header));

    def->page_width    = read_le16(buf + 1964 + 10);
    def->lines_per_page= read_le16(buf + 1964 + 12);
    def->left_margin   = read_le16(buf + 1964 + 14);
    def->right_margin  = read_le16(buf + 1964 + 16);
    def->num_columns   = read_le16(buf + 1964 + 18);
    def->double_space  = yes_byte(buf[1964 + 20]);
    def->summary_only  = yes_byte(buf[1964 + 21]);
    def->eject_after   = (buf[1964 + 23] & 2) != 0;
    def->frm_plain     = (buf[1964 + 23] & 4) != 0;
    def->no_eject_before = (buf[1964 + 23] & 1) != 0;
    def->group_eject   = yes_byte(buf[1964 + 22]);

    if (def->num_columns > FRM_MAX_COLUMNS)
        def->num_columns = FRM_MAX_COLUMNS;

    /* Column definitions: 12 bytes each from offset 1676 (slot 0 unused): the width,
     * a Y/N totals byte at 5, decimals at 6, the content and heading
     * expression pointers at 8 and 10 */
    for (i = 0; i < def->num_columns; i++) {
        int coff = 1676 + i * 12;   /* the first 12-byte slot is unused: dBASE skips it, so Clipper's reader does */
        def->columns[i].width    = read_le16(buf + coff + 0);
        def->columns[i].totals   = yes_byte(buf[coff + 5]);
        def->columns[i].decimals = read_le16(buf + coff + 6);
        extract_expr(buf, read_le16(buf + coff + 8), def->columns[i].content, sizeof(def->columns[i].content));
        extract_expr(buf, read_le16(buf + coff + 10), def->columns[i].header, sizeof(def->columns[i].header));
    }

    return 0;
}

/* ---- Binary .FRM writer ---- */

int frm_write(const char *filename, const frm_def_t *def) {
    unsigned char buf[FRM_FILE_SIZE];
    FILE *fp;
    int expr_count = 0;
    int data_offset = 0;
    int i;

    memset(buf, 0, sizeof(buf));

    /* Marker */
    write_le16(buf, 0x0002);

    /* Build expression table */
    /* We assign expressions in order:
       1: title, 2: group, 3: subgroup, 4: group_hdr, 5: subgroup_hdr
       Then 2 per column (content, header) */

    /* Helper: store an expression, NUL-terminated, its length counting
     * the NUL, its pointer 0-based (FRM_NONE for an empty one) */
    #define STORE_EXPR(str, idx_out) do { \
        int _len = (int)strlen(str); \
        if (_len > 0 && expr_count < FRM_MAX_EXPRS && data_offset + _len + 1 <= FRM_EXPR_BUF_SIZE) { \
            write_le16(buf + 4 + expr_count * 2, (uint16_t)(_len + 1)); \
            write_le16(buf + 114 + expr_count * 2, (uint16_t)data_offset); \
            memcpy(buf + 224 + data_offset, str, _len); \
            buf[224 + data_offset + _len] = '\0'; \
            data_offset += _len + 1; \
            idx_out = (uint16_t)expr_count; \
            expr_count++; \
        } else { \
            idx_out = FRM_NONE; \
        } \
    } while(0)

    {
        uint16_t title_idx, group_idx, subgroup_idx, grp_hdr_idx, subgrp_hdr_idx;

        STORE_EXPR(def->title, title_idx);
        STORE_EXPR(def->group_expr, group_idx);
        STORE_EXPR(def->subgroup_expr, subgroup_idx);
        STORE_EXPR(def->group_header, grp_hdr_idx);
        STORE_EXPR(def->subgroup_header, subgrp_hdr_idx);

        for (i = 0; i < def->num_columns; i++) {
            uint16_t cidx, hidx;
            int coff = 1676 + i * 12;   /* the first 12-byte slot is unused: dBASE skips it, so Clipper's reader does */
            STORE_EXPR(def->columns[i].content, cidx);
            STORE_EXPR(def->columns[i].header, hidx);
            write_le16(buf + coff + 0, (uint16_t)def->columns[i].width);
            buf[coff + 5] = def->columns[i].totals ? 'Y' : 'N';
            write_le16(buf + coff + 6, (uint16_t)def->columns[i].decimals);
            write_le16(buf + coff + 8, cidx);
            write_le16(buf + coff + 10, hidx);
        }

        write_le16(buf + 1964 + 0, title_idx);
        write_le16(buf + 1964 + 2, group_idx);
        write_le16(buf + 1964 + 4, subgroup_idx);
        write_le16(buf + 1964 + 6, grp_hdr_idx);
        write_le16(buf + 1964 + 8, subgrp_hdr_idx);
    }

    #undef STORE_EXPR

    write_le16(buf + 1964 + 10, (uint16_t)def->page_width);
    write_le16(buf + 1964 + 12, (uint16_t)def->lines_per_page);
    write_le16(buf + 1964 + 14, (uint16_t)def->left_margin);
    write_le16(buf + 1964 + 16, (uint16_t)def->right_margin);
    write_le16(buf + 1964 + 18, (uint16_t)def->num_columns);
    buf[1964 + 20] = def->double_space ? 'Y' : 'N';
    buf[1964 + 21] = def->summary_only ? 'Y' : 'N';
    buf[1964 + 22] = 'N';
    buf[1964 + 23] = (unsigned char)(def->eject_after ? 2 : 0);

    /* End marker */
    write_le16(buf + 1988, 0x0002);

    fp = fopen(filename, "wb");
    if (!fp) return -1;
    fwrite(buf, 1, FRM_FILE_SIZE, fp);
    fclose(fp);
    return 0;
}

/* ---- Report generation ---- */

static void print_margin(FILE *fp, int margin) {
    int i;
    for (i = 0; i < margin; i++)
        fputc(' ', fp);
}

/* Count header lines (split on ';') */
static int count_header_lines(const char *hdr) {
    int lines = 1;
    const char *p = hdr;
    if (!*p) return 0;
    while (*p) {
        if (*p == ';') lines++;
        p++;
    }
    return lines;
}

/* Get Nth header line (0-based) */
static void get_header_line(const char *hdr, int n, char *out, int outsize) {
    const char *p = hdr;
    int cur = 0;
    int i = 0;

    out[0] = '\0';
    while (*p && cur < n) {
        if (*p == ';') cur++;
        p++;
    }
    while (*p && *p != ';' && i < outsize - 1)
        out[i++] = *p++;
    out[i] = '\0';
}

static void print_page_header(frm_def_t *def, int page_num,
                               expr_ctx_t *ectx, const char *heading,
                               FILE *fp, int *lines_used) {
    char datebuf[16];
    int i, max_hdr_lines, line;

    *lines_used = 0;

    /* Page No. line */
    print_margin(fp, def->left_margin);
    fprintf(fp, "Page No. %6d\n", page_num);
    (*lines_used)++;

    /* Date line */
    {
        int y, m, d;
        int32_t today = date_today();
        date_from_jdn(today, &y, &m, &d);
        snprintf(datebuf, sizeof(datebuf), "%02d/%02d/%02d", m, d, y % 100);
    }
    print_margin(fp, def->left_margin);
    fprintf(fp, "%s\n", datebuf);
    (*lines_used)++;

    /* Title */
    if (def->title[0]) {
        value_t v;
        char title_text[256];
        if (ast_eval_dynamic(def->title, ectx, &v) == 0) {
            val_to_string(&v, title_text, sizeof(title_text));
        } else {
            str_copy(title_text, def->title, sizeof(title_text));
        }
        /* Center the title */
        {
            int tlen = strlen(title_text);
            int content_width = def->page_width - def->left_margin - def->right_margin;
            int pad = 0;
            if (tlen < content_width)
                pad = (content_width - tlen) / 2;
            print_margin(fp, def->left_margin + pad);
            fprintf(fp, "%s\n", title_text);
            (*lines_used)++;
        }
    }

    /* Heading override (from HEADING clause) */
    if (heading && heading[0]) {
        int hlen = strlen(heading);
        int content_width = def->page_width - def->left_margin - def->right_margin;
        int pad = 0;
        if (hlen < content_width)
            pad = (content_width - hlen) / 2;
        print_margin(fp, def->left_margin + pad);
        fprintf(fp, "%s\n", heading);
        (*lines_used)++;
    }

    /* Blank line before column headers */
    fprintf(fp, "\n");
    (*lines_used)++;

    /* Column headers */
    max_hdr_lines = 0;
    for (i = 0; i < def->num_columns; i++) {
        int n = count_header_lines(def->columns[i].header);
        if (n > max_hdr_lines) max_hdr_lines = n;
    }
    if (max_hdr_lines == 0) max_hdr_lines = 1;

    for (line = 0; line < max_hdr_lines; line++) {
        print_margin(fp, def->left_margin);
        for (i = 0; i < def->num_columns; i++) {
            char hline[256];
            get_header_line(def->columns[i].header, line, hline, sizeof(hline));
            fprintf(fp, "%-*s", def->columns[i].width, hline);
            if (i < def->num_columns - 1) fputc(' ', fp);
        }
        fprintf(fp, "\n");
        (*lines_used)++;
    }
}

static void print_totals_line(frm_def_t *def, double *totals,
                               const char *label, FILE *fp, int margin) {
    int i;
    print_margin(fp, margin);
    for (i = 0; i < def->num_columns; i++) {
        if (def->columns[i].totals) {
            char numbuf[64];
            snprintf(numbuf, sizeof(numbuf), "%*.*f",
                     def->columns[i].width, def->columns[i].decimals,
                     totals[i]);
            fprintf(fp, "%*s", def->columns[i].width, numbuf);
        } else {
            /* Blank space for non-total columns */
            int j;
            for (j = 0; j < def->columns[i].width; j++)
                fputc(' ', fp);
        }
        if (i < def->num_columns - 1) fputc(' ', fp);
    }
    fprintf(fp, "\n");

    /* Print the label line */
    print_margin(fp, margin);
    fprintf(fp, "%s\n", label);
}

void report_generate(frm_def_t *def, dbf_t *db, expr_ctx_t *ectx,
                     const char *for_cond, const char *heading,
                     int plain, int summary, int noeject,
                     FILE *outfile) {
    uint32_t rec;
    int lines_left;
    int page_number = 1;
    double grand_totals[FRM_MAX_COLUMNS];
    double group_totals[FRM_MAX_COLUMNS];
    double subgroup_totals[FRM_MAX_COLUMNS];
    char prev_group_val[256];
    char prev_subgroup_val[256];
    int first_record = 1;
    int has_groups, has_subgroups, has_totals;
    int i;
    int header_height = 0;

    memset(grand_totals, 0, sizeof(grand_totals));
    memset(group_totals, 0, sizeof(group_totals));
    memset(subgroup_totals, 0, sizeof(subgroup_totals));
    prev_group_val[0] = '\0';
    prev_subgroup_val[0] = '\0';

    has_groups = (def->group_expr[0] != '\0');
    has_subgroups = (def->subgroup_expr[0] != '\0');
    has_totals = 0;
    for (i = 0; i < def->num_columns; i++) {
        if (def->columns[i].totals) { has_totals = 1; break; }
    }

    lines_left = def->lines_per_page;

    for (rec = 1; rec <= db->record_count; rec++) {
        if (dbf_read_record(db, rec) < 0) continue;
        /* Skip deleted records */
        if (db->record_buf[0] == '*') continue;

        /* FOR condition */
        if (for_cond && for_cond[0]) {
            value_t cond;
            if (ast_eval_dynamic(for_cond, ectx, &cond) != 0) continue;
            if (cond.type != VAL_LOGIC || !cond.logic) continue;
        }

        /* Page header */
        if (!plain) {
            if (first_record || lines_left < 4) {
                if (!first_record && !noeject)
                    fputc('\f', outfile);
                {
                    int hlines = 0;
                    print_page_header(def, page_number, ectx, heading,
                                      outfile, &hlines);
                    header_height = hlines;
                    lines_left = def->lines_per_page - hlines;
                    page_number++;
                }
            }
        }

        /* Group processing */
        if (has_groups) {
            value_t gv;
            char cur_group[256];
            cur_group[0] = '\0';

            if (ast_eval_dynamic(def->group_expr, ectx, &gv) == 0)
                val_to_string(&gv, cur_group, sizeof(cur_group));

            if (str_icmp(cur_group, prev_group_val) != 0 && !first_record) {
                /* Group break */
                if (has_subgroups && has_totals) {
                    print_totals_line(def, subgroup_totals,
                                      "* Subtotal *", outfile, def->left_margin);
                    lines_left -= 2;
                    memset(subgroup_totals, 0, sizeof(subgroup_totals));
                }
                if (has_totals) {
                    print_totals_line(def, group_totals,
                                      "** Subtotal **", outfile, def->left_margin);
                    lines_left -= 2;
                    memset(group_totals, 0, sizeof(group_totals));
                }
            }

            if (str_icmp(cur_group, prev_group_val) != 0) {
                /* Print group header */
                if (def->group_header[0]) {
                    print_margin(outfile, def->left_margin);
                    fprintf(outfile, "** %s %s\n", def->group_header, cur_group);
                    lines_left--;
                }
                str_copy(prev_group_val, cur_group, sizeof(prev_group_val));
                prev_subgroup_val[0] = '\0';
            }
        }

        /* Subgroup processing */
        if (has_subgroups) {
            value_t sv;
            char cur_subgroup[256];
            cur_subgroup[0] = '\0';

            if (ast_eval_dynamic(def->subgroup_expr, ectx, &sv) == 0)
                val_to_string(&sv, cur_subgroup, sizeof(cur_subgroup));

            if (str_icmp(cur_subgroup, prev_subgroup_val) != 0 && !first_record) {
                if (has_totals) {
                    print_totals_line(def, subgroup_totals,
                                      "* Subtotal *", outfile, def->left_margin);
                    lines_left -= 2;
                    memset(subgroup_totals, 0, sizeof(subgroup_totals));
                }
            }

            if (str_icmp(cur_subgroup, prev_subgroup_val) != 0) {
                if (def->subgroup_header[0]) {
                    print_margin(outfile, def->left_margin);
                    fprintf(outfile, "* %s %s\n", def->subgroup_header, cur_subgroup);
                    lines_left--;
                }
                str_copy(prev_subgroup_val, cur_subgroup, sizeof(prev_subgroup_val));
            }
        }

        /* Accumulate totals and print detail line */
        if (!summary) {
            print_margin(outfile, def->left_margin);
        }
        for (i = 0; i < def->num_columns; i++) {
            value_t cv;
            char colbuf[256];
            colbuf[0] = '\0';

            if (def->columns[i].content[0] &&
                ast_eval_dynamic(def->columns[i].content, ectx, &cv) == 0) {
                val_to_string(&cv, colbuf, sizeof(colbuf));

                /* Accumulate numeric totals */
                if (cv.type == VAL_NUM && def->columns[i].totals) {
                    grand_totals[i] += cv.num;
                    group_totals[i] += cv.num;
                    subgroup_totals[i] += cv.num;
                }

                if (!summary) {
                    if (cv.type == VAL_NUM) {
                        /* Numeric: format with decimals, right-aligned */
                        char numbuf[64];
                        snprintf(numbuf, sizeof(numbuf), "%*.*f",
                                 def->columns[i].width, def->columns[i].decimals,
                                 cv.num);
                        fprintf(outfile, "%*s", def->columns[i].width, numbuf);
                    } else {
                        /* Character/Date/Logical: left-aligned */
                        trim_right(colbuf);
                        fprintf(outfile, "%-*s", def->columns[i].width, colbuf);
                    }
                }
            } else {
                if (!summary) {
                    int j;
                    for (j = 0; j < def->columns[i].width; j++)
                        fputc(' ', outfile);
                }
            }
            if (!summary && i < def->num_columns - 1)
                fputc(' ', outfile);
        }
        if (!summary) {
            fprintf(outfile, "\n");
            lines_left--;
            if (def->double_space) {
                fprintf(outfile, "\n");
                lines_left--;
            }
        }

        first_record = 0;
    }

    /* Final totals */
    if (has_totals) {
        if (has_subgroups && prev_subgroup_val[0]) {
            print_totals_line(def, subgroup_totals,
                              "* Subtotal *", outfile, def->left_margin);
            lines_left -= 2;
        }
        if (has_groups && prev_group_val[0]) {
            print_totals_line(def, group_totals,
                              "** Subtotal **", outfile, def->left_margin);
            lines_left -= 2;
        }
        print_totals_line(def, grand_totals,
                          "*** Total ***", outfile, def->left_margin);
        lines_left -= 2;
    }

    /* Eject after */
    if (!noeject && def->eject_after)
        fputc('\f', outfile);
}


/* ==== CA-Clipper's REPORT FORM layout ====
 * The rules of CA's FRMRUN.PRG / FRMBACK.PRG (built with S87_COMPAT, as CA
 * shipped them): an empty line before the first page; a page header of
 * "Page No." + STR(n,6) (with a HEADING, centred lines from column 15 with
 * the page number stuffed over the first), the date, the form's title
 * lines centred, an empty line, the column headings (numeric ones right
 * aligned), two empty lines; every line printed as text then a newline,
 * the left margin before it; character columns wrapped to their width;
 * "** Subtotal **" / "* Subsubtotal *" and "*** Total ***" each above a
 * line of totals; a page eject is a form feed to a printer only. */

static int clipper_layout;
void report_set_clipper_layout(int on) { clipper_layout = on; }
int report_clipper_layout(void) { return clipper_layout; }

#define CR_LINE 1024
#define CR_MAXHDR 64

typedef struct {
    frm_def_t *def;
    expr_ctx_t *ectx;
    FILE *fp;
    int plain, formfeeds, summary, spacing;
    int page, lines_left, max_lines_avail;
    char heading[256];
    char title[4][61];
    int ntitle;
    char coltype[FRM_MAX_COLUMNS];
    char colpict[FRM_MAX_COLUMNS][64];
    char colhdr[FRM_MAX_COLUMNS][8][256];
    int ncolhdr[FRM_MAX_COLUMNS];
} crep_t;

static void cr_print(crep_t *r, const char *text) {
    int i;
    if (text) {
        for (i = 0; i < r->def->left_margin; i++) fputc(' ', r->fp);
        fputs(text, r->fp);
    }
    fputc('\n', r->fp);
}

static void cr_eject(crep_t *r) { if (r->formfeeds) fputc('\f', r->fp); }

static int cr_call(crep_t *r, const char *fn, value_t *args, int n, value_t *out) {
    if (func_call(r->ectx, fn, args, n, out) != 0) { r->ectx->error = NULL; *out = val_str(""); return -1; }
    return 0;
}

/* XMLCOUNT / XMEMOLINE: MLCOUNT and MEMOLINE with a tab of 4 and wrapping */
static int cr_mlcount(crep_t *r, const char *s, int width) {
    value_t a[4], out;
    char t[CR_LINE];
    str_copy(t, s, sizeof(t));
    trim_right(t);   /* XMLCOUNT counts the trimmed text */
    a[0] = val_str(t); a[1] = val_num(width); a[2] = val_num(4 >= width ? width - 1 : 4); a[3] = val_logic(1);
    if (cr_call(r, "MLCOUNT", a, 4, &out) != 0 || out.type != VAL_NUM) return 1;
    return (int)out.num;
}

static void cr_memoline(crep_t *r, const char *s, int width, int n, char *out, int size) {
    value_t a[5], v;
    a[0] = val_str(s); a[1] = val_num(width); a[2] = val_num(n); a[3] = val_num(4 >= width ? width - 1 : 4); a[4] = val_logic(1);
    cr_call(r, "MEMOLINE", a, 5, &v);
    str_copy(out, v.type == VAL_CHAR ? val_text(&v) : "", size);
}

static void cr_transform(crep_t *r, const value_t *v, const char *pict, char *out, int size) {
    value_t a[2], res;
    a[0] = *v; a[1] = val_str(pict);
    cr_call(r, "TRANSFORM", a, 2, &res);
    str_copy(out, res.type == VAL_CHAR ? val_text(&res) : "", size);
}

static void pad_right(char *s, int width, int size) {
    int n = (int)strlen(s);
    if (n > width) { s[width] = '\0'; return; }
    while (n < width && n < size - 1) s[n++] = ' ';
    s[n] = '\0';
}

static void ltrim_copy(char *dst, const char *src, int size) {
    while (*src == ' ') src++;
    str_copy(dst, src, size);
}

/* MakeAStr: a group value as text */
static void cr_group_text(const value_t *v, char *out, int size) {
    switch (v->type) {
    case VAL_DATE: val_to_string(v, out, size); break;
    case VAL_LOGIC: str_copy(out, v->logic ? "T" : "F", size); break;
    case VAL_NUM: snprintf(out, (size_t)size, "%10.*f", v->ndec > 0 ? v->ndec : 0, v->num); break;
    case VAL_CHAR: str_copy(out, val_text(v), size); break;
    default: str_copy(out, "INVALID EXPRESSION", size); break;
    }
}

static int cr_eval(crep_t *r, const char *expr, value_t *out) {
    if (!expr[0] || ast_eval_dynamic(expr, r->ectx, out) != 0) { r->ectx->error = NULL; *out = val_nil(); return -1; }
    return 0;
}

/* ParseHeader: up to n items of at most 60 characters, ';' ending an item */
static int cr_parse_header(const char *src, int n, char items[][61]) {
    int k;
    for (k = 0; k < n; k++) {
        char item[61];
        const char *semi;
        int len;
        str_copy(item, src, sizeof(item));
        semi = strchr(item, ';');
        if (semi) {
            len = (int)(semi - item);
            memcpy(items[k], item, (size_t)len);
            items[k][len] = '\0';
            src += len + 1 <= (int)strlen(src) ? len + 1 : (int)strlen(src);
        } else {
            str_copy(items[k], item, 61);
            { int t = (int)strlen(items[k]); while (t > 0 && items[k][t - 1] == ' ') t--; if (t == 0) items[k][0] = '\0'; }
            src += strlen(src) < 60 ? strlen(src) : 60;
        }
    }
    return n;
}

static void cr_header(crep_t *r) {
    static char lines[CR_MAXHDR][CR_LINE];
    int n = 0, i, k, maxcol = 0;
    frm_def_t *def = r->def;
    int head_len = def->page_width - def->left_margin - 30;
    int rpage = def->page_width - def->right_margin;
    if (head_len < 1) head_len = 1;
    if (!r->plain) {
        if (!r->heading[0]) {
            snprintf(lines[n++], CR_LINE, "Page No.%6d", r->page);
        } else {
            char items[16][61];
            int nitems = 1, first = n;
            const char *c;
            for (c = r->heading; *c; c++) if (*c == ';') nitems++;
            if (nitems > 16) nitems = 16;
            cr_parse_header(r->heading, nitems, items);
            for (k = 0; k < nitems; k++) {
                char lt[256];
                int cnt, hl;
                ltrim_copy(lt, items[k], sizeof(lt));
                cnt = cr_mlcount(r, lt, head_len);
                if (cnt < 1) cnt = 1;
                for (hl = 1; hl <= cnt && n < CR_MAXHDR - 1; hl++) {
                    char seg[256];
                    int sl, left;
                    cr_memoline(r, lt, head_len, hl, seg, sizeof(seg));
                    trim_right(seg);
                    sl = (int)strlen(seg);
                    left = sl < head_len ? (head_len - sl) / 2 : 0;
                    snprintf(lines[n++], CR_LINE, "%15s%*s%s%*s", "", left, "", seg,
                             sl < head_len ? head_len - sl - left : 0, "");
                }
            }
            {
                char pg[16];
                snprintf(pg, sizeof(pg), "Page No.%6d", r->page);
                memcpy(lines[first], pg, 14);
            }
        }
        {
            value_t d = val_date(date_today());
            val_to_string(&d, lines[n++], CR_LINE);
        }
    }
    for (k = 0; k < r->ntitle; k++) {
        char lt[256];
        int cnt, hl;
        ltrim_copy(lt, r->title[k], sizeof(lt));
        cnt = cr_mlcount(r, lt, rpage);
        if (cnt < 1) cnt = 1;
        for (hl = 1; hl <= cnt && n < CR_MAXHDR - 1; hl++) {
            char seg[256];
            int pad;
            cr_memoline(r, lt, rpage, hl, seg, sizeof(seg));
            trim_right(seg);
            pad = (rpage - def->left_margin - (int)strlen(seg)) / 2;
            if (pad < 0) pad = 0;
            snprintf(lines[n++], CR_LINE, "%*s%s", pad, "", seg);
        }
    }
    lines[n++][0] = '\0';   /* S87_COMPAT */
    for (i = 0; i < def->num_columns; i++) if (r->ncolhdr[i] > maxcol) maxcol = r->ncolhdr[i];
    for (k = 0; k < maxcol && n < CR_MAXHDR; k++) {
        char *ln = lines[n++];
        ln[0] = '\0';
        for (i = 0; i < def->num_columns; i++) {
            char cell[256];
            int w = def->columns[i].width;
            if (i > 0) strncat(ln, " ", CR_LINE - strlen(ln) - 1);
            if (k < r->ncolhdr[i]) {
                if (r->coltype[i] == 'N') {
                    int hl = (int)strlen(r->colhdr[i][k]);
                    if (hl >= w) { str_copy(cell, r->colhdr[i][k], sizeof(cell)); cell[w] = '\0'; }
                    else snprintf(cell, sizeof(cell), "%*s%s", w - hl, "", r->colhdr[i][k]);
                } else {
                    str_copy(cell, r->colhdr[i][k], sizeof(cell));
                    pad_right(cell, w, sizeof(cell));
                }
            } else {
                cell[0] = '\0';
                pad_right(cell, w, sizeof(cell));
            }
            strncat(ln, cell, CR_LINE - strlen(ln) - 1);
        }
    }
    if (n < CR_MAXHDR) lines[n++][0] = '\0';
    if (n < CR_MAXHDR) lines[n++][0] = '\0';
    for (i = 0; i < n; i++) cr_print(r, lines[i]);
    r->page++;
    r->lines_left = def->lines_per_page - n;
    r->max_lines_avail = def->lines_per_page - n;
}

static void cr_new_page(crep_t *r) {
    cr_eject(r);
    if (r->plain) r->lines_left = 1000;
    else cr_header(r);
}

/* the totals line: each column's total through its picture, or spaces */
static void cr_totals_line(crep_t *r, const double *totals, char *out, int size) {
    int i;
    out[0] = '\0';
    for (i = 0; i < r->def->num_columns; i++) {
        char cell[256];
        if (i > 0) strncat(out, " ", (size_t)(size - (int)strlen(out) - 1));
        if (r->def->columns[i].totals) {
            value_t v = val_num(totals[i]);
            cr_transform(r, &v, r->colpict[i], cell, sizeof(cell));
        } else {
            cell[0] = '\0';
            pad_right(cell, r->def->columns[i].width, sizeof(cell));
        }
        strncat(out, cell, (size_t)(size - (int)strlen(out) - 1));
    }
}

void report_generate_clipper(frm_def_t *def, dbf_t *db, expr_ctx_t *ectx,
                             const char *for_cond, const char *heading,
                             int plain, int summary, int noeject, int to_printer,
                             FILE *outfile) {
    crep_t *r = calloc(1, sizeof(crep_t));
    double totals[3][FRM_MAX_COLUMNS];
    char group_val[2][256];
    const char *group_expr[2];
    const char *group_hdr[2];
    int ngroups = 0, any_totals = 0, first_pass = 1, i, g;
    uint32_t rec;
    char line[CR_LINE];
    if (!r) return;
    memset(totals, 0, sizeof(totals));
    r->def = def; r->ectx = ectx; r->fp = outfile;
    r->formfeeds = to_printer;
    r->plain = def->frm_plain;
    r->summary = def->summary_only || summary;
    r->spacing = def->double_space ? 2 : 1;
    if (plain) { r->plain = 1; r->formfeeds = 0; heading = NULL; }
    str_copy(r->heading, heading ? heading : "", sizeof(r->heading));
    {
        /* the title: four items, trailing empty ones dropped */
        char items[4][61];
        int k = 4;
        cr_parse_header(def->title, 4, items);
        while (k > 0 && !items[k - 1][0]) k--;
        for (i = 0; i < k; i++) str_copy(r->title[i], items[i], 61);
        r->ntitle = k;
    }
    if (def->group_expr[0]) { group_expr[ngroups] = def->group_expr; group_hdr[ngroups] = def->group_header; ngroups++; }
    if (def->subgroup_expr[0]) { group_expr[ngroups] = def->subgroup_expr; group_hdr[ngroups] = def->subgroup_header; ngroups++; }
    for (g = 0; g < 2; g++) group_val[g][0] = '\0';
    /* each column's type (from its value at the current record), picture
     * and heading lines */
    for (i = 0; i < def->num_columns; i++) {
        frm_column_t *c = &def->columns[i];
        value_t v;
        const char *h = c->header;
        int k = 0;
        cr_eval(r, c->content, &v);
        r->coltype[i] = v.type == VAL_NUM ? 'N' : v.type == VAL_DATE ? 'D' : v.type == VAL_LOGIC ? 'L' : 'C';
        switch (r->coltype[i]) {
        case 'N':
            if (c->decimals > 0 && c->width - c->decimals - 1 > 0) {
                int a = c->width - c->decimals - 1, j, p = 0;
                for (j = 0; j < a && p < 60; j++) r->colpict[i][p++] = '9';
                r->colpict[i][p++] = '.';
                for (j = 0; j < c->decimals && p < 62; j++) r->colpict[i][p++] = '9';
                r->colpict[i][p] = '\0';
            } else {
                int j;
                for (j = 0; j < c->width && j < 63; j++) r->colpict[i][j] = '9';
                r->colpict[i][j] = '\0';
            }
            break;
        case 'D': str_copy(r->colpict[i], "@D", 64); break;
        case 'L': { int j; r->colpict[i][0] = '@'; r->colpict[i][1] = 'L'; for (j = 0; j < c->width - 1 && j < 60; j++) r->colpict[i][2 + j] = 'X'; r->colpict[i][2 + j] = '\0'; } break;
        default: { int j; for (j = 0; j < c->width && j < 63; j++) r->colpict[i][j] = 'X'; r->colpict[i][j] = '\0'; } break;
        }
        if (c->totals) any_totals = 1;
        /* ListAsArray(header, ";"): a trailing ';' adds an empty item */
        while (*h && k < 8) {
            const char *semi = strchr(h, ';');
            int len = semi ? (int)(semi - h) : (int)strlen(h);
            if (len > 255) len = 255;
            memcpy(r->colhdr[i][k], h, (size_t)len);
            r->colhdr[i][k][len] = '\0';
            k++;
            if (!semi) { h += strlen(h); break; }
            h = semi + 1;
            if (!*h && k < 8) { r->colhdr[i][k++][0] = '\0'; }
        }
        r->ncolhdr[i] = k;
    }
    r->page = 1;
    r->lines_left = def->lines_per_page;
    r->max_lines_avail = def->lines_per_page;
    cr_print(r, NULL);                       /* S87_COMPAT: an extra line on the first page */
    if (!def->no_eject_before && !noeject) cr_eject(r);
    cr_header(r);   /* the first header always, plain or not, and its line budget (FRMRUN's) */

    for (rec = 1; rec <= db->record_count; rec++) {
        char hdr_lines[8][CR_LINE];
        int nhdr = 0, eject_grp = 0;
        if (dbf_read_record(db, rec) < 0) continue;
        if (db->record_buf[0] == '*') continue;
        if (for_cond && for_cond[0]) {
            value_t cond;
            if (ast_eval_dynamic(for_cond, ectx, &cond) != 0) { ectx->error = NULL; continue; }
            if (cond.type != VAL_LOGIC || !cond.logic) continue;
        }
        for (i = 0; i < def->num_columns; i++) {
            if (def->columns[i].totals) {
                value_t v;
                if (cr_eval(r, def->columns[i].content, &v) == 0 && v.type == VAL_NUM) totals[0][i] += v.num;
            }
        }
        /* subtotals of the groups that changed, innermost first */
        if (!first_pass && any_totals) {
            for (g = ngroups - 1; g >= 0; g--) {
                value_t gv;
                char now[256];
                cr_eval(r, group_expr[g], &gv);
                cr_group_text(&gv, now, sizeof(now));
                if (strcmp(now, group_val[g]) != 0 && nhdr < 7) {
                    str_copy(hdr_lines[nhdr++], g == 0 ? "** Subtotal **" : "* Subsubtotal *", CR_LINE);
                    cr_totals_line(r, totals[g + 1], hdr_lines[nhdr++], CR_LINE);
                    for (i = 0; i < def->num_columns; i++) if (def->columns[i].totals) totals[g + 1][i] = 0;
                    if (g == 0) eject_grp = def->group_eject;
                }
            }
        }
        first_pass = 0;
        if (nhdr > 0 && eject_grp) {
            if (nhdr > r->lines_left) cr_new_page(r);
            for (i = 0; i < nhdr; i++) cr_print(r, hdr_lines[i]);
            nhdr = 0;
            cr_new_page(r);
        }
        for (g = 0; g < ngroups; g++) {
            value_t gv;
            char now[256];
            cr_eval(r, group_expr[g], &gv);
            cr_group_text(&gv, now, sizeof(now));
            if (strcmp(now, group_val[g]) != 0 && nhdr < 7) {
                hdr_lines[nhdr++][0] = '\0';
                snprintf(hdr_lines[nhdr++], CR_LINE, "%s%s %s", g == 0 ? "** " : "* ", group_hdr[g], now);
            }
        }
        if (nhdr > 0) {
            if (nhdr > r->lines_left) cr_new_page(r);
            for (i = 0; i < nhdr; i++) cr_print(r, hdr_lines[i]);
            r->lines_left -= nhdr;
            if (r->lines_left == 0) cr_new_page(r);
        }
        for (i = 0; i < def->num_columns; i++) {
            if (def->columns[i].totals) {
                value_t v;
                if (cr_eval(r, def->columns[i].content, &v) == 0 && v.type == VAL_NUM)
                    for (g = 0; g < ngroups; g++) totals[g + 1][i] += v.num;
            }
        }
        for (g = 0; g < ngroups; g++) {
            value_t gv;
            cr_eval(r, group_expr[g], &gv);
            cr_group_text(&gv, group_val[g], sizeof(group_val[g]));
        }
        if (!r->summary) {
            /* the record's lines: character columns wrapped to their width */
            static char rec_lines[64][CR_LINE];
            int nlines = 1, ln;
            value_t vals[FRM_MAX_COLUMNS];
            for (i = 0; i < def->num_columns; i++) {
                cr_eval(r, def->columns[i].content, &vals[i]);
                if (r->coltype[i] == 'C' && vals[i].type == VAL_CHAR) {
                    int cnt = cr_mlcount(r, val_text(&vals[i]), def->columns[i].width);
                    if (cnt > nlines) nlines = cnt;
                }
            }
            if (nlines > 64) nlines = 64;
            for (ln = 0; ln < nlines; ln++) rec_lines[ln][0] = '\0';
            for (i = 0; i < def->num_columns; i++) {
                int w = def->columns[i].width;
                for (ln = 0; ln < nlines; ln++) {
                    char cell[CR_LINE];
                    if (r->coltype[i] == 'C') {
                        char tr[CR_LINE];
                        str_copy(tr, vals[i].type == VAL_CHAR ? val_text(&vals[i]) : "", sizeof(tr));
                        trim_right(tr);
                        cr_memoline(r, tr, w, ln + 1, cell, sizeof(cell));
                    } else if (ln == 0) {
                        cr_transform(r, &vals[i], r->colpict[i], cell, sizeof(cell));
                    } else {
                        cell[0] = '\0';
                    }
                    pad_right(cell, w, sizeof(cell));
                    if (i > 0) strncat(rec_lines[ln], " ", CR_LINE - strlen(rec_lines[ln]) - 1);
                    strncat(rec_lines[ln], cell, CR_LINE - strlen(rec_lines[ln]) - 1);
                }
            }
            if (nlines > r->lines_left) {
                if (nlines > r->max_lines_avail) {
                    for (ln = 0; ln < nlines - 1; ln++) {
                        cr_print(r, rec_lines[ln]);
                        if (--r->lines_left == 0) cr_new_page(r);
                    }
                } else {
                    cr_new_page(r);
                    for (ln = 0; ln < nlines; ln++) cr_print(r, rec_lines[ln]);
                    r->lines_left -= nlines;
                }
            } else {
                for (ln = 0; ln < nlines; ln++) cr_print(r, rec_lines[ln]);
                r->lines_left -= nlines;
            }
            if (r->lines_left == 0) cr_new_page(r);
            if (r->spacing > 1 && r->lines_left > r->spacing - 1) {
                for (ln = 2; ln <= r->spacing; ln++) { cr_print(r, NULL); r->lines_left--; }
            }
        }
    }
    /* the last subtotals, innermost first, then the grand total */
    if (any_totals) {
        for (g = ngroups - 1; g >= 0; g--) {
            if (r->lines_left < 2) cr_new_page(r);
            cr_print(r, g == 0 ? "** Subtotal **" : "* Subsubtotal *");
            cr_totals_line(r, totals[g + 1], line, sizeof(line));
            cr_print(r, line);
        }
        if (r->lines_left < 2) cr_new_page(r);
        cr_print(r, "*** Total ***");
        cr_totals_line(r, totals[0], line, sizeof(line));
        cr_print(r, line);
    }
    if (def->eject_after && !noeject) cr_eject(r);
    free(r);
}
