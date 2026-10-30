#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "ast.h"
#include "report.h"
#include "date.h"
#include "util.h"

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
