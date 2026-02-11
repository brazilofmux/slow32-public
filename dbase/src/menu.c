#include <stdio.h>
#include <string.h>
#include "menu.h"
#include "screen.h"
#include "command.h"
#include "expr.h"
#include "util.h"

#ifdef __slow32__
#include <term.h>
#define HAS_TERM 1
#else
#define HAS_TERM 0
#endif

/* Key codes — same as screen.c / browse.c */
#define DBASE_KEY_UP     5
#define DBASE_KEY_DOWN  24
#define DBASE_KEY_HOME   1
#define DBASE_KEY_END    6

/* ---- Registry ---- */
static popup_t popups[MAX_POPUPS];
static int last_bar_num;
static char last_prompt_text[80];
static int deactivate_flag;

void menu_init(void) {
    int i;
    for (i = 0; i < MAX_POPUPS; i++)
        popups[i].defined = 0;
    last_bar_num = 0;
    last_prompt_text[0] = '\0';
    deactivate_flag = 0;
}

static popup_t *find_popup(const char *name) {
    char upper[32];
    int i;
    str_copy(upper, name, sizeof(upper));
    str_upper(upper);
    for (i = 0; i < MAX_POPUPS; i++) {
        if (popups[i].defined && str_icmp(popups[i].name, upper) == 0)
            return &popups[i];
    }
    return NULL;
}

static popup_t *alloc_popup(void) {
    int i;
    for (i = 0; i < MAX_POPUPS; i++) {
        if (!popups[i].defined)
            return &popups[i];
    }
    return NULL;
}

popup_t *menu_define_popup(const char *name, int r1, int c1, int r2, int c2) {
    popup_t *p = find_popup(name);
    if (!p) {
        p = alloc_popup();
        if (!p) {
            printf("Too many popups defined.\n");
            return NULL;
        }
    }
    str_copy(p->name, name, sizeof(p->name));
    str_upper(p->name);
    p->from_row = r1;
    p->from_col = c1;
    p->to_row = r2;
    p->to_col = c2;
    p->defined = 1;
    p->nbar = 0;
    return p;
}

int menu_define_bar(const char *popup_name, int bar_num, const char *prompt,
                    const char *message, const char *skip_expr) {
    popup_t *p = find_popup(popup_name);
    if (!p) {
        printf("Popup not defined: %s\n", popup_name);
        return -1;
    }
    if (p->nbar >= MAX_BARS) {
        printf("Too many bars in popup %s.\n", p->name);
        return -1;
    }
    {
        bar_entry_t *b = &p->bars[p->nbar];
        b->number = bar_num;
        str_copy(b->prompt, prompt, sizeof(b->prompt));
        str_copy(b->message, message ? message : "", sizeof(b->message));
        b->skip = 0;
        str_copy(b->skip_expr, skip_expr ? skip_expr : "", sizeof(b->skip_expr));
        p->nbar++;
    }
    return 0;
}

/* Check if a bar should be skipped */
static int bar_is_skipped(const bar_entry_t *b) {
    if (b->skip) return 1;
    if (b->skip_expr[0]) {
        expr_ctx_t *ctx = cmd_get_expr_ctx();
        value_t result;
        if (expr_eval_str(ctx, b->skip_expr, &result) == 0 &&
            result.type == VAL_LOGIC && result.logic)
            return 1;
    }
    return 0;
}

/* Find first non-skipped bar index. Returns -1 if all skipped. */
static int first_selectable(const popup_t *p) {
    int i;
    for (i = 0; i < p->nbar; i++) {
        if (!bar_is_skipped(&p->bars[i]))
            return i;
    }
    return -1;
}

/* Navigate to next/previous non-skipped bar */
static int next_selectable(const popup_t *p, int cur, int dir, int wrap) {
    int i, n = p->nbar;
    for (i = 1; i <= n; i++) {
        int idx;
        if (dir > 0) {
            idx = (cur + i) % n;
            if (!wrap && cur + i >= n) return cur;
        } else {
            idx = (cur - i + n) % n;
            if (!wrap && cur - i < 0) return cur;
        }
        if (!bar_is_skipped(&p->bars[idx]))
            return idx;
    }
    return cur;
}

/* Find last non-skipped bar index */
static int last_selectable(const popup_t *p) {
    int i;
    for (i = p->nbar - 1; i >= 0; i--) {
        if (!bar_is_skipped(&p->bars[i]))
            return i;
    }
    return -1;
}

int menu_activate_popup(const char *name) {
    popup_t *p = find_popup(name);
    int cur, i;
    int msg_row, wrap;
    int r1, c1, r2, c2;
    int widest, inner_w;

    if (!p) {
        printf("Popup not defined: %s\n", name);
        return 0;
    }
    if (p->nbar == 0) {
        printf("Popup %s has no bars.\n", p->name);
        return 0;
    }

    cur = first_selectable(p);
    if (cur < 0) return 0;  /* all bars skipped */

    deactivate_flag = 0;
    msg_row = cmd_get_message_row();
    wrap = cmd_get_wrap();

    /* Compute bounds */
    r1 = p->from_row;
    c1 = p->from_col;
    widest = 0;
    for (i = 0; i < p->nbar; i++) {
        int len = (int)strlen(p->bars[i].prompt);
        if (len > widest) widest = len;
    }
    inner_w = widest + 2;  /* 1 space padding each side */

    if (p->to_row > 0 && p->to_col > 0) {
        r2 = p->to_row;
        c2 = p->to_col;
    } else {
        r2 = r1 + p->nbar + 1;  /* border top + nbar rows + border bottom */
        c2 = c1 + inner_w + 1;  /* border left + inner + border right */
    }

#if HAS_TERM
    if (screen_term_available()) {
        int key;

        /* Draw border */
        screen_box(r1, c1, r2, c2, 0);
        fflush(stdout);
        term_set_raw(1);

        for (;;) {
            /* Redraw all bars */
            for (i = 0; i < p->nbar; i++) {
                int row = r1 + 1 + i;
                int j, plen;
                term_gotoxy(row, c1 + 1);
                if (i == cur)
                    term_set_attr(7);  /* reverse */
                else
                    term_set_attr(0);  /* normal */
                /* Space padding + prompt + space padding */
                term_putc(' ');
                term_puts(p->bars[i].prompt);
                plen = (int)strlen(p->bars[i].prompt);
                for (j = plen + 1; j < c2 - c1; j++)
                    term_putc(' ');
            }

            /* Show MESSAGE for current bar */
            if (msg_row >= 0) {
                char blank[81];
                int len = 80;
                if (len > (int)sizeof(blank) - 1) len = (int)sizeof(blank) - 1;
                memset(blank, ' ', len);
                blank[len] = '\0';
                term_gotoxy(msg_row, 0);
                term_set_attr(0);
                term_puts(blank);
                if (p->bars[cur].message[0]) {
                    int mlen = (int)strlen(p->bars[cur].message);
                    int mcol = (80 - mlen) / 2;
                    if (mcol < 0) mcol = 0;
                    term_gotoxy(msg_row, mcol);
                    term_puts(p->bars[cur].message);
                }
            }

            key = read_dbase_key();
            if (screen_check_key_handler(key)) continue;

            if (deactivate_flag) {
                cur = -1;
                break;
            }

            if (key == '\r' || key == '\n') {
                screen_set_lastkey(13);
                break;
            }
            if (key == 27) {  /* Escape */
                screen_set_lastkey(27);
                cur = -1;
                break;
            }
            if (key == DBASE_KEY_UP) {
                cur = next_selectable(p, cur, -1, wrap);
            } else if (key == DBASE_KEY_DOWN) {
                cur = next_selectable(p, cur, 1, wrap);
            } else if (key == DBASE_KEY_HOME) {
                int f = first_selectable(p);
                if (f >= 0) cur = f;
            } else if (key == DBASE_KEY_END) {
                int l = last_selectable(p);
                if (l >= 0) cur = l;
            }
        }

        /* Restore display */
        term_set_attr(0);

        /* Clear message row */
        if (msg_row >= 0) {
            char blank[81];
            int len = 80;
            if (len > (int)sizeof(blank) - 1) len = (int)sizeof(blank) - 1;
            memset(blank, ' ', len);
            blank[len] = '\0';
            term_gotoxy(msg_row, 0);
            term_puts(blank);
        }

        term_set_raw(0);

        /* Clear popup region */
        screen_clear_region(r1, c1, r2, c2);
    } else
#endif
    {
        /* Non-terminal fallback: print bars, pick first non-skipped */
        printf("Popup %s:\n", p->name);
        for (i = 0; i < p->nbar; i++) {
            if (bar_is_skipped(&p->bars[i]))
                printf("  %d. %s (skipped)\n", p->bars[i].number, p->bars[i].prompt);
            else
                printf("  %d. %s\n", p->bars[i].number, p->bars[i].prompt);
        }
    }

    /* Store result */
    if (cur >= 0) {
        last_bar_num = p->bars[cur].number;
        str_copy(last_prompt_text, p->bars[cur].prompt, sizeof(last_prompt_text));
    } else {
        last_bar_num = 0;
        last_prompt_text[0] = '\0';
    }

    return last_bar_num;
}

void menu_deactivate_popup(void) {
    deactivate_flag = 1;
}

void menu_release_popup(const char *name) {
    popup_t *p = find_popup(name);
    if (p) {
        p->defined = 0;
    } else {
        printf("Popup not defined: %s\n", name);
    }
}

void menu_release_all(void) {
    int i;
    for (i = 0; i < MAX_POPUPS; i++)
        popups[i].defined = 0;
}

int menu_last_bar(void) {
    return last_bar_num;
}

const char *menu_last_prompt(void) {
    return last_prompt_text;
}
