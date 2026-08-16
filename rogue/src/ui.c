/* Screen handling: the term.h full-screen game and the --line fallback. */

#include "rogue.h"

#include <stdio.h>
#include <string.h>
#include <term.h>

static char item_glyph(int type) {
    switch (type) {
    case IT_GOLD:   return '*';
    case IT_FOOD:   return ':';
    case IT_POTION: return '!';
    case IT_SCROLL: return '?';
    case IT_WEAPON: return ')';
    case IT_ARMOR:  return ']';
    case IT_AMULET: return ',';
    }
    return '?';
}

static char glyph_at(game_t *g, int r, int c) {
    int i;
    if (!(g->flags[r][c] & F_SEEN)) {
        return ' ';
    }
    if (r == g->pr && c == g->pc) {
        return '@';
    }
    for (i = 0; i < MAX_MONS; i++) {
        mon_t *m = &g->mons[i];
        if (m->kind >= 0 && m->r == r && m->c == c && mon_visible(g, m)) {
            return mon_table[m->kind].ch;
        }
    }
    for (i = 0; i < MAX_ITEMS; i++) {
        item_t *it = &g->items[i];
        if (it->type != IT_NONE && it->r == r && it->c == c) {
            return item_glyph(it->type);
        }
    }
    return g->map[r][c];
}

static void status_line(game_t *g, char *buf, int cap) {
    snprintf(buf, cap,
             "Level: %d  Gold: %d  Hp: %d(%d)  Str: %d(%d)  Arm: %d  Exp: %d/%d  %s",
             g->depth, g->gold, g->hp, g->maxhp, g->str, g->maxstr,
             player_armor(g), g->level, g->exp, rank_name(g->level));
}

/* ---- line mode ---------------------------------------------------- */

void line_render(game_t *g, FILE *out) {
    int r, c;
    char status[120];
    for (r = 0; r < MAP_ROWS; r++) {
        char row[MAP_COLS + 1];
        for (c = 0; c < MAP_COLS; c++) {
            row[c] = glyph_at(g, r, c);
        }
        row[MAP_COLS] = '\0';
        /* Trim trailing blanks to keep transcripts small. */
        for (c = MAP_COLS; c > 0 && row[c - 1] == ' '; c--) {
            row[c - 1] = '\0';
        }
        fprintf(out, "%s\n", row);
    }
    status_line(g, status, (int)sizeof(status));
    fprintf(out, "%s\n", status);
}

/* ---- full-screen mode --------------------------------------------- */

static void draw(game_t *g) {
    int r, c;
    char status[120];
    term_begin_update();
    term_gotoxy(1, 1);
    term_clear(1);
    term_puts(g->msg);
    for (r = 0; r < MAP_ROWS; r++) {
        term_gotoxy(r + 2, 1);
        for (c = 0; c < MAP_COLS; c++) {
            term_putc(glyph_at(g, r, c));
        }
    }
    status_line(g, status, (int)sizeof(status));
    term_gotoxy(24, 1);
    term_clear(1);
    term_puts(status);
    term_gotoxy(g->pr + 2, g->pc + 1);
    term_end_update();
}

static int read_key(void) {
    int ch = term_getkey();
    if (ch != 27) {
        return ch;
    }
    if (!term_kbhit() || term_getkey() != '[') {
        return 27;
    }
    switch (term_getkey()) {
    case 'A': return 'k';
    case 'B': return 'j';
    case 'C': return 'l';
    case 'D': return 'h';
    }
    return 27;
}

int ui_pick_slot(game_t *g, int want_type, const char *verb) {
    int ch;
    char prompt[64];
    snprintf(prompt, sizeof(prompt), "%s what? [a-z, ESC to cancel]", verb);
    term_gotoxy(1, 1);
    term_clear(1);
    term_puts(prompt);
    ch = term_getkey();
    if (ch < 'a' || ch > 'z') {
        return -1;
    }
    if (g->inv[ch - 'a'].type != want_type) {
        msgf(g, "You cannot %s that.", verb);
        return -1;
    }
    return ch - 'a';
}

void ui_show_inventory(game_t *g) {
    int i, row = 1, n = 0;
    char name[64];
    term_save_screen();
    term_clear(0);
    term_gotoxy(row++, 1);
    term_puts("You are carrying:");
    for (i = 0; i < INV_SLOTS; i++) {
        char line[96];
        if (g->inv[i].type == IT_NONE) {
            continue;
        }
        inv_name(g, &g->inv[i], name, sizeof(name));
        snprintf(line, sizeof(line), "  %c) %s%s%s", 'a' + i, name,
                 i == g->wielding ? " (weapon in hand)" : "",
                 i == g->wearing ? " (being worn)" : "");
        term_gotoxy(row++, 1);
        term_puts(line);
        n++;
    }
    if (g->has_amulet) {
        term_gotoxy(row++, 1);
        term_puts("     the Amulet of Yendor");
        n++;
    }
    if (n == 0) {
        term_gotoxy(row++, 1);
        term_puts("  nothing at all");
    }
    term_gotoxy(row + 1, 1);
    term_puts("--press any key--");
    term_getkey();
    term_restore_screen();
}

static void help_screen(void) {
    static const char *lines[] = {
        "Commands:",
        "  h j k l    move left/down/up/right (arrows work too)",
        "  y u b n    move diagonally",
        "  >  <       descend / ascend a staircase (%)",
        "  i          inventory        e   eat food",
        "  q          quaff a potion   r   read a scroll",
        "  w          wield a weapon   W   wear armor    T  take off",
        "  . or s     rest a turn",
        "  S          save and exit    Q   quit",
        "",
        "Fetch the Amulet of Yendor from level 26 and bring it back up.",
        NULL
    };
    int i;
    term_save_screen();
    term_clear(0);
    for (i = 0; lines[i]; i++) {
        term_gotoxy(i + 1, 1);
        term_puts(lines[i]);
    }
    term_gotoxy(i + 2, 1);
    term_puts("--press any key--");
    term_getkey();
    term_restore_screen();
}

void ui_play(game_t *g) {
    msgf(g, "Hello. Welcome to the Dungeons of Doom. (? for help)");
    for (;;) {
        int ch;
        draw(g);
        ch = read_key();
        if (ch < 0) {
            g->dead = 3;
            break;
        }
        if (ch == '?') {
            help_screen();
            continue;
        }
        if (!do_command(g, ch)) {
            break;
        }
    }
    draw(g);
    if (g->dead != 3 || g->msg[0]) {
        term_gotoxy(1, 1);
        term_clear(1);
        term_puts(g->msg);
        term_gotoxy(24, 1);
        term_puts("--press any key--");
        term_getkey();
    }
}
