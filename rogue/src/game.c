/* The game proper: monsters, items, combat, hunger, and the command
 * dispatcher shared by the term UI and line mode. */

#include "rogue.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

const mon_kind_t mon_table[] = {
    /* name          ch  min max hd ac dn dd  exp  mean */
    { "bat",         'B', 1,  8, 1, 6, 1, 2,   1, 0 },
    { "emu",         'E', 1,  7, 1, 3, 1, 2,   2, 1 },
    { "kestrel",     'K', 1,  6, 1, 3, 1, 4,   1, 1 },
    { "snake",       'S', 1,  9, 1, 5, 1, 3,   2, 1 },
    { "hobgoblin",   'H', 1,  9, 1, 5, 1, 8,   3, 1 },
    { "rattlesnake", 'R', 3, 12, 2, 7, 1, 6,   9, 1 },
    { "orc",         'O', 4, 13, 1, 4, 1, 8,   5, 1 },
    { "zombie",      'Z', 7, 14, 2, 2, 1, 8,   7, 1 },
    { "centaur",     'C', 7, 16, 4, 6, 1, 6,  17, 0 },
    { "quagga",      'Q', 8, 17, 3, 7, 1, 5,  15, 1 },
    { "troll",       'T', 13, 22, 6, 6, 4, 4, 120, 1 },
    { "wraith",      'W', 14, 23, 5, 6, 1, 6,  55, 0 },
    { "griffin",     'G', 20, 26, 13, 8, 4, 3, 2000, 1 },
    { "dragon",      'D', 22, 26, 10, 9, 3, 10, 5000, 0 },
    { "jabberwock",  'J', 24, 26, 15, 4, 2, 12, 3000, 0 },
};
const int mon_table_len = (int)(sizeof(mon_table) / sizeof(mon_table[0]));

static const char *potion_names[NUM_POTIONS] = {
    "healing", "extra healing", "gain strength", "poison"
};
static const char *scroll_names[NUM_SCROLLS] = {
    "magic mapping", "teleportation", "enchant weapon", "enchant armor"
};
static const struct { const char *name; int n, d; } weapon_table[NUM_WEAPONS] = {
    { "dagger", 1, 6 }, { "mace", 2, 4 },
    { "long sword", 3, 4 }, { "two-handed sword", 4, 4 }
};
static const struct { const char *name; int prot; } armor_table[NUM_ARMORS] = {
    { "leather armor", 2 }, { "ring mail", 3 },
    { "chain mail", 5 }, { "plate mail", 7 }
};

static const int level_exp[] = {
    0, 10, 20, 40, 80, 160, 320, 640, 1300, 2600,
    5200, 13000, 26000, 50000, 100000, 200000
};

unsigned rnd(game_t *g, int n) {
    unsigned x = g->rng;
    x ^= x << 13;
    x ^= x >> 17;
    x ^= x << 5;
    g->rng = x;
    if (n <= 0) {
        return 0;
    }
    return (x >> 8) % (unsigned)n;
}

int roll(game_t *g, int n, int d) {
    int sum = 0, i;
    for (i = 0; i < n; i++) {
        sum += (int)rnd(g, d) + 1;
    }
    return sum;
}

void msgf(game_t *g, const char *fmt, ...) {
    va_list ap;
    char buf[100];
    va_start(ap, fmt);
    vsnprintf(buf, sizeof(buf), fmt, ap);
    va_end(ap);
    if (g->msg[0]) {
        strncpy(g->prev_msg, g->msg, sizeof(g->prev_msg) - 1);
        g->prev_msg[sizeof(g->prev_msg) - 1] = '\0';
    }
    strncpy(g->msg, buf, sizeof(g->msg) - 1);
    g->msg[sizeof(g->msg) - 1] = '\0';
    if (g->line_mode) {
        printf("%s\n", g->msg);
    }
}

const char *rank_name(int level) {
    static const char *ranks[] = {
        "Rookie", "Apprentice", "Journeyman", "Adventurer", "Fighter",
        "Warrior", "Rogue", "Champion", "Master Rogue", "Warlord",
        "Hero", "Guild Master", "Dragonlord", "Wizard", "Rogue Geek",
        "Rogue Addict"
    };
    if (level < 1) {
        level = 1;
    }
    if (level > 16) {
        level = 16;
    }
    return ranks[level - 1];
}

void inv_name(game_t *g, inv_t *it, char *buf, int cap) {
    (void)g;
    switch (it->type) {
    case IT_FOOD:
        if (it->count > 1) {
            snprintf(buf, cap, "%d rations of food", it->count);
        } else {
            snprintf(buf, cap, "some food");
        }
        break;
    case IT_POTION:
        snprintf(buf, cap, "a potion of %s", potion_names[it->sub]);
        break;
    case IT_SCROLL:
        snprintf(buf, cap, "a scroll of %s", scroll_names[it->sub]);
        break;
    case IT_WEAPON:
        snprintf(buf, cap, "a %+d %s", it->enchant, weapon_table[it->sub].name);
        break;
    case IT_ARMOR:
        snprintf(buf, cap, "%+d %s", it->enchant, armor_table[it->sub].name);
        break;
    case IT_AMULET:
        snprintf(buf, cap, "the Amulet of Yendor");
        break;
    default:
        snprintf(buf, cap, "something odd");
        break;
    }
}

void init_game(game_t *g, unsigned seed) {
    int i;
    memset(g, 0, sizeof(*g));
    g->rng = seed ? seed : 0x5375721u;
    g->hp = g->maxhp = 12;
    g->str = g->maxstr = 16;
    g->level = 1;
    g->depth = 1;
    g->max_depth = 1;
    g->food = 1300;
    g->wielding = -1;
    g->wearing = -1;
    for (i = 0; i < INV_SLOTS; i++) {
        g->inv[i].type = IT_NONE;
    }
    /* Starting kit: food, a mace, ring mail. */
    g->inv[0].type = IT_FOOD;
    g->inv[0].count = 1;
    g->inv[1].type = IT_WEAPON;
    g->inv[1].sub = W_MACE;
    g->inv[1].count = 1;
    g->inv[1].enchant = 1;
    g->inv[2].type = IT_ARMOR;
    g->inv[2].sub = A_RING;
    g->inv[2].count = 1;
    g->inv[2].enchant = 0;
    g->wielding = 1;
    g->wearing = 2;
    new_level(g);
}

/* ---- combat ------------------------------------------------------ */

int player_armor(game_t *g) {
    if (g->wearing >= 0) {
        return armor_table[g->inv[g->wearing].sub].prot +
               g->inv[g->wearing].enchant;
    }
    return 1;
}

static int player_ac(game_t *g) {
    int ac = 1;
    if (g->wearing >= 0) {
        ac = armor_table[g->inv[g->wearing].sub].prot +
             g->inv[g->wearing].enchant;
    }
    return ac;
}

static int str_bonus(int str) {
    if (str >= 21) return 3;
    if (str >= 19) return 2;
    if (str >= 17) return 1;
    if (str <= 6)  return -1;
    return 0;
}

static void check_level_up(game_t *g) {
    int max = (int)(sizeof(level_exp) / sizeof(level_exp[0]));
    while (g->level < max && g->exp >= level_exp[g->level]) {
        g->level++;
        g->maxhp += roll(g, 1, 10);
        g->hp = g->maxhp;
        msgf(g, "Welcome to level %d.", g->level);
    }
}

static void player_dies(game_t *g, const char *by) {
    g->dead = 1;
    strncpy(g->death_by, by, sizeof(g->death_by) - 1);
    g->death_by[sizeof(g->death_by) - 1] = '\0';
    msgf(g, "You die... killed by %s.", by);
}

static void attack_monster(game_t *g, mon_t *m) {
    const mon_kind_t *k = &mon_table[m->kind];
    int need = 10 + k->ac - g->level - str_bonus(g->str);
    m->asleep = 0;
    if ((int)rnd(g, 20) + 1 >= need) {
        int dmg;
        if (g->wielding >= 0) {
            inv_t *w = &g->inv[g->wielding];
            dmg = roll(g, weapon_table[w->sub].n, weapon_table[w->sub].d) +
                  w->enchant;
        } else {
            dmg = roll(g, 1, 4);
        }
        dmg += str_bonus(g->str);
        if (dmg < 1) {
            dmg = 1;
        }
        m->hp -= dmg;
        if (m->hp <= 0) {
            msgf(g, "You have defeated the %s.", k->name);
            g->exp += k->exp;
            m->kind = -1;
            check_level_up(g);
        } else {
            msgf(g, "You hit the %s.", k->name);
        }
    } else {
        msgf(g, "You miss the %s.", k->name);
    }
}

static void monster_attacks(game_t *g, mon_t *m) {
    const mon_kind_t *k = &mon_table[m->kind];
    int need = 8 + player_ac(g) - k->hd;
    if ((int)rnd(g, 20) + 1 >= need) {
        int dmg = roll(g, k->dmg_n, k->dmg_d);
        g->hp -= dmg;
        msgf(g, "The %s hits you.", k->name);
        if (g->hp <= 0) {
            char by[40];
            snprintf(by, sizeof(by), "a %s", k->name);
            player_dies(g, by);
        }
    } else {
        msgf(g, "The %s misses you.", k->name);
    }
}

/* ---- monsters ---------------------------------------------------- */

int mon_visible(game_t *g, mon_t *m) {
    int pri = room_at(g, g->pr, g->pc);
    int mri = room_at(g, m->r, m->c);
    int dr = m->r - g->pr, dc = m->c - g->pc;
    if (dr >= -1 && dr <= 1 && dc >= -1 && dc <= 1) {
        return 1;
    }
    return pri >= 0 && pri == mri;
}

static int walkable(game_t *g, int r, int c) {
    char t;
    if (r < 0 || r >= MAP_ROWS || c < 0 || c >= MAP_COLS) {
        return 0;
    }
    t = g->map[r][c];
    return t == T_FLOOR || t == T_DOOR || t == T_PASSAGE || t == T_STAIRS;
}

static int mon_at(game_t *g, int r, int c) {
    int i;
    for (i = 0; i < MAX_MONS; i++) {
        if (g->mons[i].kind >= 0 && g->mons[i].r == r && g->mons[i].c == c) {
            return i;
        }
    }
    return -1;
}

static void move_monster(game_t *g, mon_t *m) {
    const mon_kind_t *k = &mon_table[m->kind];
    int dr, dc, tr, tc;

    if (m->asleep) {
        /* Sleepers wake when the player shares their room or bumps close. */
        if (mon_visible(g, m) && (int)rnd(g, 3) == 0) {
            m->asleep = 0;
        }
        return;
    }
    if (m->r == g->pr && m->c == g->pc) {
        return;
    }
    /* Adjacent: attack. */
    if (m->r - g->pr >= -1 && m->r - g->pr <= 1 &&
        m->c - g->pc >= -1 && m->c - g->pc <= 1) {
        monster_attacks(g, m);
        return;
    }
    /* Chase only when it can see the player; bats flit at random. */
    if (!mon_visible(g, m) && !k->mean) {
        return;
    }
    if (k->ch == 'B' && (int)rnd(g, 2) == 0) {
        dr = (int)rnd(g, 3) - 1;
        dc = (int)rnd(g, 3) - 1;
    } else if (mon_visible(g, m)) {
        dr = (g->pr > m->r) - (g->pr < m->r);
        dc = (g->pc > m->c) - (g->pc < m->c);
    } else {
        dr = (int)rnd(g, 3) - 1;
        dc = (int)rnd(g, 3) - 1;
    }
    tr = m->r + dr;
    tc = m->c + dc;
    if (!walkable(g, tr, tc) || mon_at(g, tr, tc) >= 0) {
        /* Try the two single-axis moves. */
        if (walkable(g, m->r + dr, m->c) && mon_at(g, m->r + dr, m->c) < 0 && dr) {
            tr = m->r + dr;
            tc = m->c;
        } else if (walkable(g, m->r, m->c + dc) && mon_at(g, m->r, m->c + dc) < 0 && dc) {
            tr = m->r;
            tc = m->c + dc;
        } else {
            return;
        }
    }
    if (tr == g->pr && tc == g->pc) {
        monster_attacks(g, m);
        return;
    }
    m->r = tr;
    m->c = tc;
}

static void monsters_act(game_t *g) {
    int i;
    for (i = 0; i < MAX_MONS && !g->dead; i++) {
        if (g->mons[i].kind >= 0) {
            move_monster(g, &g->mons[i]);
        }
    }
}

/* ---- hunger and regeneration ------------------------------------- */

static void clock_tick(game_t *g) {
    g->turns++;
    if (g->food > 0) {
        g->food--;
        if (g->food == 300) {
            msgf(g, "You are starting to get hungry.");
        } else if (g->food == 150) {
            msgf(g, "You are weak from hunger.");
        } else if (g->food == 0) {
            msgf(g, "You are starving!");
        }
    } else {
        g->hp--;
        if (g->hp <= 0) {
            player_dies(g, "starvation");
            return;
        }
    }
    if (g->hp < g->maxhp && g->food > 0) {
        int period = 21 - g->level * 2;
        if (period < 3) {
            period = 3;
        }
        if (++g->heal_clock >= period) {
            g->heal_clock = 0;
            g->hp++;
        }
    }
}

/* ---- items ------------------------------------------------------- */

static int inv_add(game_t *g, item_t *it) {
    int i;
    if (it->type == IT_FOOD) {
        for (i = 0; i < INV_SLOTS; i++) {
            if (g->inv[i].type == IT_FOOD) {
                g->inv[i].count += it->count;
                return i;
            }
        }
    }
    for (i = 0; i < INV_SLOTS; i++) {
        if (g->inv[i].type == IT_NONE) {
            g->inv[i].type = it->type;
            g->inv[i].sub = it->sub;
            g->inv[i].count = it->count;
            g->inv[i].enchant = it->enchant;
            return i;
        }
    }
    return -1;
}

static void pick_up(game_t *g) {
    int i;
    for (i = 0; i < MAX_ITEMS; i++) {
        item_t *it = &g->items[i];
        if (it->type == IT_NONE || it->r != g->pr || it->c != g->pc) {
            continue;
        }
        if (it->type == IT_GOLD) {
            g->gold += it->count;
            msgf(g, "You find %d gold pieces.", it->count);
            it->type = IT_NONE;
        } else if (it->type == IT_AMULET) {
            g->has_amulet = 1;
            msgf(g, "You now have the Amulet of Yendor!");
            it->type = IT_NONE;
        } else {
            int slot = inv_add(g, it);
            if (slot < 0) {
                msgf(g, "Your pack is full.");
            } else {
                char name[64];
                inv_name(g, &g->inv[slot], name, sizeof(name));
                msgf(g, "You now have %s (%c).", name, 'a' + slot);
                it->type = IT_NONE;
            }
        }
        return;
    }
}

static void quaff(game_t *g, int slot) {
    inv_t *it = &g->inv[slot];
    switch (it->sub) {
    case P_HEALING:
        g->hp += roll(g, g->level, 4);
        if (g->hp > g->maxhp) {
            g->maxhp++;
            g->hp = g->maxhp;
        }
        msgf(g, "You begin to feel better.");
        break;
    case P_EXTRA:
        g->hp += roll(g, g->level, 8);
        if (g->hp > g->maxhp) {
            g->maxhp += 2;
            g->hp = g->maxhp;
        }
        msgf(g, "You begin to feel much better.");
        break;
    case P_STRENGTH:
        g->str++;
        if (g->str > g->maxstr) {
            g->maxstr = g->str;
        }
        msgf(g, "You feel stronger. What bulging muscles!");
        break;
    case P_POISON:
        g->hp -= roll(g, 1, 8);
        msgf(g, "You feel very sick.");
        if (g->hp <= 0) {
            player_dies(g, "poison");
        }
        break;
    }
    it->type = IT_NONE;
}

static void teleport(game_t *g) {
    int r, c, i;
    for (i = 0; i < 1000; i++) {
        r = (int)rnd(g, MAP_ROWS);
        c = (int)rnd(g, MAP_COLS);
        if (g->map[r][c] == T_FLOOR && mon_at(g, r, c) < 0) {
            g->pr = r;
            g->pc = c;
            mark_seen(g);
            return;
        }
    }
}

static void read_scroll(game_t *g, int slot) {
    inv_t *it = &g->inv[slot];
    switch (it->sub) {
    case S_MAPPING:
        reveal_map(g);
        msgf(g, "Oh, now this scroll has a map on it!");
        break;
    case S_TELEPORT:
        teleport(g);
        msgf(g, "You feel a wrenching sensation.");
        break;
    case S_ENCH_WEAPON:
        if (g->wielding >= 0) {
            g->inv[g->wielding].enchant++;
            msgf(g, "Your weapon glows blue for a moment.");
        } else {
            msgf(g, "Your hands tingle.");
        }
        break;
    case S_ENCH_ARMOR:
        if (g->wearing >= 0) {
            g->inv[g->wearing].enchant++;
            msgf(g, "Your armor glows silver for a moment.");
        } else {
            msgf(g, "Your skin itches.");
        }
        break;
    }
    it->type = IT_NONE;
}

/* ---- commands ---------------------------------------------------- */

static int prompt_slot(game_t *g, int want_type, const char *verb) {
    int i, found = -1, n = 0;
    for (i = 0; i < INV_SLOTS; i++) {
        if (g->inv[i].type == want_type) {
            found = i;
            n++;
        }
    }
    if (n == 0) {
        msgf(g, "You have nothing to %s.", verb);
        return -1;
    }
    /* One matching item: no interactive prompt needed (and line mode
     * always takes the first). Several: the UI asks. */
    return found;
}

extern int ui_pick_slot(game_t *g, int want_type, const char *verb);

static int choose(game_t *g, int want_type, const char *verb) {
    int i, n = 0;
    for (i = 0; i < INV_SLOTS; i++) {
        if (g->inv[i].type == want_type) {
            n++;
        }
    }
    if (n > 1 && !g->line_mode) {
        return ui_pick_slot(g, want_type, verb);
    }
    return prompt_slot(g, want_type, verb);
}

static void show_inventory(game_t *g) {
    int i, n = 0;
    char name[64];
    for (i = 0; i < INV_SLOTS; i++) {
        if (g->inv[i].type == IT_NONE) {
            continue;
        }
        inv_name(g, &g->inv[i], name, sizeof(name));
        if (g->line_mode) {
            printf("%c) %s%s%s\n", 'a' + i, name,
                   i == g->wielding ? " (weapon in hand)" : "",
                   i == g->wearing ? " (being worn)" : "");
        }
        n++;
    }
    if (g->line_mode && g->has_amulet) {
        printf("   the Amulet of Yendor\n");
    }
    if (n == 0) {
        msgf(g, "You are empty handed.");
    }
}

extern void ui_show_inventory(game_t *g);

static void try_move(game_t *g, int dr, int dc) {
    int tr = g->pr + dr, tc = g->pc + dc;
    int mi = mon_at(g, tr, tc);
    if (mi >= 0) {
        attack_monster(g, &g->mons[mi]);
        return;
    }
    if (!walkable(g, tr, tc)) {
        return;
    }
    /* No diagonal squeezing through doorways. */
    if (dr && dc && (g->map[g->pr][g->pc] == T_DOOR || g->map[tr][tc] == T_DOOR)) {
        return;
    }
    g->pr = tr;
    g->pc = tc;
    mark_seen(g);
    pick_up(g);
    if (g->map[tr][tc] == T_STAIRS && !g->line_mode) {
        msgf(g, "There is a staircase here.");
    }
}

static void descend(game_t *g) {
    if (g->map[g->pr][g->pc] != T_STAIRS) {
        msgf(g, "I see no way down.");
        return;
    }
    g->depth++;
    if (g->depth > g->max_depth) {
        g->max_depth = g->depth;
    }
    new_level(g);
    msgf(g, "You descend to level %d.", g->depth);
}

static void ascend(game_t *g) {
    if (g->map[g->pr][g->pc] != T_STAIRS) {
        msgf(g, "I see no way up.");
        return;
    }
    if (!g->has_amulet) {
        msgf(g, "Your way is magically blocked.");
        return;
    }
    if (g->depth == 1) {
        g->dead = 2;
        msgf(g, "You escape with the Amulet of Yendor. Total winner!");
        return;
    }
    g->depth--;
    new_level(g);
    msgf(g, "You climb up to level %d.", g->depth);
}

/* Returns 0 to quit the game loop, 1 to continue.  Sets g->dead on
 * death/win/quit. */
int do_command(game_t *g, int ch) {
    int moved = 1; /* most commands cost a turn */
    int slot;

    g->msg[0] = '\0';

    switch (ch) {
    case 'h': try_move(g, 0, -1); break;
    case 'j': try_move(g, 1, 0); break;
    case 'k': try_move(g, -1, 0); break;
    case 'l': try_move(g, 0, 1); break;
    case 'y': try_move(g, -1, -1); break;
    case 'u': try_move(g, -1, 1); break;
    case 'b': try_move(g, 1, -1); break;
    case 'n': try_move(g, 1, 1); break;
    case '.': case 's': break; /* rest / search: burn a turn */
    case '>': descend(g); break;
    case '<': ascend(g); break;
    case 'i':
        if (g->line_mode) {
            show_inventory(g);
        } else {
            ui_show_inventory(g);
        }
        moved = 0;
        break;
    case 'e':
        slot = choose(g, IT_FOOD, "eat");
        if (slot >= 0) {
            g->food = 1300;
            if (--g->inv[slot].count <= 0) {
                g->inv[slot].type = IT_NONE;
            }
            msgf(g, "Yum, that tasted good.");
        }
        break;
    case 'q':
        slot = choose(g, IT_POTION, "quaff");
        if (slot >= 0) {
            quaff(g, slot);
        }
        break;
    case 'r':
        slot = choose(g, IT_SCROLL, "read");
        if (slot >= 0) {
            read_scroll(g, slot);
        }
        break;
    case 'w':
        slot = choose(g, IT_WEAPON, "wield");
        if (slot >= 0) {
            char name[64];
            g->wielding = slot;
            inv_name(g, &g->inv[slot], name, sizeof(name));
            msgf(g, "You are now wielding %s.", name);
        }
        break;
    case 'W':
        slot = choose(g, IT_ARMOR, "wear");
        if (slot >= 0) {
            char name[64];
            g->wearing = slot;
            inv_name(g, &g->inv[slot], name, sizeof(name));
            msgf(g, "You are now wearing %s.", name);
        }
        break;
    case 'T':
        if (g->wearing >= 0) {
            g->wearing = -1;
            msgf(g, "You take off your armor.");
        } else {
            msgf(g, "You are not wearing armor.");
        }
        break;
    case 'S':
        if (save_game(g, "rogue.sav") == 0) {
            msgf(g, "Game saved.");
            g->dead = 3;
        } else {
            msgf(g, "Cannot write rogue.sav.");
        }
        return 0;
    case 'Q':
        g->dead = 3;
        msgf(g, "You quit.");
        return 0;
    case '&':
        if (g->wizard) {
            g->pr = g->stairs_r;
            g->pc = g->stairs_c;
            mark_seen(g);
            msgf(g, "Zap! You stand on the staircase.");
        }
        moved = 0;
        break;
    case '*':
        if (g->wizard) {
            reveal_map(g);
            msgf(g, "The level lies bare before you.");
        }
        moved = 0;
        break;
    case 'A':
        if (g->wizard) {
            g->has_amulet = 1;
            msgf(g, "You now have the Amulet of Yendor!");
        }
        moved = 0;
        break;
    case '+':
        if (g->wizard) {
            g->hp = g->maxhp = 999;
            msgf(g, "You feel invincible.");
        }
        moved = 0;
        break;
    default:
        moved = 0;
        break;
    }

    if (g->dead) {
        return 0;
    }
    if (moved) {
        monsters_act(g);
        if (!g->dead) {
            clock_tick(g);
        }
    }
    return g->dead ? 0 : 1;
}
