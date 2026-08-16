/* Level generation: the classic 3x3 grid of rooms, corridors between
 * neighbors, everything hidden until walked into. */

#include "rogue.h"

#include <string.h>

/* The screen is carved into a 3x3 grid of cells, one room per cell. */
#define GRID 3
#define CELL_H (MAP_ROWS / GRID)      /* 7 */
#define CELL_W (MAP_COLS / GRID)      /* 26 */

static int cell_of(int i, int *top, int *left) {
    *top = (i / GRID) * CELL_H;
    *left = (i % GRID) * CELL_W;
    return i;
}

int room_at(game_t *g, int r, int c) {
    int i;
    for (i = 0; i < MAX_ROOMS; i++) {
        room_t *rm = &g->rooms[i];
        if (rm->gone) {
            continue;
        }
        if (r >= rm->r && r <= rm->r + rm->h + 1 &&
            c >= rm->c && c <= rm->c + rm->w + 1) {
            return i;
        }
    }
    return -1;
}

static void draw_room(game_t *g, room_t *rm) {
    int r, c;
    for (c = rm->c; c <= rm->c + rm->w + 1; c++) {
        g->map[rm->r][c] = T_HWALL;
        g->map[rm->r + rm->h + 1][c] = T_HWALL;
    }
    for (r = rm->r + 1; r <= rm->r + rm->h; r++) {
        g->map[r][rm->c] = T_VWALL;
        g->map[r][rm->c + rm->w + 1] = T_VWALL;
        for (c = rm->c + 1; c <= rm->c + rm->w; c++) {
            g->map[r][c] = T_FLOOR;
        }
    }
}

/* Carve rock into passage along a row/column span (inclusive), leaving
 * doors, walls, and existing corridors alone. */
static void carve_h(game_t *g, int r, int ca, int cb) {
    int c, lo = ca < cb ? ca : cb, hi = ca < cb ? cb : ca;
    for (c = lo; c <= hi; c++) {
        if (g->map[r][c] == T_ROCK) {
            g->map[r][c] = T_PASSAGE;
        }
    }
}

static void carve_v(game_t *g, int ra, int rb, int c) {
    int r, lo = ra < rb ? ra : rb, hi = ra < rb ? rb : ra;
    for (r = lo; r <= hi; r++) {
        if (g->map[r][c] == T_ROCK) {
            g->map[r][c] = T_PASSAGE;
        }
    }
}

/* Dig a dog-legged corridor between two points (which sit on room walls
 * and become doors).  Room placement guarantees r1+2 <= r2 for vertical
 * connections (c1+2 <= c2 for horizontal), so the mid leg always has a
 * lane of rock to run in. */
static void dig(game_t *g, int r1, int c1, int r2, int c2, int vertical) {
    g->map[r1][c1] = T_DOOR;
    g->map[r2][c2] = T_DOOR;
    if (vertical) {
        int midr = (r1 + r2) / 2;
        carve_v(g, r1 + 1, midr, c1);
        carve_h(g, midr, c1, c2);
        carve_v(g, midr, r2 - 1, c2);
    } else {
        int midc = (c1 + c2) / 2;
        carve_h(g, r1, c1 + 1, midc);
        carve_v(g, r1, r2, midc);
        carve_h(g, r2, midc, c2 - 1);
    }
    /* Doors dug into a gone room's waypoint are just passage. */
    if (room_at(g, r1, c1) < 0) {
        g->map[r1][c1] = T_PASSAGE;
    }
    if (room_at(g, r2, c2) < 0) {
        g->map[r2][c2] = T_PASSAGE;
    }
}

/* Pick a spot on the shared wall of room i facing direction (dr,dc). */
static void wall_spot(game_t *g, int i, int dr, int dc, int *r, int *c) {
    room_t *rm = &g->rooms[i];
    if (rm->gone) {
        *r = rm->r;
        *c = rm->c;
        return;
    }
    if (dr < 0) {
        *r = rm->r;
        *c = rm->c + 1 + (int)rnd(g, rm->w);
    } else if (dr > 0) {
        *r = rm->r + rm->h + 1;
        *c = rm->c + 1 + (int)rnd(g, rm->w);
    } else if (dc < 0) {
        *r = rm->r + 1 + (int)rnd(g, rm->h);
        *c = rm->c;
    } else {
        *r = rm->r + 1 + (int)rnd(g, rm->h);
        *c = rm->c + rm->w + 1;
    }
}

static void connect_rooms(game_t *g, int a, int b) {
    int r1, c1, r2, c2;
    int vertical = (b - a == GRID);
    if (vertical) {
        wall_spot(g, a, 1, 0, &r1, &c1);
        wall_spot(g, b, -1, 0, &r2, &c2);
    } else {
        wall_spot(g, a, 0, 1, &r1, &c1);
        wall_spot(g, b, 0, -1, &r2, &c2);
    }
    dig(g, r1, c1, r2, c2, vertical);
}

static void rand_floor(game_t *g, int *out_r, int *out_c) {
    int i, r, c;
    for (i = 0; i < 1000; i++) {
        room_t *rm;
        int ri = (int)rnd(g, MAX_ROOMS);
        rm = &g->rooms[ri];
        if (rm->gone) {
            continue;
        }
        r = rm->r + 1 + (int)rnd(g, rm->h);
        c = rm->c + 1 + (int)rnd(g, rm->w);
        if (g->map[r][c] == T_FLOOR) {
            *out_r = r;
            *out_c = c;
            return;
        }
    }
    *out_r = g->rooms[0].r + 1;
    *out_c = g->rooms[0].c + 1;
}

static int mon_spot_free(game_t *g, int r, int c) {
    int i;
    if (r == g->pr && c == g->pc) {
        return 0;
    }
    for (i = 0; i < MAX_MONS; i++) {
        if (g->mons[i].kind >= 0 && g->mons[i].r == r && g->mons[i].c == c) {
            return 0;
        }
    }
    return 1;
}

static void place_monsters(game_t *g) {
    int want = 3 + (int)rnd(g, 4) + g->depth / 4;
    int i, m = 0;
    if (want > MAX_MONS) {
        want = MAX_MONS;
    }
    for (i = 0; i < MAX_MONS; i++) {
        g->mons[i].kind = -1;
    }
    while (m < want) {
        int k, tries, r, c;
        for (tries = 0; tries < 100; tries++) {
            k = (int)rnd(g, mon_table_len);
            if (g->depth >= mon_table[k].minlvl && g->depth <= mon_table[k].maxlvl) {
                break;
            }
        }
        if (tries >= 100) {
            break;
        }
        rand_floor(g, &r, &c);
        if (!mon_spot_free(g, r, c)) {
            continue;
        }
        g->mons[m].kind = k;
        g->mons[m].r = r;
        g->mons[m].c = c;
        g->mons[m].hp = roll(g, mon_table[k].hd, 8);
        g->mons[m].asleep = mon_table[k].mean ? ((int)rnd(g, 4) == 0) : 1;
        m++;
    }
}

static void place_items(game_t *g) {
    int want = 2 + (int)rnd(g, 4);
    int i, n = 0;
    for (i = 0; i < MAX_ITEMS; i++) {
        g->items[i].type = IT_NONE;
    }
    while (n < want) {
        item_t *it = &g->items[n];
        int pick = (int)rnd(g, 100);
        rand_floor(g, &it->r, &it->c);
        it->count = 1;
        it->enchant = 0;
        if (pick < 35) {
            it->type = IT_GOLD;
            it->count = 5 + (int)rnd(g, 25 + 10 * g->depth);
        } else if (pick < 55) {
            it->type = IT_POTION;
            it->sub = (int)rnd(g, NUM_POTIONS);
        } else if (pick < 70) {
            it->type = IT_SCROLL;
            it->sub = (int)rnd(g, NUM_SCROLLS);
        } else if (pick < 80) {
            it->type = IT_FOOD;
        } else if (pick < 90) {
            it->type = IT_WEAPON;
            it->sub = (int)rnd(g, NUM_WEAPONS);
        } else {
            it->type = IT_ARMOR;
            it->sub = (int)rnd(g, NUM_ARMORS);
        }
        n++;
    }
    if (g->depth >= AMULET_LEVEL && !g->has_amulet && n < MAX_ITEMS) {
        item_t *it = &g->items[n];
        it->type = IT_AMULET;
        it->sub = 0;
        it->count = 1;
        it->enchant = 0;
        rand_floor(g, &it->r, &it->c);
    }
}

void new_level(game_t *g) {
    int i, gone_budget;

    memset(g->map, T_ROCK, sizeof(g->map));
    memset(g->flags, 0, sizeof(g->flags));

    gone_budget = (int)rnd(g, 3);
    for (i = 0; i < MAX_ROOMS; i++) {
        int top, left;
        room_t *rm = &g->rooms[i];
        cell_of(i, &top, &left);
        rm->gone = 0;
        if (gone_budget > 0 && (int)rnd(g, 3) == 0) {
            /* A gone room: corridors route through a single waypoint. */
            rm->gone = 1;
            rm->r = top + 1 + (int)rnd(g, CELL_H - 3);
            rm->c = left + 1 + (int)rnd(g, CELL_W - 3);
            rm->h = rm->w = 0;
            gone_budget--;
            continue;
        }
        /* Keep the far wall at least 2 cells inside the grid cell so a
         * corridor to the neighbor always has a rock lane to bend in. */
        rm->h = 2 + (int)rnd(g, CELL_H - 4);
        rm->w = 4 + (int)rnd(g, CELL_W - 8);
        rm->r = top + (int)rnd(g, CELL_H - rm->h - 2);
        rm->c = left + (int)rnd(g, CELL_W - rm->w - 2);
        draw_room(g, rm);
    }

    /* Connect each room to its right and lower neighbor; that already
     * yields a connected graph on the full 3x3 grid. */
    for (i = 0; i < MAX_ROOMS; i++) {
        if (i % GRID != GRID - 1) {
            connect_rooms(g, i, i + 1);
        }
        if (i / GRID != GRID - 1) {
            connect_rooms(g, i, i + GRID);
        }
    }
    /* Gone-room waypoints end as plain passage. */
    for (i = 0; i < MAX_ROOMS; i++) {
        if (g->rooms[i].gone) {
            if (g->map[g->rooms[i].r][g->rooms[i].c] == T_ROCK) {
                g->map[g->rooms[i].r][g->rooms[i].c] = T_PASSAGE;
            }
        }
    }

    rand_floor(g, &g->stairs_r, &g->stairs_c);
    g->map[g->stairs_r][g->stairs_c] = T_STAIRS;

    do {
        rand_floor(g, &g->pr, &g->pc);
    } while (g->pr == g->stairs_r && g->pc == g->stairs_c);

    place_monsters(g);
    place_items(g);
    mark_seen(g);
}

/* Reveal what the player can see: the whole room they stand in, or the
 * 8 neighbors when in a corridor or doorway. */
void mark_seen(game_t *g) {
    int i = room_at(g, g->pr, g->pc);
    int r, c;
    if (i >= 0 && g->map[g->pr][g->pc] != T_DOOR) {
        room_t *rm = &g->rooms[i];
        for (r = rm->r; r <= rm->r + rm->h + 1; r++) {
            for (c = rm->c; c <= rm->c + rm->w + 1; c++) {
                g->flags[r][c] |= F_SEEN;
            }
        }
    }
    for (r = g->pr - 1; r <= g->pr + 1; r++) {
        for (c = g->pc - 1; c <= g->pc + 1; c++) {
            if (r >= 0 && r < MAP_ROWS && c >= 0 && c < MAP_COLS) {
                g->flags[r][c] |= F_SEEN;
            }
        }
    }
}

void reveal_map(game_t *g) {
    int r, c;
    for (r = 0; r < MAP_ROWS; r++) {
        for (c = 0; c < MAP_COLS; c++) {
            if (g->map[r][c] != T_ROCK) {
                g->flags[r][c] |= F_SEEN;
            }
        }
    }
}
