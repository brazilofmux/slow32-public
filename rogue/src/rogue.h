#ifndef ROGUE_H
#define ROGUE_H

#include <stdio.h>

#define MAP_ROWS 22
#define MAP_COLS 80

#define MAX_ROOMS 9
#define MAX_MONS  16
#define MAX_ITEMS 24
#define INV_SLOTS 26

#define AMULET_LEVEL 26

/* Tile characters double as the map representation. */
#define T_ROCK    ' '
#define T_HWALL   '-'
#define T_VWALL   '|'
#define T_DOOR    '+'
#define T_FLOOR   '.'
#define T_PASSAGE '#'
#define T_STAIRS  '%'

/* Per-cell flags. */
#define F_SEEN 0x01

/* Item types. */
#define IT_NONE   0
#define IT_GOLD   1
#define IT_FOOD   2
#define IT_POTION 3
#define IT_SCROLL 4
#define IT_WEAPON 5
#define IT_ARMOR  6
#define IT_AMULET 7

/* Potion subtypes. */
#define P_HEALING  0
#define P_EXTRA    1
#define P_STRENGTH 2
#define P_POISON   3
#define NUM_POTIONS 4

/* Scroll subtypes. */
#define S_MAPPING  0
#define S_TELEPORT 1
#define S_ENCH_WEAPON 2
#define S_ENCH_ARMOR  3
#define NUM_SCROLLS 4

/* Weapon subtypes. */
#define W_DAGGER 0
#define W_MACE   1
#define W_SWORD  2
#define W_2SWORD 3
#define NUM_WEAPONS 4

/* Armor subtypes. */
#define A_LEATHER 0
#define A_RING    1
#define A_CHAIN   2
#define A_PLATE   3
#define NUM_ARMORS 4

typedef struct {
    int r, c;       /* top-left corner (of the wall) */
    int h, w;       /* interior height/width */
    int gone;       /* "gone" room: just a corridor waypoint */
} room_t;

typedef struct {
    int type;       /* IT_* or IT_NONE for a free slot */
    int sub;
    int count;      /* gold amount / food rations */
    int enchant;    /* weapon/armor plus */
    int r, c;       /* on the floor */
} item_t;

typedef struct {
    int kind;       /* index into mon_table, -1 for a free slot */
    int r, c;
    int hp;
    int asleep;
} mon_t;

typedef struct {
    const char *name;
    char ch;
    int minlvl, maxlvl; /* dungeon depths where it appears */
    int hd;             /* hit dice (d8s) */
    int ac;             /* protection 0 (none) .. 9 */
    int dmg_n, dmg_d;   /* damage dice */
    int exp;
    int mean;           /* starts awake and hunting */
} mon_kind_t;

typedef struct {
    int type;       /* IT_NONE for a free slot */
    int sub;
    int count;
    int enchant;
} inv_t;

typedef struct {
    unsigned rng;

    /* Level. */
    char map[MAP_ROWS][MAP_COLS];
    unsigned char flags[MAP_ROWS][MAP_COLS];
    room_t rooms[MAX_ROOMS];
    mon_t mons[MAX_MONS];
    item_t items[MAX_ITEMS];
    int stairs_r, stairs_c;

    /* Player. */
    int pr, pc;
    int hp, maxhp;
    int str, maxstr;
    int level, exp;
    int gold;
    int depth;
    int max_depth;
    int food;
    int wielding;   /* inventory slot or -1 */
    int wearing;    /* inventory slot or -1 */
    int has_amulet;
    inv_t inv[INV_SLOTS];

    int turns;
    int heal_clock;
    int dead;       /* 0 alive, 1 dead, 2 escaped with amulet, 3 quit */
    char death_by[32];

    int wizard;
    int line_mode;
    char msg[100];
    char prev_msg[100];
} game_t;

extern const mon_kind_t mon_table[];
extern const int mon_table_len;

/* dungeon.c */
void new_level(game_t *g);
int  room_at(game_t *g, int r, int c);
void mark_seen(game_t *g);
void reveal_map(game_t *g);

/* game.c */
unsigned rnd(game_t *g, int n);
int  roll(game_t *g, int n, int d);
void msgf(game_t *g, const char *fmt, ...);
void init_game(game_t *g, unsigned seed);
int  do_command(game_t *g, int ch);
int  mon_visible(game_t *g, mon_t *m);
void inv_name(game_t *g, inv_t *it, char *buf, int cap);
const char *rank_name(int level);
int  player_armor(game_t *g);

/* save.c */
int  save_game(game_t *g, const char *path);
int  load_game(game_t *g, const char *path);
void record_score(game_t *g, const char *path);
void show_scores(const char *path, FILE *out);

/* ui.c */
void ui_play(game_t *g);
void line_render(game_t *g, FILE *out);

#endif /* ROGUE_H */
