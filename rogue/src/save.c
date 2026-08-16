/* Save games and the score file.  Only the current level exists (the
 * dungeon regrows behind you, just like 1980), so a save is one flat
 * dump of the game struct. */

#include "rogue.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define SAVE_MAGIC "RGUE1\n"

int save_game(game_t *g, const char *path) {
    FILE *f = fopen(path, "wb");
    if (!f) {
        return -1;
    }
    if (fwrite(SAVE_MAGIC, 1, 6, f) != 6 ||
        fwrite(g, 1, sizeof(*g), f) != sizeof(*g)) {
        fclose(f);
        remove(path);
        return -1;
    }
    fclose(f);
    return 0;
}

int load_game(game_t *g, const char *path) {
    char magic[6];
    int line_mode = g->line_mode;
    int wizard = g->wizard;
    FILE *f = fopen(path, "rb");
    if (!f) {
        return -1;
    }
    if (fread(magic, 1, 6, f) != 6 || memcmp(magic, SAVE_MAGIC, 6) != 0 ||
        fread(g, 1, sizeof(*g), f) != sizeof(*g)) {
        fclose(f);
        return -1;
    }
    fclose(f);
    /* Permadeath: the save is consumed by loading it. */
    remove(path);
    g->line_mode = line_mode;
    g->wizard = wizard;
    g->dead = 0;
    g->msg[0] = '\0';
    return 0;
}

void record_score(game_t *g, const char *path) {
    FILE *f = fopen(path, "a");
    if (!f) {
        return;
    }
    if (g->dead == 2) {
        fprintf(f, "%d gold, escaped with the Amulet (level %d)\n",
                g->gold, g->level);
    } else if (g->dead == 1) {
        fprintf(f, "%d gold, killed by %s on dungeon level %d\n",
                g->gold, g->death_by, g->depth);
    } else {
        fprintf(f, "%d gold, quit on dungeon level %d\n",
                g->gold, g->depth);
    }
    fclose(f);
}

void show_scores(const char *path, FILE *out) {
    char line[128];
    int n = 0;
    FILE *f = fopen(path, "r");
    if (!f) {
        return;
    }
    fprintf(out, "\nTop Rogueists:\n");
    while (n < 10 && fgets(line, sizeof(line), f)) {
        fprintf(out, "  %s", line);
        n++;
    }
    fclose(f);
}
