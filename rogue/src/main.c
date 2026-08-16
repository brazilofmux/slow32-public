/* rogue - the Saturday afternoon, on SLOW-32.
 *
 *   rogue                 full-screen game on the term service
 *   rogue --line          line mode (also the no-term fallback)
 *   rogue --seed N        deterministic dungeon, no save restore
 *   rogue --wizard        enable the debug commands (& * A)
 */

#include "rogue.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <term.h>
#include <time.h>

static game_t g;

static void line_loop(void) {
    char buf[80];
    printf("Hello. Welcome to the Dungeons of Doom.\n");
    line_render(&g, stdout);
    while (fgets(buf, sizeof(buf), stdin)) {
        int ch = buf[0];
        if (ch == '\0' || ch == '\n') {
            line_render(&g, stdout);
            continue;
        }
        if (!do_command(&g, ch)) {
            break;
        }
        line_render(&g, stdout);
    }
    if (!g.dead) {
        g.dead = 3;
    }
}

int main(int argc, char **argv) {
    int i, line_mode = 0, wizard = 0, restore = 1;
    unsigned seed = 0;

    for (i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--line") == 0) {
            line_mode = 1;
        } else if (strcmp(argv[i], "--wizard") == 0) {
            wizard = 1;
        } else if (strcmp(argv[i], "--seed") == 0 && i + 1 < argc) {
            seed = (unsigned)atoi(argv[++i]);
            restore = 0;
        } else {
            printf("usage: rogue [--line] [--wizard] [--seed N]\n");
            return 1;
        }
    }

    g.line_mode = line_mode;
    g.wizard = wizard;
    if (restore && load_game(&g, "rogue.sav") == 0) {
        g.msg[0] = '\0';
        if (line_mode) {
            printf("Welcome back to the Dungeons of Doom.\n");
        }
    } else {
        if (!seed) {
            seed = (unsigned)time(NULL) ^ 0x9E3779B9u;
        }
        init_game(&g, seed);
        g.line_mode = line_mode;
        g.wizard = wizard;
    }

    if (!line_mode && term_init() == 0) {
        ui_play(&g);
        term_cleanup();
    } else {
        g.line_mode = 1;
        line_loop();
    }

    if (g.dead == 1 || g.dead == 2) {
        record_score(&g, "rogue.scr");
        show_scores("rogue.scr", stdout);
    }
    return 0;
}
