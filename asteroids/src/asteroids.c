/* asteroids - the vec flagship.
 *
 * A triangle with a gun, on the tube. 4096x4096 wrapped space, all
 * integer math (positions in 1/256 space units) so every engine
 * computes the identical frame.
 *
 *   asteroids                     live game; s32-crt is the screen
 *   asteroids --frames N --seed S deterministic test run, no pacing,
 *                                 one injected key event per frame,
 *                                 prints a report line at exit
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <tube.h>

#include "vfont.h"

#define SPACE      (4096 * 256)      /* 1/256 space units */
#define MAX_ROCKS  28
#define MAX_SHOTS  4
#define MAX_DEBRIS 24
#define LIST_CAP   2048

#define K_UP    0x100
#define K_DOWN  0x101
#define K_LEFT  0x102
#define K_RIGHT 0x103

/* cos(2*pi*a/64) * 256; sin(a) = COS64[(a + 48) & 63]. */
static const int16_t COS64[64] = {
     256,  255,  251,  245,  237,  226,  213,  198,
     181,  162,  142,  121,   98,   74,   50,   25,
       0,  -25,  -50,  -74,  -98, -121, -142, -162,
    -181, -198, -213, -226, -237, -245, -251, -255,
    -256, -255, -251, -245, -237, -226, -213, -198,
    -181, -162, -142, -121,  -98,  -74,  -50,  -25,
       0,   25,   50,   74,   98,  121,  142,  162,
     181,  198,  213,  226,  237,  245,  251,  255,
};

static int cos64(int a) { return COS64[a & 63]; }
static int sin64(int a) { return COS64[(a + 48) & 63]; }

/* Three rock outlines, vertices in 1/256ths of the rock radius. */
static const int16_t rock_shape[3][12][2] = {
    { {256,64},{160,224},{16,256},{-144,224},{-256,96},{-224,-32},
      {-256,-160},{-112,-256},{16,-208},{144,-256},{256,-144},{208,-16} },
    { {224,96},{96,256},{-64,208},{-208,240},{-256,32},{-160,-64},
      {-240,-192},{-64,-256},{64,-224},{208,-208},{256,-64},{176,16} },
    { {256,32},{192,192},{48,224},{-96,256},{-240,144},{-256,-48},
      {-192,-224},{-16,-256},{96,-192},{224,-240},{240,-96},{160,-32} },
};

typedef struct {
    int alive;
    int32_t x, y, vx, vy;
    int size;    /* 2 large, 1 medium, 0 small */
    int shape;
} rock_t;

typedef struct {
    int ttl;
    int32_t x, y, vx, vy;
} shot_t;

typedef struct {
    int ttl;
    int32_t x, y, vx, vy;
} debris_t;

typedef struct {
    uint16_t code;
    uint8_t down;
    uint8_t pad;
} keyev_t;

static rock_t rocks[MAX_ROCKS];
static shot_t shots[MAX_SHOTS];
static debris_t debris[MAX_DEBRIS];
static uint32_t list[LIST_CAP];

static uint32_t rng_state = 1;
static int32_t px, py, pvx, pvy;   /* ship */
static int pangle;                 /* 0..63, 16 = up */
static int alive, respawn_timer, invuln;
static int lives, wave;
static unsigned score, next_life;
static int game_over;
static unsigned frame;
static int test_mode, thrusting;
static int held_left, held_right, held_up, held_fire;
static int fire_latch, hyper_latch;

static uint32_t rnd(uint32_t n) {
    uint32_t x = rng_state;
    x ^= x << 13;
    x ^= x >> 17;
    x ^= x << 5;
    rng_state = x;
    return n ? (x >> 8) % n : 0;
}

static int32_t wrap(int32_t v) {
    v %= SPACE;
    if (v < 0) {
        v += SPACE;
    }
    return v;
}

/* Wrap-aware distance check in whole space units, radii summed. */
static int hits(int32_t ax, int32_t ay, int32_t bx, int32_t by, int rr) {
    int32_t dx = (ax - bx) / 256;
    int32_t dy = (ay - by) / 256;
    if (dx > 2048) dx -= 4096;
    if (dx < -2048) dx += 4096;
    if (dy > 2048) dy -= 4096;
    if (dy < -2048) dy += 4096;
    return dx * dx + dy * dy < rr * rr;
}

static int rock_radius(int size) {
    return size == 2 ? 260 : size == 1 ? 130 : 65;
}

static unsigned rock_points(int size) {
    return size == 2 ? 20u : size == 1 ? 50u : 100u;
}

static void spawn_rock(int size, int32_t x, int32_t y) {
    int i;
    for (i = 0; i < MAX_ROCKS; i++) {
        if (!rocks[i].alive) {
            int sp = (3 - size) * 3 + 3; /* small rocks fly faster */
            rocks[i].alive = 1;
            rocks[i].size = size;
            rocks[i].shape = (int)rnd(3);
            rocks[i].x = wrap(x);
            rocks[i].y = wrap(y);
            rocks[i].vx = (int32_t)rnd(2u * (uint32_t)sp * 256u) - sp * 256;
            rocks[i].vy = (int32_t)rnd(2u * (uint32_t)sp * 256u) - sp * 256;
            return;
        }
    }
}

static void spawn_wave(void) {
    int n = 3 + wave;
    int i;
    if (n > 6) {
        n = 6;
    }
    for (i = 0; i < n; i++) {
        /* Away from the center where the ship sits. */
        int32_t x = (int32_t)(1024 + rnd(2048)) * 256;
        int32_t y = (int32_t)rnd(SPACE);
        if (rnd(2)) {
            x = (int32_t)rnd(SPACE);
            y = (int32_t)(1024 + rnd(2048)) * 256;
        }
        spawn_rock(2, wrap(x + SPACE / 2), y);
    }
}

static void spawn_debris(int32_t x, int32_t y, int n) {
    int i;
    for (i = 0; i < MAX_DEBRIS && n > 0; i++) {
        if (debris[i].ttl <= 0) {
            debris[i].ttl = 12 + (int)rnd(10);
            debris[i].x = x;
            debris[i].y = y;
            debris[i].vx = (int32_t)rnd(4096) - 2048;
            debris[i].vy = (int32_t)rnd(4096) - 2048;
            n--;
        }
    }
}

static void kill_rock(int i) {
    rock_t *r = &rocks[i];
    score += rock_points(r->size);
    if (score >= next_life) {
        lives++;
        next_life += 10000u;
    }
    spawn_debris(r->x, r->y, 6);
    if (r->size > 0) {
        spawn_rock(r->size - 1, r->x, r->y);
        spawn_rock(r->size - 1, r->x, r->y);
    }
    r->alive = 0;
}

static void kill_ship(void) {
    spawn_debris(px, py, 10);
    alive = 0;
    lives--;
    respawn_timer = 60;
    if (lives <= 0) {
        game_over = 1;
    }
}

static int center_clear(void) {
    int i;
    for (i = 0; i < MAX_ROCKS; i++) {
        if (rocks[i].alive &&
            hits(rocks[i].x, rocks[i].y, SPACE / 2, SPACE / 2,
                 rock_radius(rocks[i].size) + 500)) {
            return 0;
        }
    }
    return 1;
}

static void reset_ship(void) {
    px = py = SPACE / 2;
    pvx = pvy = 0;
    pangle = 16;
    alive = 1;
    invuln = 60;
}

static void fire(void) {
    int i;
    if (!alive) {
        return;
    }
    for (i = 0; i < MAX_SHOTS; i++) {
        if (shots[i].ttl <= 0) {
            shots[i].ttl = 34;
            shots[i].x = wrap(px + 120 * cos64(pangle));
            shots[i].y = wrap(py + 120 * sin64(pangle));
            shots[i].vx = pvx + 120 * cos64(pangle);
            shots[i].vy = pvy + 120 * sin64(pangle);
            return;
        }
    }
}

static void hyperspace(void) {
    if (!alive) {
        return;
    }
    px = (int32_t)rnd(SPACE);
    py = (int32_t)rnd(SPACE);
    pvx = pvy = 0;
    if (rnd(8) == 0) {
        kill_ship();
    }
}

static void step(void) {
    int i, j, live_rocks = 0;

    frame++;

    if (alive) {
        if (held_left) {
            pangle = (pangle + 1) & 63;
        }
        if (held_right) {
            pangle = (pangle - 1) & 63;
        }
        thrusting = held_up;
        if (thrusting) {
            pvx += 3 * cos64(pangle);
            pvy += 3 * sin64(pangle);
        }
        /* gentle drag, clamp top speed */
        pvx -= pvx / 64;
        pvy -= pvy / 64;
        if (pvx > 5000) pvx = 5000;
        if (pvx < -5000) pvx = -5000;
        if (pvy > 5000) pvy = 5000;
        if (pvy < -5000) pvy = -5000;
        px = wrap(px + pvx);
        py = wrap(py + pvy);
        if (invuln > 0) {
            invuln--;
        }
    } else if (!game_over) {
        if (respawn_timer > 0) {
            respawn_timer--;
        } else if (center_clear()) {
            reset_ship();
        }
    }

    if (fire_latch) {
        fire_latch = 0;
        fire();
    }
    if (hyper_latch) {
        hyper_latch = 0;
        hyperspace();
    }

    for (i = 0; i < MAX_SHOTS; i++) {
        if (shots[i].ttl > 0) {
            shots[i].ttl--;
            shots[i].x = wrap(shots[i].x + shots[i].vx);
            shots[i].y = wrap(shots[i].y + shots[i].vy);
        }
    }
    for (i = 0; i < MAX_DEBRIS; i++) {
        if (debris[i].ttl > 0) {
            debris[i].ttl--;
            debris[i].x = wrap(debris[i].x + debris[i].vx);
            debris[i].y = wrap(debris[i].y + debris[i].vy);
        }
    }

    for (i = 0; i < MAX_ROCKS; i++) {
        rock_t *r = &rocks[i];
        if (!r->alive) {
            continue;
        }
        live_rocks++;
        r->x = wrap(r->x + r->vx);
        r->y = wrap(r->y + r->vy);

        for (j = 0; j < MAX_SHOTS; j++) {
            if (shots[j].ttl > 0 &&
                hits(shots[j].x, shots[j].y, r->x, r->y,
                     rock_radius(r->size) + 8)) {
                shots[j].ttl = 0;
                kill_rock(i);
                break;
            }
        }
        if (r->alive && alive && !invuln &&
            hits(px, py, r->x, r->y, rock_radius(r->size) + 60)) {
            kill_rock(i);
            kill_ship();
        }
    }

    if (live_rocks == 0 && !game_over) {
        wave++;
        spawn_wave();
    }
}

static void draw_poly(uint32_t *n, int32_t cx, int32_t cy,
                      const int16_t (*pts)[2], int npts, int scale) {
    int i;
    for (i = 0; i <= npts; i++) {
        const int16_t *p = pts[i % npts];
        int x = (int)((cx / 256 + (p[0] * scale) / 256));
        int y = (int)((cy / 256 + (p[1] * scale) / 256));
        if (x < 0) x = 0;
        if (x > 4095) x = 4095;
        if (y < 0) y = 0;
        if (y > 4095) y = 4095;
        list[(*n)++] = TUBE_WORD(i == 0 ? TUBE_OP_MOVE : TUBE_OP_DRAW,
                                 (uint32_t)x, (uint32_t)y);
    }
}

static void draw_ship(uint32_t *n, int32_t cx, int32_t cy, int angle,
                      int flame) {
    /* nose, wing, notch, wing — classic triangle-with-a-keel */
    int16_t pts[6][2];
    int c = cos64(angle), s = sin64(angle);
    static const int base[6][2] = {
        { 90, 0 }, { -70, 55 }, { -45, 27 }, { -45, -27 }, { -70, -55 }, { 90, 0 }
    };
    int i;
    /* Rotate into space units; draw_poly with scale 256 passes them
       through unscaled. */
    for (i = 0; i < 6; i++) {
        pts[i][0] = (int16_t)((base[i][0] * c - base[i][1] * s) / 256);
        pts[i][1] = (int16_t)((base[i][0] * s + base[i][1] * c) / 256);
    }
    draw_poly(n, cx, cy, (const int16_t (*)[2])pts, 5, 256);
    if (flame) {
        int16_t fl[3][2];
        static const int fb[3][2] = { { -45, 18 }, { -95, 0 }, { -45, -18 } };
        for (i = 0; i < 3; i++) {
            fl[i][0] = (int16_t)((fb[i][0] * c - fb[i][1] * s) / 256);
            fl[i][1] = (int16_t)((fb[i][0] * s + fb[i][1] * c) / 256);
        }
        list[(*n)++] = TUBE_WORD_INTEN(160);
        draw_poly(n, cx, cy, (const int16_t (*)[2])fl, 2, 256);
        list[(*n)++] = TUBE_WORD_INTEN(255);
    }
}

static uint32_t build_frame(void) {
    uint32_t n = 0;
    int i;

    list[n++] = TUBE_WORD_COLOR(0xFFFFFF);
    list[n++] = TUBE_WORD_INTEN(200);
    vfont_uint(list, &n, 120, 3900, 14, score, 2);
    for (i = 0; i < lives && i < 8; i++) {
        draw_ship(&n, (int32_t)(160 + i * 140) * 256, 3700 * 256, 16, 0);
    }

    list[n++] = TUBE_WORD_INTEN(180);
    for (i = 0; i < MAX_ROCKS; i++) {
        if (rocks[i].alive) {
            draw_poly(&n, rocks[i].x, rocks[i].y,
                      rock_shape[rocks[i].shape], 12,
                      rock_radius(rocks[i].size));
        }
    }

    list[n++] = TUBE_WORD_INTEN(255);
    for (i = 0; i < MAX_SHOTS; i++) {
        if (shots[i].ttl > 0) {
            list[n++] = TUBE_WORD(TUBE_OP_POINT,
                                  (uint32_t)(shots[i].x / 256) & 0xFFFu,
                                  (uint32_t)(shots[i].y / 256) & 0xFFFu);
        }
    }
    list[n++] = TUBE_WORD_INTEN(120);
    for (i = 0; i < MAX_DEBRIS; i++) {
        if (debris[i].ttl > 0) {
            list[n++] = TUBE_WORD(TUBE_OP_POINT,
                                  (uint32_t)(debris[i].x / 256) & 0xFFFu,
                                  (uint32_t)(debris[i].y / 256) & 0xFFFu);
        }
    }
    list[n++] = TUBE_WORD_INTEN(255);

    if (alive && !(invuln > 0 && (frame & 2))) {
        draw_ship(&n, px, py, pangle, thrusting && (frame & 1));
    }
    if (game_over) {
        vfont_text(list, &n, 1500, 2200, 20, "GAME OVER");
    }

    list[n++] = TUBE_WORD(TUBE_OP_END, 0, 0);
    return n;
}

/* Returns 0 to keep running, 1 to quit. */
static int handle_key(const keyev_t *ev) {
    switch (ev->code) {
    case K_LEFT:  held_left = ev->down; break;
    case K_RIGHT: held_right = ev->down; break;
    case K_UP:    held_up = ev->down; break;
    case ' ':
        if (ev->down && !held_fire) {
            fire_latch = 1;
        }
        held_fire = ev->down;
        break;
    case 'h':
        if (ev->down) {
            hyper_latch = 1;
        }
        break;
    case 'q':
    case 'Q':
    case 27:
        if (ev->down) {
            return 1;
        }
        break;
    default:
        break;
    }
    return 0;
}

static int poll_keys(void) {
    keyev_t ev[16];
    int n, i;
    if (test_mode) {
        /* One injected event per frame: the keys file is a timeline. */
        n = tube_keys(ev, 4);
    } else {
        n = tube_keys(ev, (uint32_t)sizeof(ev));
    }
    for (i = 0; i < n; i++) {
        if (handle_key(&ev[i])) {
            return 1;
        }
    }
    return 0;
}

int main(int argc, char **argv) {
    unsigned frames_limit = 0;
    uint32_t seed = 0;
    int i;

    for (i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--frames") == 0 && i + 1 < argc) {
            frames_limit = (unsigned)atoi(argv[++i]);
            test_mode = 1;
        } else if (strcmp(argv[i], "--seed") == 0 && i + 1 < argc) {
            seed = (uint32_t)atoi(argv[++i]);
        } else {
            printf("usage: asteroids [--frames N --seed S]\n");
            return 1;
        }
    }

    if (tube_init() != 0) {
        printf("no tube: attach a screen\n");
        return 1;
    }
    if (tube_open(TUBE_MODE_VEC) != 0) {
        printf("open-fail\n");
        tube_cleanup();
        return 1;
    }

    rng_state = seed ? seed : 0xA57E01D5u;
    lives = 3;
    wave = 1;
    next_life = 10000u;
    reset_ship();
    spawn_wave();

    if (!test_mode) {
        int spins = 0;
        printf("asteroids: arrows steer, space fires, h hyperspace, "
               "q on the CRT quits\n");
        while ((tube_info() & (1u << 8)) == 0 && spins++ < 150) {
            usleep(20000);
        }
    }

    for (;;) {
        if (poll_keys()) {
            break;
        }
        if (!game_over || test_mode) {
            step();
        }
        if (tube_present(list, build_frame(), frame) != 0) {
            printf("present-fail\n");
            break;
        }
        if (test_mode) {
            if (frame >= frames_limit) {
                break;
            }
        } else {
            usleep(33000);
        }
    }

    if (test_mode) {
        int nrocks = 0;
        for (i = 0; i < MAX_ROCKS; i++) {
            nrocks += rocks[i].alive;
        }
        printf("report frames=%u score=%u lives=%d wave=%d rocks=%d\n",
               frame, score, lives, wave, nrocks);
    }
    tube_cleanup();
    return 0;
}
