#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include "tube.h"

#define VIEWER_BIT (1u << 8)
#define CX 2048
#define CY 2048

/* cos(i * 22.5°) * 256, 16 steps. sin(i) = C[(i + 12) & 15]. */
static const int16_t C16[16] = {
    256, 237, 181, 97, 0, -97, -181, -237,
    -256, -237, -181, -97, 0, 97, 181, 237
};

static int16_t c16(int a) { return C16[a & 15]; }
static int16_t s16(int a) { return C16[(a + 12) & 15]; }

static void rot(int a, int x, int y, uint32_t *ox, uint32_t *oy) {
    int nx = (x * (int)c16(a) - y * (int)s16(a)) / 256;
    int ny = (x * (int)s16(a) + y * (int)c16(a)) / 256;
    *ox = (uint32_t)(CX + nx);
    *oy = (uint32_t)(CY + ny);
}

static uint32_t emit(uint32_t *w, uint32_t n, uint32_t word) {
    w[n] = word;
    return n + 1;
}

static uint32_t build(uint32_t *w, int angle) {
    static const int ship[][2] = {
        { 0, 720 }, { -460, -560 }, { 0, -280 }, { 460, -560 }, { 0, 720 }
    };
    uint32_t n = 0;
    uint32_t x, y;
    int i;

    n = emit(w, n, TUBE_WORD_COLOR(0x4488FF));
    n = emit(w, n, TUBE_WORD_INTEN(80));
    n = emit(w, n, TUBE_WORD(TUBE_OP_MOVE, 400, 400));
    n = emit(w, n, TUBE_WORD(TUBE_OP_DRAW, 3695, 400));
    n = emit(w, n, TUBE_WORD(TUBE_OP_DRAW, 3695, 3695));
    n = emit(w, n, TUBE_WORD(TUBE_OP_DRAW, 400, 3695));
    n = emit(w, n, TUBE_WORD(TUBE_OP_DRAW, 400, 400));

    n = emit(w, n, TUBE_WORD_COLOR(0xFFFFFF));
    n = emit(w, n, TUBE_WORD_INTEN(255));
    rot(angle, ship[0][0], ship[0][1], &x, &y);
    n = emit(w, n, TUBE_WORD(TUBE_OP_MOVE, x, y));
    for (i = 1; i < 5; i++) {
        rot(angle, ship[i][0], ship[i][1], &x, &y);
        n = emit(w, n, TUBE_WORD(TUBE_OP_DRAW, x, y));
    }

    n = emit(w, n, TUBE_WORD(TUBE_OP_END, 0, 0));
    return n;
}

static int wait_viewer(void) {
    int spins = 0;
    while ((tube_info() & VIEWER_BIT) == 0) {
        if (++spins > 50) {
            return -1;
        }
        usleep(20000);
    }
    return 0;
}

int main(void) {
    static uint32_t list[32];
    typedef struct {
        uint16_t code;
        uint8_t down;
        uint8_t pad;
    } key_t;
    key_t ev[8];
    int angle = 0;
    uint32_t gen = 1;
    int done = 0;

    if (tube_init() != 0) {
        printf("no tube: attach a screen\n");
        return 1;
    }
    if (tube_open(TUBE_MODE_VEC) != 0) {
        printf("open-fail\n");
        return 1;
    }
    if (wait_viewer() != 0) {
        printf("no viewer — start s32-crt first\n");
        return 1;
    }
    printf("vecscope: q or ESC on the CRT to quit\n");

    while (!done) {
        uint32_t nw = build(list, angle);
        int n, i;
        if (tube_present(list, nw, gen++) != 0) {
            printf("present-fail\n");
            return 1;
        }
        n = tube_keys(ev, (uint32_t)sizeof(ev));
        for (i = 0; i < n; i++) {
            if (ev[i].down && (ev[i].code == 'q' || ev[i].code == 'Q' ||
                               ev[i].code == 27)) {
                done = 1;
            }
        }
        angle = (angle + 1) & 15;
        usleep(40000);
    }

    tube_cleanup();
    return 0;
}
