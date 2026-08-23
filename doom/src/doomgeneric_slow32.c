/* doomgeneric_slow32 - Doom on the SLOW-32 tube.
 *
 * The fb flagship. DG_ScreenBuffer (8bpp under CMAP256) is presented
 * straight down the tube; the palette rides beside it and is rebuilt
 * whenever the game changes it (gamma, item flashes, pain reddening).
 * Keys arrive as real make/break events from the glass.
 */

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <time.h>

#include <tube.h>

#include "doomgeneric.h"
#include "doomkeys.h"
#include "i_video.h"

static uint32_t pal[256];
static uint32_t gen;

extern boolean palette_changed;
extern struct color colors[256];

void DG_Init(void) {
    if (tube_init() != 0) {
        printf("no tube: attach a screen\n");
        exit(1);
    }
    if (tube_open_fb(DOOMGENERIC_RESX, DOOMGENERIC_RESY,
                     DG_ScreenBuffer, pal) != 0) {
        printf("tube fb open failed\n");
        exit(1);
    }
    {
        /* Give the glass a moment; run regardless (headless is legal). */
        int spins = 0;
        while ((tube_info() & (1u << 8)) == 0 && spins++ < 100) {
            usleep(20000);
        }
    }
}

void DG_DrawFrame(void) {
    if (palette_changed) {
        int i;
        for (i = 0; i < 256; i++) {
            pal[i] = ((uint32_t)colors[i].r << 16) |
                     ((uint32_t)colors[i].g << 8) |
                     (uint32_t)colors[i].b;
        }
        palette_changed = false;
    }
    tube_flip(++gen);
}

uint32_t DG_GetTicksMs(void) {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return (uint32_t)((uint64_t)ts.tv_sec * 1000u +
                      (uint64_t)ts.tv_nsec / 1000000u);
}

void DG_SleepMs(uint32_t ms) {
    usleep(ms * 1000u);
}

/* Map tube key codes onto Doom's. Arrows steer, ctrl fires, space
 * uses, shift runs, alt strafes — the 1993 hands. WASD also works. */
static unsigned char map_key(uint16_t code) {
    switch (code) {
        case 0x100: return KEY_UPARROW;
        case 0x101: return KEY_DOWNARROW;
        case 0x102: return KEY_LEFTARROW;
        case 0x103: return KEY_RIGHTARROW;
        case 0x104: case 0x105: return KEY_RSHIFT;
        case 0x106: return KEY_FIRE;
        case 0x107: return KEY_LALT;
        case 27:    return KEY_ESCAPE;
        case 13:    return KEY_ENTER;
        case 8:     return KEY_BACKSPACE;
        case ' ':   return KEY_USE;
        case 'w':   return KEY_UPARROW;
        case 's':   return KEY_DOWNARROW;
        case 'a':   return KEY_STRAFE_L;
        case 'd':   return KEY_STRAFE_R;
        default:
            if (code >= 32 && code < 127) {
                return (unsigned char)code;
            }
            return 0;
    }
}

int DG_GetKey(int *pressed, unsigned char *doomKey) {
    struct {
        uint16_t code;
        uint8_t down, pad;
    } ev;
    for (;;) {
        int n = tube_keys(&ev, 4);
        unsigned char k;
        if (n <= 0) {
            return 0;
        }
        k = map_key(ev.code);
        if (k) {
            *pressed = ev.down;
            *doomKey = k;
            return 1;
        }
    }
}

void DG_SetWindowTitle(const char *title) {
    (void)title;
}

int main(int argc, char **argv) {
    setvbuf(stdout, NULL, _IONBF, 0);   /* crash locations stay honest */
    doomgeneric_Create(argc, argv);
    for (;;) {
        doomgeneric_Tick();
    }
    return 0;
}
