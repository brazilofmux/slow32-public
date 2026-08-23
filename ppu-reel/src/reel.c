/* reel - the ppu conformance reel (docs/TUBE.md #5).
 *
 * A demo that does not try to be a game: fourteen fixed frames, each
 * exercising one corner of the ppu — flips, all eight sub-palettes,
 * scroll wrap on both torus axes, the 128x128 nametable cap, OAM
 * priority, edge-straddling sprites, the alpha formula, pixel-0
 * transparency, palette animation, and all 128 sprites at once.
 *
 * Every frame is deterministic. tests/run-tests.sh golden-hashes the
 * S32_TUBE_DUMP journal across engines and tests/check-pixels.py
 * re-derives ~50 pixels from the spec text and asserts them against
 * the .ppm dumps. This reel is what freezes the provisional bit
 * layouts in TUBE.md #5.
 *
 * Run with --show to pace the frames for a glass (s32-crt/-mac).
 */

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <tube.h>

#define MAX_NT 128

static uint8_t pattern[1024 * 32];
static uint16_t nt[MAX_NT * MAX_NT];
static uint32_t palettes[8][16];
static uint8_t oam[128][8];
static uint32_t regs[16];

static int show = 0;
static uint32_t frame = 0;

/* ------------------------------------------------------------------ */
/* building blocks                                                     */

static void set_pixel(int tile, int x, int y, int v) {
    uint8_t *b = &pattern[tile * 32 + y * 4 + x / 2];
    if (x & 1) {
        *b = (uint8_t)((*b & 0xF0) | (v & 0xF));
    } else {
        *b = (uint8_t)((*b & 0x0F) | ((v & 0xF) << 4));
    }
}

static void solid_tile(int tile, int v) {
    int x, y;
    for (y = 0; y < 8; y++)
        for (x = 0; x < 8; x++)
            set_pixel(tile, x, y, v);
}

/* The tile catalog. Tile 0 stays all-zero: the transparent tile.
 * t4 is the glyph: asymmetric on both axes so every flip is distinct.
 * t5 has a 2x2 pixel-0 hole. t512/t1023 prove the pattern-table range. */
static void build_tiles(void) {
    int i;
    memset(pattern, 0, sizeof(pattern));
    solid_tile(1, 1);       /* red (pal0)     */
    solid_tile(2, 2);       /* green          */
    solid_tile(3, 3);       /* blue           */
    /* t4 glyph "F": stem x=0 y=0..6, top bar y=0 x=0..4, mid y=3 x=0..3 */
    for (i = 0; i <= 6; i++) set_pixel(4, 0, i, 15);
    for (i = 0; i <= 4; i++) set_pixel(4, i, 0, 15);
    for (i = 0; i <= 3; i++) set_pixel(4, i, 3, 15);
    solid_tile(5, 5);       /* magenta, then punch the hole */
    set_pixel(5, 3, 3, 0); set_pixel(5, 4, 3, 0);
    set_pixel(5, 3, 4, 0); set_pixel(5, 4, 4, 0);
    solid_tile(6, 9);       /* the hue index for the sub-palette scene */
    solid_tile(512, 6);     /* cyan  — middle of the pattern table    */
    solid_tile(1023, 15);   /* white — last tile in the pattern table */
}

/* Sub-palette p, index 9 is that palette's hue; pal 0 holds the
 * opaque primaries; pal 7 holds the translucent entries. All values
 * are mirrored in tests/check-pixels.py — change both or neither. */
static const uint32_t HUE[8] = {
    0xFFFF0000u, 0xFF00FF00u, 0xFF0000FFu, 0xFFFFFF00u,
    0xFFFF00FFu, 0xFF00FFFFu, 0xFFFFFFFFu, 0xFFFF8000u,
};

static void build_palettes(void) {
    int p, i;
    memset(palettes, 0, sizeof(palettes));
    for (p = 0; p < 8; p++) {
        uint32_t hr = (HUE[p] >> 16) & 0xFFu;
        uint32_t hg = (HUE[p] >> 8) & 0xFFu;
        uint32_t hb = HUE[p] & 0xFFu;
        for (i = 1; i < 16; i++) {
            /* brightness ramp of the hue: s = 120 + 9i (i=15 -> 255) */
            uint32_t s = 120u + 9u * (uint32_t)i;
            palettes[p][i] = 0xFF000000u | ((hr * s / 255u) << 16) |
                             ((hg * s / 255u) << 8) | (hb * s / 255u);
        }
        palettes[p][9] = HUE[p];
        palettes[p][15] = 0xFFFFFFFFu;
    }
    palettes[0][1] = 0xFFFF0000u;   /* red     */
    palettes[0][2] = 0xFF00FF00u;   /* green   */
    palettes[0][3] = 0xFF0000FFu;   /* blue    */
    palettes[0][5] = 0xFFFF00FFu;   /* magenta */
    palettes[0][6] = 0xFF00FFFFu;   /* cyan    */
    palettes[0][7] = 0xFFFFFF00u;   /* yellow  */
    palettes[7][1] = 0x80FF0000u;   /* half-alpha red      */
    palettes[7][2] = 0x8000FF00u;   /* half-alpha green    */
    palettes[7][3] = 0x400000FFu;   /* quarter-alpha blue  */
}

static void clear_nt(void) {
    memset(nt, 0, sizeof(nt));
}

static void cell(int cx, int cy, int tile, int pal, int hf, int vf) {
    nt[cy * (int)regs[4] + cx] =
        (uint16_t)((tile & 0x3FF) | ((pal & 7) << 10) |
                   (hf ? 0x2000 : 0) | (vf ? 0x4000 : 0));
}

static void clear_sprites(void) {
    memset(oam, 0, sizeof(oam));
}

static void sprite(int i, int x, int y, int tile, int pal,
                   int hf, int vf, int alpha, int en) {
    oam[i][0] = (uint8_t)(x & 0xFF);
    oam[i][1] = (uint8_t)((x >> 8) & 0xFF);
    oam[i][2] = (uint8_t)(y & 0xFF);
    oam[i][3] = (uint8_t)((y >> 8) & 0xFF);
    oam[i][4] = (uint8_t)(tile & 0xFF);
    oam[i][5] = (uint8_t)(((tile >> 8) & 3) | ((pal & 7) << 2) |
                          (hf ? 0x20 : 0) | (vf ? 0x40 : 0));
    oam[i][6] = (uint8_t)alpha;
    oam[i][7] = (uint8_t)(en ? 1 : 0);
}

/* nt size + scroll + bg for a scene (nt content set by the scene) */
static void view(uint32_t w, uint32_t h, uint32_t sx, uint32_t sy,
                 uint32_t bg) {
    regs[4] = w;
    regs[5] = h;
    regs[6] = sx;
    regs[7] = sy;
    regs[8] = bg;
}

static void present(void) {
    if (tube_flip(++frame) != 0) {
        printf("present failed at frame %u\n", (unsigned)(frame - 1));
    }
    if (show)
        usleep(800000);
}

/* ------------------------------------------------------------------ */
/* the reel                                                            */

int main(int argc, char *argv[]) {
    if (argc > 1 && strcmp(argv[1], "--show") == 0)
        show = 1;

    build_tiles();
    build_palettes();
    clear_nt();
    clear_sprites();

    memset(regs, 0, sizeof(regs));
    regs[0] = (uint32_t)(uintptr_t)pattern;
    regs[1] = (uint32_t)(uintptr_t)nt;
    regs[2] = (uint32_t)(uintptr_t)oam;
    regs[3] = (uint32_t)(uintptr_t)palettes;
    view(40, 25, 0, 0, 0xFF400080u);

    if (tube_init() != 0) {
        printf("no tube: attach a screen\n");
        return 1;
    }
    if (tube_open_ppu(regs) != 0) {
        printf("ppu open failed\n");
        return 1;
    }
    if (show) {
        int spins = 0;
        while ((tube_info() & (1u << 8)) == 0 && spins++ < 150)
            usleep(20000);
    }

    /* F0 — void: empty nametable, every pixel is bg_color */
    present();

    /* F1 — checker: opaque bg tiles, no scroll */
    {
        int cx, cy;
        view(40, 25, 0, 0, 0xFF102030u);
        for (cy = 0; cy < 25; cy++)
            for (cx = 0; cx < 40; cx++)
                cell(cx, cy, ((cx + cy) & 1) ? 2 : 1, 0, 0, 0);
        present();
    }

    /* F2 — glyph flips in the nametable: normal / H / V / HV */
    clear_nt();
    cell(2, 2, 4, 0, 0, 0);
    cell(4, 2, 4, 0, 1, 0);
    cell(2, 4, 4, 0, 0, 1);
    cell(4, 4, 4, 0, 1, 1);
    present();

    /* F3 — the same tile through all eight sub-palettes */
    {
        int p;
        clear_nt();
        for (p = 0; p < 8; p++)
            cell(1 + p, 1, 6, p, 0, 0);
        present();
    }

    /* F4 — scroll is modulo the world: values past the torus wrap */
    clear_nt();
    cell(0, 0, 1, 0, 0, 0);
    view(40, 25, 320u + 13u, 200u * 3u + 7u, 0xFF102030u);
    present();

    /* F5 — the seam itself, mid-screen: right column meets left */
    {
        int cy;
        clear_nt();
        for (cy = 0; cy < 25; cy++) {
            cell(0, cy, 2, 0, 0, 0);
            cell(39, cy, 3, 0, 0, 0);
        }
        view(40, 25, 160, 0, 0xFF102030u);
        present();
    }

    /* F6 — the 128x128 nametable cap, wrapped on both axes */
    {
        int c;
        view(128, 128, 900, 900, 0xFF102030u);
        clear_nt();
        for (c = 0; c < 128; c++)
            cell(c, c, 1, 0, 0, 0);
        present();
    }

    /* F7 — sprite priority (0 wins), the enable bit, tile range */
    clear_nt();
    view(40, 25, 0, 0, 0xFF102030u);
    sprite(0, 100, 100, 3, 0, 0, 0, 255, 1);
    sprite(1, 104, 100, 1, 0, 0, 0, 255, 1);
    sprite(2, 108, 100, 2, 0, 0, 0, 255, 1);
    sprite(3, 200, 50, 1, 0, 0, 0, 255, 0);    /* disabled: invisible */
    sprite(4, 30, 30, 512, 0, 0, 0, 255, 1);   /* mid pattern table  */
    sprite(5, 50, 30, 1023, 0, 0, 0, 255, 1);  /* last pattern slot  */
    present();

    /* F8 — sprites straddling every edge; fully offscreen draws nothing */
    clear_sprites();
    sprite(0, -4, -4, 1, 0, 0, 0, 255, 1);
    sprite(1, 316, -4, 1, 0, 0, 0, 255, 1);
    sprite(2, -4, 196, 1, 0, 0, 0, 255, 1);
    sprite(3, 316, 196, 1, 0, 0, 0, 255, 1);
    sprite(4, 320, 100, 1, 0, 0, 0, 255, 1);
    sprite(5, -8, 100, 1, 0, 0, 0, 255, 1);
    sprite(6, 100, 200, 1, 0, 0, 0, 255, 1);
    sprite(7, 100, -8, 1, 0, 0, 0, 255, 1);
    sprite(8, -32768, -32768, 1, 0, 0, 0, 255, 1);
    present();

    /* F9 — the sprite-alpha ramp over a known background */
    {
        static const int ramp[5] = { 0, 64, 128, 192, 255 };
        int i;
        clear_sprites();
        view(40, 25, 0, 0, 0xFF004080u);
        for (i = 0; i < 5; i++)
            sprite(i, 20 + i * 16, 60, 1, 0, 0, 0, ramp[i], 1);
        present();
    }

    /* F10 — alpha over alpha, and sprite-alpha times palette-alpha */
    clear_sprites();
    sprite(1, 150, 60, 2, 7, 0, 0, 255, 1);    /* half-alpha green   */
    sprite(0, 154, 60, 1, 7, 0, 0, 255, 1);    /* half-alpha red on top */
    sprite(2, 180, 60, 1, 7, 0, 0, 128, 1);    /* 128 x 128/255 combined */
    present();

    /* F11 — pixel-0 holes: bg shows through tiles, tiles through sprites */
    clear_sprites();
    clear_nt();
    cell(5, 5, 5, 0, 0, 0);                    /* magenta tile, 2x2 hole */
    sprite(0, 40, 40, 4, 0, 0, 0, 255, 1);     /* glyph over that tile   */
    sprite(1, 100, 140, 4, 0, 0, 1, 255, 1);   /* vflipped glyph via OAM */
    present();

    /* F12 — palette animation: same tables, new colors, host re-reads */
    palettes[0][5] = 0xFF00A0FFu;
    palettes[0][15] = 0xFFFFFF00u;
    present();

    /* F13 — the crowd: all 128 sprites over a scrolled checkerboard */
    {
        static const int tiles[5] = { 1, 2, 3, 512, 1023 };
        int i, cx, cy;
        build_palettes();                       /* undo F12 */
        clear_sprites();
        clear_nt();
        view(40, 25, 4, 4, 0xFF080808u);
        for (cy = 0; cy < 25; cy++)
            for (cx = 0; cx < 40; cx++)
                cell(cx, cy, ((cx + cy) & 1) ? 2 : 1, 0, 0, 0);
        for (i = 0; i < 128; i++)
            sprite(i, (i % 16) * 20 + 2, (i / 16) * 24 + 3,
                   tiles[i % 5], i & 7, i & 1, (i >> 1) & 1,
                   255 - (i & 3) * 40, 1);
        present();
    }

    if (show)
        usleep(2000000);
    tube_cleanup();
    printf("reel: %u frames\n", (unsigned)frame);
    return 0;
}
