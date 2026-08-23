#!/usr/bin/env python3
"""Spec-derived pixel assertions for the ppu conformance reel.

Reads the S32_TUBE_DUMP_FULL .ppm journal and asserts ~50 pixels whose
values are re-derived here, independently, from docs/TUBE.md #5: the
composite order (bg_color -> bg tile -> sprites, descending OAM index)
and the exact integer blend

    a     = (sprite_alpha * palette_alpha) / 255
    out_c = (src_c * a + dst_c * (255 - a)) / 255

with integer division truncating toward zero. Constants (palettes,
scene geometry) mirror src/reel.c — change both or neither.

Usage: check-pixels.py <dump-dir>
"""
import sys, os

# --- mirrors of src/reel.c ------------------------------------------

HUE = [0xFF0000, 0x00FF00, 0x0000FF, 0xFFFF00,
       0xFF00FF, 0x00FFFF, 0xFFFFFF, 0xFF8000]

def rgb(v):
    return ((v >> 16) & 0xFF, (v >> 8) & 0xFF, v & 0xFF)

def build_palettes():
    """palettes[p][i] = (r, g, b, alpha)"""
    pal = [[(0, 0, 0, 0)] * 16 for _ in range(8)]
    for p in range(8):
        hr, hg, hb = rgb(HUE[p])
        for i in range(1, 16):
            s = 120 + 9 * i
            pal[p][i] = (hr * s // 255, hg * s // 255, hb * s // 255, 255)
        pal[p][9] = (*rgb(HUE[p]), 255)
        pal[p][15] = (255, 255, 255, 255)
    pal[0][1] = (255, 0, 0, 255)
    pal[0][2] = (0, 255, 0, 255)
    pal[0][3] = (0, 0, 255, 255)
    pal[0][5] = (255, 0, 255, 255)
    pal[0][6] = (0, 255, 255, 255)
    pal[0][7] = (255, 255, 0, 255)
    pal[7][1] = (255, 0, 0, 128)
    pal[7][2] = (0, 255, 0, 128)
    pal[7][3] = (0, 0, 255, 64)
    return pal

PAL = build_palettes()

def over(dst, src, a):
    """The spec blend: src over dst at effective alpha a."""
    return tuple((s * a + d * (255 - a)) // 255 for s, d in zip(src, dst))

def spr(dst, p, i, salpha=255):
    """Sprite pixel of palette p index i over dst (spec formula)."""
    r, g, b, pa = PAL[p][i]
    return over(dst, (r, g, b), salpha * pa // 255)

BG_VOID = (0x40, 0x00, 0x80)
BG_MAIN = (0x10, 0x20, 0x30)
BG_BLUE = (0x00, 0x40, 0x80)
BG_DARK = (0x08, 0x08, 0x08)

RED     = spr(BG_MAIN, 0, 1)      # opaque: dst irrelevant
GREEN   = spr(BG_MAIN, 0, 2)
BLUE    = spr(BG_MAIN, 0, 3)
MAGENTA = spr(BG_MAIN, 0, 5)
CYAN    = spr(BG_MAIN, 0, 6)
WHITE   = spr(BG_MAIN, 0, 15)

# --- assertions: (frame, x, y, expected, label) ---------------------

A = []
def at(f, x, y, want, label):
    A.append((f, x, y, want, label))

# F0 void: every pixel is bg_color
for (x, y) in [(0, 0), (319, 0), (0, 199), (319, 199), (160, 100)]:
    at(0, x, y, BG_VOID, "void bg")

# F1 checker
at(1, 0, 0, RED, "checker (0,0)")
at(1, 8, 0, GREEN, "checker (8,0)")
at(1, 0, 8, GREEN, "checker (0,8)")
at(1, 8, 8, RED, "checker (8,8)")
at(1, 319, 199, GREEN, "checker far corner")

# F2 glyph flips (glyph pixels are idx 15 = white)
at(2, 16, 16, WHITE, "glyph normal stem")
at(2, 20, 16, WHITE, "glyph normal top bar end")
at(2, 21, 16, BG_MAIN, "glyph normal past bar")
at(2, 39, 16, WHITE, "glyph hflip top bar at right")
at(2, 34, 16, BG_MAIN, "glyph hflip left empty")
at(2, 16, 39, WHITE, "glyph vflip bar at bottom")
at(2, 16, 32, BG_MAIN, "glyph vflip top empty")
at(2, 39, 39, WHITE, "glyph hv bar bottom-right")
at(2, 32, 32, BG_MAIN, "glyph hv top-left empty")

# F3 sub-palettes: cell (1+p,1), center pixel = pure hue
for p in range(8):
    at(3, 8 * (1 + p) + 4, 12, rgb(HUE[p]), "sub-palette %d hue" % p)

# F4 scroll modulo: marker tile visible at wrapped position
at(4, 310, 195, RED, "scroll-mod marker")
at(4, 310, 0, RED, "scroll-mod marker y-wrap")
at(4, 310, 8, BG_MAIN, "scroll-mod off marker")
at(4, 300, 195, BG_MAIN, "scroll-mod left of marker")
at(4, 315, 195, BG_MAIN, "scroll-mod right of marker")

# F5 seam at mid-screen
at(5, 152, 50, BLUE, "seam right column")
at(5, 159, 50, BLUE, "seam last wrapped px")
at(5, 160, 50, GREEN, "seam left column")
at(5, 167, 50, GREEN, "seam left col end")
at(5, 168, 50, BG_MAIN, "seam past columns")
at(5, 151, 50, BG_MAIN, "seam before columns")

# F6 128x128 nametable, scroll 900/900
at(6, 0, 0, RED, "big-nt diagonal origin")
at(6, 10, 10, RED, "big-nt diagonal")
at(6, 130, 130, RED, "big-nt diagonal after wrap")
at(6, 130, 50, BG_MAIN, "big-nt off diagonal")

# F7 sprite priority / enable / tile range
at(7, 105, 103, BLUE, "priority: sprite 0 wins")
at(7, 109, 103, RED, "priority: sprite 1 over 2")
at(7, 114, 103, GREEN, "priority: sprite 2 alone")
at(7, 203, 53, BG_MAIN, "disabled sprite invisible")
at(7, 33, 33, CYAN, "tile 512")
at(7, 53, 33, WHITE, "tile 1023")

# F8 edge straddling
at(8, 3, 3, RED, "straddle top-left")
at(8, 4, 4, BG_MAIN, "straddle tl clip")
at(8, 319, 3, RED, "straddle top-right")
at(8, 315, 0, BG_MAIN, "straddle tr clip")
at(8, 3, 199, RED, "straddle bottom-left")
at(8, 316, 196, RED, "straddle bottom-right")
at(8, 319, 103, BG_MAIN, "fully offscreen right")
at(8, 0, 103, BG_MAIN, "fully offscreen left")
at(8, 103, 0, BG_MAIN, "fully offscreen top")
at(8, 103, 199, BG_MAIN, "fully offscreen bottom")

# F9 alpha ramp: red over BG_BLUE at salpha 0/64/128/192/255
for i, a in enumerate([0, 64, 128, 192, 255]):
    at(9, 23 + 16 * i, 63, spr(BG_BLUE, 0, 1, a), "alpha ramp %d" % a)

# F10 alpha over alpha (pal 7), and salpha x palette-alpha
g1 = spr(BG_BLUE, 7, 2)                 # half green over bg
at(10, 151, 63, g1, "half green alone")
at(10, 159, 63, spr(BG_BLUE, 7, 1), "half red alone")
at(10, 155, 63, spr(g1, 7, 1), "red over green stack")
at(10, 183, 63, spr(BG_BLUE, 7, 1, 128), "salpha*palalpha")

# F11 pixel-0 transparency (bg is still BG_BLUE here)
at(11, 43, 44, BG_BLUE, "tile hole shows bg")
at(11, 42, 42, MAGENTA, "tile solid part")
at(11, 40, 40, WHITE, "sprite glyph pixel")
at(11, 46, 46, MAGENTA, "sprite hole shows tile")
at(11, 100, 147, WHITE, "vflip sprite bar at bottom")
at(11, 100, 140, BG_BLUE, "vflip sprite top empty")

# F12 palette animation (same tables, colors changed)
at(12, 42, 42, (0, 160, 255), "animated tile color")
at(12, 40, 40, (255, 255, 0), "animated glyph color")
at(12, 43, 44, BG_BLUE, "hole still bg")

# F13 the crowd (bg checker scrolled by 4, pal restored)
at(13, 5, 6, RED, "crowd sprite 0")
under = spr(BG_DARK, 0, 1)              # checker t1 under sprite 127
at(13, 305, 174, spr(under, 7, 3, 135), "crowd sprite 127 blend")

# --- runner ---------------------------------------------------------

def read_ppm(path):
    data = open(path, 'rb').read()
    parts = data.split(b'\n', 3)
    assert parts[0] == b'P6' and parts[2] == b'255', "unexpected ppm"
    w, h = map(int, parts[1].split())
    return w, h, parts[3]

def main():
    if len(sys.argv) != 2:
        print("usage: check-pixels.py <dump-dir>")
        return 2
    d = sys.argv[1]
    frames = {}
    fails = 0
    for f, x, y, want, label in A:
        if f not in frames:
            path = os.path.join(d, "%06d.ppm" % f)
            if not os.path.exists(path):
                print("MISSING frame %d (%s)" % (f, path))
                return 2
            frames[f] = read_ppm(path)
        w, h, pix = frames[f]
        o = (y * w + x) * 3
        got = (pix[o], pix[o + 1], pix[o + 2])
        if got != tuple(want):
            print("FAIL f%d (%d,%d) %s: want %s got %s"
                  % (f, x, y, label, tuple(want), got))
            fails += 1
    print("%d/%d pixel assertions passed" % (len(A) - fails, len(A)))
    return 1 if fails else 0

if __name__ == '__main__':
    sys.exit(main())
