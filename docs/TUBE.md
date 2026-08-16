# The Tube Service — Specification v0.1

Wire-level spec for the `tube` graphics service. The product plan and
the constitutional amendment live in
[docs/plans/tube.md](plans/tube.md); this document is the layer below:
opcodes, memory formats, the viewer socket, and the test contract.

Status: **spec only.** Nothing here is implemented. Each mode is built
when its flagship program demands it (vec: Tempest/Asteroids;
fb: a Doom port; ppu: unnamed). Mode 4 (`gpu`) has a number and no
spec, deliberately.

## 1. Negotiation and session

`tube` is a negotiated service, exactly like `term`:

- Guest sends `SVC_REQUEST` (0xF0) with name `"tube"`, version 1.
- Host checks policy (`--deny tube` is a legal answer; programs
  degrade — Tempest prints "no tube: attach a screen" and exits the
  way BROWSE falls back to LIST).
- On `SVC_OK`, the response carries `base_opcode`; the tube owns
  `base+0 .. base+15`.
- One tube session per guest, one active mode per session (v1).
  Session cleanup on guest exit closes the viewer socket and frees
  all snapshots.

### The synchronous-snapshot rule

Ring requests are synchronous: the guest CPU is paused while the host
services a descriptor. Therefore **`PRESENT` is an atomic snapshot
barrier by construction** — the host reads guest memory while the
guest cannot be mutating it. No locks, no generation races, no torn
frames, ever. The earlier free-running-reader design is rejected;
persistence and phosphor are simulated in the viewer from the
snapshot stream.

### Vocabulary

The emulator **composites** (deterministic pixel/segment math,
regression-testable). The viewer **renders** (glass, phosphor, glow).
The emulator never renders. The viewer never composites.

## 2. Common opcodes

All descriptors use the standard ring shape
(`opcode, length, offset, status`) and the shared data buffer.

| Op | Name | Request | Response |
|---|---|---|---|
| base+0 | `TUBE_INFO` | — | status = modes bitmask (bit0 vec, bit1 fb, bit2 ppu) \| viewer-attached << 8 \| version << 16 |
| base+1 | `TUBE_OPEN` | mode id in status; mode-specific param block in data buffer | 0 or error |
| base+2 | `TUBE_CLOSE` | — | 0 |
| base+3 | `TUBE_PRESENT` | mode-specific (see §3–§5); offset = generation | 0 or error |
| base+4 | `TUBE_STATUS` | — | frames presented (low 24) \| viewer attached (bit 31) |
| base+5 | `TUBE_KEYS` | length = max events | events copied to data buffer; status = count |
| base+6..15 | reserved | | |

### Input events (`TUBE_KEYS`)

Key make/break events flow viewer → emulator → queue → guest. Each
event is 4 bytes: `u16 code, u8 down, u8 reserved`. Codes: printable
ASCII 32–126 as themselves; `ESC=27, ENTER=13, TAB=9, BS=8`; extended
keys from 0x100: `UP=0x100, DOWN, LEFT, RIGHT, LSHIFT, RSHIFT, LCTRL,
LALT, F1..F12` (0x108–0x113). With no viewer attached the queue is
simply empty — cooked stdin via the ordinary stdio path remains
available and unrelated.

There is no vsync and no timer interrupt (this machine has no
interrupts). Guests pace with `GETTIME`/`SLEEP`. Whole-frame
presentation only; the era's per-scanline tricks are out of scope and
out of character.

## 3. Mode 1: `vec` — the display list

Coordinate space: 0..4095 × 0..4095, origin **lower-left, y up**
(scope and plotter convention). The viewer letterboxes to a square.

The display list is an array of 32-bit little-endian words in guest
memory. Top 4 bits are the op:

| Op | Word layout (low bits) | Meaning |
|---|---|---|
| 0x0 `END` | — | end of list |
| 0x1 `MOVE` | x:12, y:12 | move beam, pen up |
| 0x2 `DRAW` | x:12, y:12 | line from current position |
| 0x3 `POINT` | x:12, y:12 | dot |
| 0x4 `INTEN` | i:8 | beam intensity 0–255 (default 255) |
| 0x5 `COLOR` | rgb:24 | RGB888 (default white; monochrome viewers may ignore) |

Linear lists only in v1: no JSR, no loops, no scale ops — if Tempest
genuinely pulls one of the DVG's fancier ops, it gets added here with
the flagship as justification. List cap: 65536 words; exceeding it or
missing `END` fails the `PRESENT` (status = error), never truncates
silently.

`PRESENT` for vec: `status = list base (guest address)`,
`length = word count`, `offset = generation`. The host walks the list
once, snapshots the resulting segment set, ships it to the viewer,
and journals it for tests.

Canonical test form: one line per element,
`M x y` / `D x y r g b i` / `P x y r g b i`, in list order. Golden
tests compare this text (or its FNV-1a 64 hash). The glow is never
tested.

## 4. Mode 2: `fb` — the framebuffer

`TUBE_OPEN` param block:

```c
struct tube_fb_params {
    u32 width;      /* 320 mandatory-supported; host may allow <= 640 */
    u32 height;     /* 200 mandatory-supported; host may allow <= 480 */
    u32 format;     /* 1 = P8 (8bpp indexed), the only v1 format */
    u32 pix_base;   /* guest address, width*height bytes, row-major */
    u32 pal_base;   /* guest address, 256 x u32 0x00RRGGBB */
};
```

`PRESENT`: host snapshots pixels + palette, expands to RGBA8, ships
the frame. Palette is re-read every present (palette animation is
free). `offset = generation`.

This is the honest framebuffer the plan amendment admits: the guest
owns every pixel, Doom renders into its own RAM and presents. The
known toll — key make/break input — is §2's event queue, specified
here because this mode is what demands it.

## 5. Mode 3: `ppu` — sprites and tiles

Everything lives in guest RAM; `TUBE_OPEN` passes one register block
address and `PRESENT` snapshots the block and every table it points
at. Composite output is 320×200 RGBA8 (matches fb).

Register block (u32 each): `pattern_base, nametable_base, oam_base,
palette_base, nt_w, nt_h, scroll_x, scroll_y, bg_color, flags`,
rest reserved. `nt_w`/`nt_h` in tiles, max 128×128; scroll in pixels,
wraps the nametable torus.

- **Tiles**: 8×8, 4bpp packed (two pixels per byte, high nibble
  left), 32 bytes per tile, up to 1024 tiles in the pattern table.
- **Nametable**: u16 per cell: `tile:10 | palette:3 | hflip | vflip |
  priority` (priority reserved v1).
- **Palettes**: 8 sub-palettes × 16 × u32 RGBA8888. Alpha is
  first-class. Pixel value 0 is transparent in sprites and in bg
  tiles (bg shows `bg_color` through it).
- **OAM**: 128 sprites × 8 bytes: `u16 x, u16 y` (top-left, may be
  offscreen), `u16 tile|palette|flips` (same packing as nametable),
  `u8 alpha` (whole-sprite multiplier), `u8 flags` (bit0 enable,
  bit1 16×16 — a 2×2 tile block, reserved v1).

Composite order, per pixel: `bg_color` → background tile pixel →
sprites in **descending OAM index** (sprite 0 wins on top,
NES-style), each src-over blended with
`a = sprite.alpha × palette.alpha`. Pure function of the snapshot;
golden-hashable.

Deferred until a flagship pulls them: second background layer, 16×16
sprites, priority bits, per-scanline anything (see §2: no interrupts,
no raster tricks).

## 6. The viewer socket

When policy allows and a mode opens, the emulator listens on
`127.0.0.1:0` and writes `tube.port` (the `bbs.port` convention). One
viewer at a time (v1). The viewer — `s32-crt` or anything else that
speaks the protocol — attaches, receives the latest snapshot
immediately, then a message per `PRESENT`.

Framing: `u32 length, u32 tag, payload`, little-endian.

| Tag | Direction | Payload |
|---|---|---|
| `HELO` | emu → viewer | u32 version, u32 mode, u32 w, u32 h |
| `VSEG` | emu → viewer | u32 generation, u32 count, count × segment (`u16 x0,y0,x1,y1, u8 r,g,b,i`; points have x0==x1, y0==y1) |
| `VFRM` | emu → viewer | u32 generation, u32 w, u32 h, w×h×4 RGBA8 |
| `KEYE` | viewer → emu | u16 code, u8 down, u8 reserved |
| `BYE`  | either | — |

Viewer absence is invisible to the guest: `PRESENT` succeeds, frames
drop. Viewer attach/detach mid-run is legal. The emulator never
blocks on the viewer; a slow viewer gets the latest frame, not a
backlog (frames are dropped oldest-first, generation numbers make
the gaps visible).

## 7. Test contract

`S32_TUBE_DUMP=<dir>` makes the emulator journal every `PRESENT`
with no viewer needed: `NNNNNN.hash` (FNV-1a 64 over the canonical
form — §3 text for vec, RGBA buffer for fb/ppu), plus `.txt`/`.ppm`
alongside when `S32_TUBE_DUMP_FULL=1`. Regression is: run guest,
compare hashes. Deterministic by the synchronous-snapshot rule.

## 8. Selfhost and fleet carry

The guest-visible surface is ring opcodes and guest-memory formats —
nothing else. Emulators link no display library; the viewer is a
separate process. Per the hose.md precedent, selfhost copies of the
MMIO ring grow these opcodes only when those emulators need them,
possibly never. QEMU's tube, if ever, implements this spec's guest
surface; the socket side is emulator-internal and unconstrained.

## 9. Explicitly out of scope

Mode 4 (`gpu`): number reserved, no spec, warning label in
plans/tube.md. Mouse: has not earned its way in. Audio: a different
document, if ever. Vsync/interrupts: this machine has none, and that
constraint is the aesthetic.
