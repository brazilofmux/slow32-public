# The Tube Service — Specification v0.2

Wire-level spec for the `tube` graphics service. The product plan and
the constitutional amendment live in
[docs/plans/tube.md](plans/tube.md); this document is the layer below:
opcodes, memory formats, the viewer socket, and the test contract.

Status: **spec only.** Nothing here is implemented. Each mode is built
when its flagship program demands it (vec: Tempest/Asteroids;
fb: a Doom port; ppu: unnamed). Mode 4 (`gpu`) has a number and no
spec, deliberately. `ppu` bit layouts in §5 are **provisional** and
freeze when that flagship has a name.

v0.2 pins the numbers an implementer could not invent: mode ids,
display-list bitfields, errno, the guest-memory walk, `PRESENT`
generation vs data-buffer `offset`, viewer framing, and the hash
grammar. It also records the attach-on-opcode rule, the host-written
port file, and that double-buffering is a guest convenience.

## 1. Negotiation and session

`tube` is a negotiated service, exactly like `term`:

- Guest sends `SVC_REQUEST` (0xF0) with name `"tube"` (NUL-terminated)
  in the data buffer, same shape as `term_init`: `length` includes the
  NUL, `offset` is the buffer index, `status` is 0. The guest does not
  send a version; the host grants version 1 in the reply.
- Host checks policy (`--deny tube` is a legal answer; programs
  degrade — Tempest prints "no tube: attach a screen" and exits the
  way BROWSE falls back to LIST).
- On `SVC_OK`, the 16-byte reply blob is `[result, base_opcode,
  opcode_count, version]` as `u32`s. `opcode_count` is 16; the tube
  owns `base+0 .. base+15`.
- One tube session per guest, one active mode per session (v1).
  `CLOSE` then `OPEN` of the same or another implemented mode is
  legal. Session cleanup on `SVC_RELEASE` or guest exit closes the
  viewer socket, unlinks the port file, and frees all snapshots.
- `term` and `tube` may both be granted. They do not share input
  (see §2).

### The synchronous-snapshot rule

Ring requests are synchronous: the guest CPU is paused while the host
services a descriptor. Therefore **`PRESENT` is an atomic snapshot
barrier by construction** — the host reads guest memory while the
guest cannot be mutating it. No locks, no generation races, no torn
frames, ever. The earlier free-running-reader design is rejected;
persistence and phosphor are simulated in the viewer from the
snapshot stream.

After `PRESENT` returns, the host has its own segment set or RGBA
buffer. The guest may smash that memory immediately. Double-buffering
lists in guest RAM is a convenience, not a host requirement; the
generation number is for the viewer to see dropped frames, not for
the host to handshake.

### Guest-memory walk

`PRESENT` (and `OPEN`, for the addresses it is given) reads guest RAM
through the emulator memory manager (`mm_read` / equivalent): the
region must exist and have `PROT_READ`. A flat `guest_mem_base +
addr` pointer is **not** the contract — `slow32` and `slow32-fast`
are sparse, and the code segment is execute-only. A display list or
framebuffer in `.text` fails the descriptor. Any unreadable word
fails the whole `PRESENT`; the host never publishes a partial
snapshot.

### Vocabulary

The emulator **composites** (deterministic pixel/segment math,
regression-testable). The viewer **renders** (glass, phosphor, glow).
The emulator never renders. The viewer never composites.

`vec` origin is lower-left, y up. `fb` and `ppu` are top-left, y
down, row-major. These are not unified.

## 2. Common opcodes

All descriptors use the standard ring shape
(`opcode, length, offset, status`) and the shared data buffer.
`length` is a **byte** count except where a row below says otherwise.
`offset` is a data-buffer index except on `PRESENT`, where it is the
guest-supplied generation and is **not** a data-buffer offset.

### Mode ids

| Id | Name | INFO bit |
|---|---|---|
| 1 | `vec` | bit 0 (`1 << 0`) |
| 2 | `fb`  | bit 1 (`1 << 1`) |
| 3 | `ppu` | bit 2 (`1 << 2`) |

`TUBE_OPEN` takes the id in `status`. `TUBE_INFO` reports implemented
modes as `1 << (id - 1)`. Id 4 (`gpu`) is reserved and is never set.
An `OPEN` of an unimplemented or unknown id fails.

### Opcode table

| Op | Name | Request | Response |
|---|---|---|---|
| base+0 | `TUBE_INFO` | `length`/`offset`/`status` unused | `status` packed (below); `length` = 0 |
| base+1 | `TUBE_OPEN` | `status` = mode id; param block at data[`offset`], `length` = block bytes | `status` = 0 or `ERR` |
| base+2 | `TUBE_CLOSE` | — | `status` = 0 (idempotent if not open) |
| base+3 | `TUBE_PRESENT` | mode-specific; **`offset` = generation**, not a buffer index | `status` = 0 or `ERR` |
| base+4 | `TUBE_STATUS` | — | `status` packed (below); `length` = 0 |
| base+5 | `TUBE_KEYS` | `length` = max **bytes** to copy (multiple of 4); `offset` = dest | `status` = event count copied; `length` = bytes copied |
| base+6..15 | reserved | | `ERR` / `EINVAL` |

`TUBE_INFO` `status`:

```
 2:0   implemented modes (bit (id-1))
 7:3   reserved 0
 8     viewer currently attached
15:9   reserved 0
31:16  service version (1)
```

`TUBE_STATUS` `status`:

```
23:0   frames successfully presented (wraps)
30:24  reserved 0
31     viewer currently attached
```

### Errors

Failures use the existing ring contract: `resp.status =
S32_MMIO_STATUS_ERR` (`0xFFFFFFFF`), `resp.length` = a positive
errno. Guests map that into `errno` the way every other opcode does.
Do not invent a parallel "0 or error" space.

| Condition | errno |
|---|---|
| `OPEN` of unknown / unimplemented mode | `EINVAL` |
| `OPEN` while a mode is already open | `EINVAL` |
| `OPEN` param block wrong size, bad width/height/format | `EINVAL` |
| `PRESENT` with no mode open | `EINVAL` |
| `PRESENT` list longer than 65536 words, or `END` missing inside `length` words | `EINVAL` |
| `PRESENT` / `OPEN` address unmapped or not `PROT_READ` | `EINVAL` |
| `PRESENT` unknown display-list op | `EINVAL` |
| `KEYS` `length` not a multiple of 4, or the copy will not fit the data buffer | `EINVAL` |
| reserved opcode | `EINVAL` |

Listen/bind failure for the viewer socket is **not** an `OPEN`
failure. Headless is the common case: `OPEN` succeeds, no
`tube.port` is written, `PRESENT` still journals.

### Input events (`TUBE_KEYS`)

Key make/break events flow viewer → emulator → queue → guest. Each
event is 4 bytes, packed, little-endian: `u16 code, u8 down, u8
reserved`. `TUBE_KEYS.length` is a byte budget, a multiple of 4 —
not an event count. `status` out is the number of events copied.
Leftover events stay queued. Queue cap: 256 events; overflow drops
oldest.

Codes: printable ASCII 32–126 as themselves; `ESC=27, ENTER=13,
TAB=9, BS=8`; extended keys from 0x100: `UP=0x100, DOWN, LEFT,
RIGHT, LSHIFT, RSHIFT, LCTRL, LALT, F1..F12` (0x108–0x113). Unknown
codes are still queued; the guest decides. `reserved` is 0 on write
and ignored on read.

`TUBE_KEYS` is the viewer queue only. `term_getkey` / `term_kbhit`
are host stdin. They are not merged. A guest that wants both has two
devices. `KEYS` is legal whenever the service is granted, including
before `OPEN`; without a viewer and without injection the queue is
empty and the copy returns count 0.

Injection, for tests that must not sit forever: `S32_TUBE_KEYS=<file>`
is a host file of the same 4-byte records, preloaded into the queue
at `OPEN` (then `KEYE` from a viewer appends). No viewer is required.

There is no vsync and no timer interrupt (this machine has no
interrupts). Guests pace with `GETTIME`/`SLEEP`. Those are host wall
clock and host `nanosleep` — they are **not** deterministic.
Whole-frame presentation only; the era's per-scanline tricks are out
of scope and out of character.

## 3. Mode 1: `vec` — the display list

Coordinate space: 0..4095 × 0..4095, origin **lower-left, y up**
(scope and plotter convention). The viewer letterboxes to a square.

The display list is an array of 32-bit little-endian words in guest
memory. Every word:

```
31:28  op
27:16  x          (MOVE / DRAW / POINT)
15:4   y          (MOVE / DRAW / POINT)
 3:0   reserved   ignore on read, write as 0

INTEN uses 7:0 as intensity; COLOR uses 23:0 as RGB888.
Bits the op does not name are ignored.
```

| Op | Bits | Meaning |
|---|---|---|
| 0x0 `END` | — | end of list |
| 0x1 `MOVE` | x, y | move beam, pen up |
| 0x2 `DRAW` | x, y | line from current position to (x, y) |
| 0x3 `POINT` | x, y | dot at (x, y); does not change the beam position |
| 0x4 `INTEN` | i: 7:0 | beam intensity 0–255 |
| 0x5 `COLOR` | rgb: 23:0 | RGB888 (monochrome viewers may ignore) |

Walk state at the start of every `PRESENT`: beam `(0, 0)`, intensity
`255`, color white (`0xFFFFFF`). `INTEN` / `COLOR` persist until
changed. `MOVE` / `DRAW` update the beam; `POINT` does not.

Linear lists only in v1: no JSR, no loops, no scale ops — if Tempest
genuinely pulls one of the DVG's fancier ops, it gets added here with
the flagship as justification. List cap: 65536 words. `PRESENT`
`length` is the **word** count (this is the exception to "length is
bytes"). The host walks at most `length` words and stops at the first
`END`. `length == 0`, `length > 65536`, or no `END` inside the window
fails the `PRESENT` (`EINVAL`), never truncates silently. An `END` as
the first word is a successful empty snapshot.

`PRESENT` for vec: `status` = list base (guest address, 4-byte
aligned; unaligned is `EINVAL`), `length` = word count, `offset` =
generation. The host walks the list once through the memory manager,
snapshots the resulting segment set, ships it to the viewer if one is
attached, and journals it for tests.

### Canonical test form

One UTF-8 (ASCII) line per emitted element, in list order, `\n`
(0x0A) terminated including the last line. No `\r`. Fields are
decimal integers, no `+` sign, no leading zeros except the number
`0` itself, separated by a single ASCII space:

```
M <x> <y>
D <x> <y> <r> <g> <b> <i>
P <x> <y> <r> <g> <b> <i>
```

`MOVE` does not carry color or intensity. `INTEN` / `COLOR` do not
emit lines; they fold into subsequent `D` / `P`. An empty list
(immediate `END`) is **zero bytes**, not a blank line. Golden tests
compare this exact byte string, or its FNV-1a 64 hash. The glow is
never tested.

## 4. Mode 2: `fb` — the framebuffer

Origin top-left, y down, row-major.

`TUBE_OPEN` param block (20 bytes at data[`offset`], `length` = 20):

```c
struct tube_fb_params {
    u32 width;      /* 320 mandatory-supported; host may allow <= 640 */
    u32 height;     /* 200 mandatory-supported; host may allow <= 480 */
    u32 format;     /* 1 = P8 (8bpp indexed), the only v1 format */
    u32 pix_base;   /* guest address, width*height bytes, row-major */
    u32 pal_base;   /* guest address, 256 x u32 0x00RRGGBB */
};
```

`PRESENT`: `offset` = generation; `length` and `status` unused. Host
snapshots pixels + palette through the memory manager, expands each
palette entry `0x00RRGGBB` to RGBA8 (`R, G, B, 255`) in that byte
order, ships the frame. Palette is re-read every present (palette
animation is free).

This is the honest framebuffer the plan amendment admits: the guest
owns every pixel, Doom renders into its own RAM and presents. The
known toll — key make/break input — is §2's event queue, specified
here because this mode is what demands it.

## 5. Mode 3: `ppu` — sprites and tiles

**Provisional.** The shape is the one we intend; bit layouts freeze
when a sprite-shaped flagship exists.

Everything lives in guest RAM. `TUBE_OPEN` `length` = 4, data buffer
holds the register-block guest address (4-byte aligned). `PRESENT`
snapshots the block and every table it points at. Composite output
is 320×200 RGBA8 (matches fb). Origin top-left, y down.

Register block: 16 × `u32`, little-endian. Words 0–11 are
`pattern_base, nametable_base, oam_base, palette_base, nt_w, nt_h,
scroll_x, scroll_y, bg_color, flags, reserved, reserved`. Words
12–15 reserved, write as 0, ignore on read. `nt_w` / `nt_h` in
tiles, max 128×128; scroll in pixels, wraps the nametable torus.
`bg_color` is `0xAARRGGBB`. `flags` unused in v1.

- **Tiles**: 8×8, 4bpp packed (two pixels per byte, high nibble
  left), 32 bytes per tile, up to 1024 tiles in the pattern table.
- **Nametable**: `u16` per cell:
  ```
   9:0   tile
  12:10  palette
  13     hflip
  14     vflip
  15     priority   reserved v1, write 0, ignore
  ```
- **Palettes**: 8 sub-palettes × 16 × `u32` `0xAARRGGBB`. Alpha is
  first-class. Pixel value 0 is transparent in sprites and in bg
  tiles (bg shows `bg_color` through it).
- **OAM**: 128 sprites × 8 bytes: `u16 x, u16 y` (top-left, may be
  offscreen), `u16` packed as a nametable cell, `u8 alpha`
  (whole-sprite multiplier), `u8 flags` (bit 0 enable, bit 1 16×16
  — a 2×2 tile block, reserved v1).

Composite order, per pixel: `bg_color` → background tile pixel →
sprites in **descending OAM index** (sprite 0 wins on top,
NES-style). Blend is src-over, 8-bit channels, integer divide
truncating toward zero:

```
a     = (sprite.alpha * palette.alpha) / 255
out_c = (src_c * a + dst_c * (255 - a)) / 255
out_a = a + dst_a * (255 - a) / 255
```

Pure function of the snapshot; golden-hashable.

Deferred until a flagship pulls them: second background layer, 16×16
sprites, priority bits, per-scanline anything (see §2: no interrupts,
no raster tricks).

## 6. The viewer socket

When policy allows, a mode is open, and listen succeeds, the
**emulator** (not the guest) binds `127.0.0.1:0` and writes a decimal
port number plus a single `\n` to `tube.port` in the emulator
process's cwd. This is a host-written port file, not the `bbs.port`
convention: `bbs.port` is the guest binding via `net` and writing
through `fs`. Two emulators in the same cwd clobber one file; a
leftover file from a crashed run may be stale — a viewer that cannot
connect treats that as "no tube." `CLOSE` and session cleanup unlink
the file. No `tube.port` is written if listen fails; `OPEN` still
succeeds.

One viewer at a time (v1). A second connect replaces the first: the
old socket is closed, the new one gets `HELO` plus the latest
snapshot.

Socket I/O — `accept`, `HELO`, send of the latest snapshot, `KEYE`
recv, `VSEG`/`VFRM` send, `BYE` — happens only inside `TUBE_OPEN`,
`TUBE_INFO`, `TUBE_PRESENT`, `TUBE_STATUS`, `TUBE_KEYS`,
`TUBE_CLOSE`, and session destroy. There is no extra `poll` in the
CPU loop. A viewer that attaches between presents waits until the
next one of those opcodes; it does not get the snapshot "immediately"
in wall time. The emulator never blocks on the viewer. Sends are
non-blocking; a slow viewer gets the latest frame, not a backlog
(oldest-first drop; generation numbers make the gaps visible).

### Framing

Little-endian on the wire:

```
u32 length     /* bytes of (tag + payload); does not include this word */
u32 tag        /* fourCC, bytes on the wire in name order */
u8  payload[length - 4]
```

Total frame size is `4 + length`. FourCCs are ASCII, right-NUL-padded
if short, written on the wire as those bytes (so a dump reads
`HELO`, `VSEG`, …):

| Tag bytes | LE `u32` | Direction | Payload |
|---|---|---|---|
| `HELO` | `0x4F4C4548` | emu → viewer | `u32 version, u32 mode, u32 w, u32 h` |
| `VSEG` | `0x47455356` | emu → viewer | `u32 generation, u32 count`, then `count` packed segments |
| `VFRM` | `0x4D524656` | emu → viewer | `u32 generation, u32 w, u32 h`, then `w*h*4` RGBA8 (`R,G,B,A`) |
| `KEYE` | `0x4559454B` | viewer → emu | `u16 code, u8 down, u8 reserved` |
| `BYE\0` | `0x00455942` | either | none |

`HELO.version` is 1. `HELO.mode` is the mode id (1/2/3). For `vec`,
`w = h = 4096`.

`VSEG` segment is 12 bytes, packed, no pad:

```
u16 x0, y0, x1, y1;
u8  r, g, b, i;
```

Points have `x0==x1` and `y0==y1`. `DRAW` of a zero-length segment
is a point via `D` in the dump and a point-shaped `VSEG` record.

Viewer absence is invisible to the guest: `PRESENT` succeeds, frames
drop, the dump still journals. Viewer attach/detach mid-run is legal.

## 7. Test contract

`S32_TUBE_DUMP=<dir>` makes the emulator journal every successful
`PRESENT` with no viewer needed. Frame `n` (from 0) writes
`<dir>/NNNNNN.hash` where `NNNNNN` is `n` as six decimal digits,
zero-padded. File contents: 16 lowercase hex digits of FNV-1a 64
and a single trailing `\n`.

FNV-1a 64: offset basis `14695981039346656037`
(`0xcbf29ce484222325`), prime `1099511628211` (`0x100000001b3`),
hash the canonical bytes — §3 text for `vec`, the raw RGBA8 buffer
for `fb`/`ppu`. `S32_TUBE_DUMP_FULL=1` also writes `NNNNNN.txt`
(vec) or `NNNNNN.ppm` (fb/ppu).

Regression is: run guest, compare hashes. Deterministic by the
synchronous-snapshot rule **and** by the guest presenting a fixed
sequence and exiting. A live game loop paced on `GETTIME`/`SLEEP` is
not golden-testable until a virtual clock exists; do not write that
test. The first prove-out is a present-once guest, not Asteroids.

`S32_TUBE_KEYS` (see §2) is how a scripted guest sees make/break
without a viewer.

Tube cases run on the C emulators (`slow32`, `slow32-fast`,
`slow32-dbt`) that share `mmio_ring.c`. `run-differential.sh` skips
them until QEMU implements this spec's guest surface. Selfhost
copies stay blind (see §8).

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
constraint is the aesthetic. A host `poll` in the CPU inner loop:
how you accidentally spend a week on a 2% DSB mystery. Virtual time:
later, when a flagship actually needs a golden game loop.
