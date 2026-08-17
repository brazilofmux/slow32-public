# The tube

How SLOW-32 shows pictures, and how it does not.

Wire-level spec: [docs/TUBE.md](../TUBE.md) (v0.2: opcodes, memory
formats, viewer socket, test contract — numbers pinned).

Captured 2026-08-16. The hose carries bytes; the tube shows pictures.
Sibling document to [hose.md](hose.md), same rules: the host composes
machines, guests see services, absence is a legal answer.

## The amendment

The 1987-desk thesis said "if it needs a framebuffer, it is a
different hobby." That line is hereby amended, out loud, by the
person who wrote it. The new boundary is:

**No GPU. No 3D pipeline. No GL marshalling. A mouse still has to
earn its way in.**

A framebuffer is a 1987 object — every EGA card was one — and
pretending a blit-from-guest-memory isn't a framebuffer was the
dishonest version of this feature. Better one declared mode than a
loophole.

## Three modes

One negotiated service (`tube`, beside `term` / `net` / `exec`),
three modes. Each mode is a real 1980s display architecture with a
proper name, and each gets built only when its flagship program
demands it. Interfaces first, games later is the open-loop trap;
the game pulls the ops.

### 1. `vec` — the display list

The Atari DVG, rediscovered. A display list in guest memory; the
host interprets it as line segments and hands them to the viewer.
`PRESENT` is the VG taking the list: a synchronous snapshot, after
which the guest may smash that memory. Double-buffering two lists
is a guest convenience, not a host handshake. The generation number
is for the viewer to see dropped frames.

Phosphor decay, beam intensity, refresh flicker: viewer-side
aesthetics, never protocol. Tests assert the *interpretation* — feed
a list, check the segment set — and never the glow.

Flagship: **Tempest**, or Asteroids if the week is short. Also the
screen the plotter thread has been circling: Tektronix 4014 output,
and a live target for the sheet's `/Graph`.

### 2. `fb` — the framebuffer

Just a frame store. 320x200x8 with a palette to start; the guest
owns the pixels, the host scans them out. The whole interface is
"here is the base address, here is the palette, present."

Flagship: **a Doom port** — the final boss of "does your C compiler
actually work." Known toll: Doom wants key make/break events, not
cooked characters, so this mode pulls an input upgrade along with
it. That cost is part of the mode, not a surprise.

### 3. `ppu` — sprites and tiles

The console model, retained: tile patterns, a nametable, OAM,
scroll registers, all tables in guest RAM that the host composites
at frame rate. Sprites are not an add-on here; they are the point.
Radically smaller to implement and carry than any immediate-mode
command surface.

Alpha is first-class (one blend op). The ROP zoo is demoted: the
ROP3 byte *is* its truth table, so if generic ROPs are ever wanted
they are one bit-parallel inner loop plus a handful of specialized
hot cases — never 256 loops. The pattern operand is a printing-ism
and stays in printing.

Flagship: something sprite-shaped. Named when it exists.

GDI — if it ever exists — is a guest-side library over `fb`/`ppu`,
the way `term.h` sits over the term service. It is not host surface.

## The split that keeps the fleet honest

**The emulator never renders.** No SDL, no Cocoa, no window handle,
ever. The emulator's whole job is transport: it exposes the display
list or composited frames on a socket, and the viewer — `s32-crt`,
a separate host process, the actual piece of glass you attach — does
the glowing. The phosphor thread lives in the viewer.

What this buys:

- slow32 / slow32-fast / slow32-dbt stay pure POSIX and stay
  compilable by the stage08 cross-compiler. The guest-visible
  surface is just more ring opcodes.
- The headless fleet (Chromebooks, kagura, containers) keeps
  validating everything: `fb`/`ppu` composites are deterministic,
  so regression is golden-frame hashes with no display attached.
  `vec` regression is segment-list assertions.
- The viewer can be as fancy as it likes — SDL, Metal, a web page —
  with zero carry, because nothing else links it.

## Selfhost carry

`tube` is optional and negotiated; absence is a legal answer and
programs degrade (no tube means Tempest tells you to attach a
screen, the way no term means LIST instead of BROWSE). Per the
hose.md precedent, selfhost copies of the MMIO ring grow these
opcodes only when those emulators need them — possibly never. The
stage08-built emulator staying blind forever is policy, not debt.

## Order

`vec` first: smallest surface, period-purest, zero test ambiguity,
and it gives Tempest, the 4014, and the sheet chart one screen.
**Dump path landed** (negotiate / OPEN / PRESENT / `S32_TUBE_DUMP`).
**Viewer socket landed** (`s32-crt` is the glass; the emulator still
does not render). No flagship yet. Then `fb` when someone is actually
willing to carry Doom. `ppu` when its flagship has a name.

## What not to do

- Mode 4 (`gpu`) is hereby assigned its number and nothing else.
  Marshalling a GL/GPU surface through the ring is a full-time job
  wearing a feature's clothes — ye gods. If the desk ever gets that
  crazy, the number is reserved and this paragraph is the warning
  label on the box.
- No immediate-mode drawing API in the host surface. Retained
  tables and dumb stores only; command-stream graphics is what the
  guest-side library is for.
- No rendering in the emulator, even "just for debugging." That is
  how the fleet dies.
