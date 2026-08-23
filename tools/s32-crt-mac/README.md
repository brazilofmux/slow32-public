# s32-crt-mac

The good glass. A native macOS viewer for the SLOW-32 tube — the
emulator still never renders; this is the peripheral that glows.

```bash
make
cd ../../asteroids
../tools/emulator/slow32-fast asteroids.s32x &
../tools/s32-crt-mac/s32-crt-mac          # or --green for P31
```

- **vec**: additive beam strokes (crossings brighten, like real
  phosphor and unlike toner), three-pass bloom, 60Hz persistence
  decay that keeps fading after the guest halts.
- **fb / ppu**: nearest-neighbor scaling with subtle scanlines
  (`--no-scanlines` to disable).
- **Keys**: real NSEvent make/break — held arrows are actually held;
  no terminal escape-sequence synthesis. Cmd+Q or close quits the
  glass; everything else goes to the guest.
- Attaches to `./tube.port` (or `--port N` / `--port-file PATH`),
  holds the last picture when the guest halts, and quietly reattaches
  when a new tube appears — leave it open, relaunch guests at will.

Flags: `--green` (P31 phosphor tint for vec), `--no-scanlines`.
