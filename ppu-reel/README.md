# ppu-reel — the ppu conformance reel

A demo that does not try to be a game. Fourteen fixed frames, each
exercising one corner of the tube's `ppu` mode (docs/TUBE.md §5), so
that the whole surface is proven — not just the parts a game would
happen to use. **This reel is what froze §5's bit layouts** (they were
provisional until it landed).

| Frame | Proves |
|---|---|
| 0 | `bg_color` alone (empty nametable, everything transparent) |
| 1 | opaque background tiles, cell addressing |
| 2 | nametable hflip / vflip / both, on an asymmetric glyph |
| 3 | all eight sub-palettes selecting distinct colors |
| 4 | scroll is modulo the world: values past the torus wrap |
| 5 | the wrap seam itself, mid-screen |
| 6 | the 128×128 nametable cap, wrapped on both axes |
| 7 | OAM priority (sprite 0 wins), the enable bit, tiles 512/1023 |
| 8 | sprites straddling all four edges; fully offscreen draws nothing |
| 9 | the sprite-alpha ramp: exact integer blend at 0/64/128/192/255 |
| 10 | alpha over alpha, and sprite-alpha × palette-alpha |
| 11 | pixel-0 transparency: bg through tiles, tiles through sprites; OAM flips |
| 12 | palette animation: same tables, new colors, host re-reads |
| 13 | all 128 sprites at once over a scrolled checkerboard |

## Verify

```bash
bash build.sh
bash tests/run-tests.sh
```

The test runs the reel under every available engine (`slow32`,
`slow32-fast`, `slow32-dbt`), requires the 14 frame hashes to be
bit-identical across engines and equal to `tests/golden/`, then runs
`tests/check-pixels.py`: ~80 pixels re-derived independently from the
spec text (composite order and the exact truncating-integer blend)
and asserted against the `.ppm` journal. Two oracles, one artifact.

## Watch

```bash
../tools/emulator/slow32-fast reel.s32x --show
# in another terminal
../tools/s32-crt-mac        # or ../tools/s32-crt/s32-crt
```

`--show` paces the frames for the glass; it does not change their
content.

## Changing anything

The reel is the freeze. If a §5 semantic must change, change the spec
text, the compositor, `src/reel.c`, and `tests/check-pixels.py`
together, regenerate `tests/golden/` deliberately (delete it and run
the tests once), and say so in the commit message.
