# SLOW-32 Asteroids

The vec flagship. A triangle with a gun, on the tube — the game that
justifies the display-list interface it runs on (docs/TUBE.md §3).

4096x4096 wrapped space, all integer math (positions in 1/256 space
units, a 64-step cosine table), so every engine computes the identical
frame — the test suite plays a scripted game on slow32, slow32-fast,
and slow32-dbt and demands bit-identical frame hashes.

```bash
./build.sh
../tools/emulator/slow32-fast asteroids.s32x &   # the machine
../tools/s32-crt/s32-crt                          # the glass
./tests/run-tests.sh
```

Keys (on the CRT): arrows or WASD rotate and thrust, space fires, `h` is
hyperspace (classic risk included), `q`/ESC quits. Rocks split
large -> 2 medium -> 2 small (20/50/100 points), extra ship at
10,000, waves grow. Score and spare ships are drawn with the stroke
font in `src/vfont.c`, DVG-style.

`--frames N --seed S` is the deterministic test mode: fixed timestep,
no pacing, one key event per frame from the `S32_TUBE_KEYS` injection
file (mind the 256-event queue cap), and a `report ...` line at exit.

Not here yet, per the plan's build-when-pulled rule: saucers,
rock spin, a restart key, sound (a different document, if ever).
