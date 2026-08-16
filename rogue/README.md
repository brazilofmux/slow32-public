# SLOW-32 Rogue

The Saturday afternoon. 80x24, `@` and `%` and `D`, permadeath.
Item #4 on the 1987-desk plan — `term.h` is why it exists.

Fetch the Amulet of Yendor from dungeon level 26 and climb back out.
Rooms reveal as you enter them, monsters get meaner with depth, food
runs out, and the save file is consumed by loading it.

```bash
./build.sh
../tools/emulator/slow32-fast rogue.s32x            # full screen
../tools/emulator/slow32-fast rogue.s32x --line     # line mode (tests, no term)
../tools/emulator/slow32-fast rogue.s32x --seed 7   # deterministic dungeon
./tests/run-tests.sh
```

Keys: `hjkl`/arrows and `yubn` move, `>`/`<` take the staircase (`%`),
`i` inventory, `e` eat, `q` quaff, `r` read, `w` wield, `W` wear,
`T` take off, `.` rest, `S` save+exit (`rogue.sav`), `Q` quit, `?` help.
Death and victory append to `rogue.scr`.

`--wizard` adds debug keys: `&` jump to the stairs, `*` reveal the map,
`A` conjure the Amulet. The test suite plays wizard.

v1 keeps rooms lit and items pre-identified; dark rooms, traps, rings,
and the identification minigame are the classic next fights.
