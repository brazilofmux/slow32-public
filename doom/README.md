# DOOM on SLOW-32

The fb flagship, landed. id Software's Doom (via the doomgeneric
refactoring — GPLv2, see LICENSE) compiled by the SLOW-32 LLVM
toolchain, presenting 320x200 P8 frames straight down the tube.
`CMAP256` makes doomgeneric's screen buffer our framebuffer with
zero conversion; the palette rides beside it, rebuilt on every
gamma/pain/pickup flash.

```bash
./fetch-wad.sh                       # shareware doom1.wad (~4MB)
./build.sh
../tools/dbt/slow32-dbt doom.s32x &  # the show engine
../tools/s32-crt-mac/s32-crt-mac     # the glass
./tests/run-tests.sh
```

(`slow32-fast` also runs it and is the better debug engine; the DBT
is for show.) Keys: arrows move and turn, ctrl fires, space uses,
shift runs, alt strafes — or WASD. ESC for the menu.

`-timedemo demo3` is the regression workload: fully deterministic,
2173 presented frames, and every engine must produce the
bit-identical final frame hash. It is the largest single C program
the toolchain has swallowed, and landing it flushed out five real
platform bugs in one evening: a day-one libc strncpy that wrote n+1
bytes, a .comm/.bss address-allocation overlap in the assembler,
local symbols leaking into (and colliding with) the linker's global
namespace, sscanf existing only as a prototype, and printf ignoring
precision on integers. The port layer itself is one file
(`src/doomgeneric_slow32.c`); everything else is vendored doom with
three tiny patches (no system(), no zenity, debug hooks).

No sound (a different document, if ever). No mouse (it has not
earned its way in). Saves and config land in the working directory.

## How fast

`-timedemo demo1` (5026 gametics; a realtic is 1/35 s), run in an
empty directory, 2026-10-14:

    ../tools/dbt/slow32-dbt doom.s32x -iwad doom1.wad -timedemo demo1

| engine, build | Apple M5 Max | Xeon 8259CL |
|---|---|---|
| slow32-dbt, clang -O1 (this `doom.s32x`) | 75 | 169 |
| slow32-dbt, clang -O2 | 73–74 | |
| slow32-dbt, stage08 cc | 84 | |
| slow32-fast (interpreter) | 861 | |

For reference, the DOS DOOM's same demo1 under `~/x86`'s dos-monster
takes 93 realtics on the Mac (bare MS-DOS 6.22) and 177–190 on the
Xeon.  Different programs -- a user-mode binary handing its frame to
the tube, against a DOS executable on an emulated PC -- so take it as
a data point.  Repeat runs agree within a realtic; the wall clock adds
about 2.4 s of startup on the Mac.  Timing the stage08 build is how
"0.000000 fps" turned up (selfhost ISSUES-90).
