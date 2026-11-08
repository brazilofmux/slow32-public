# tzlookup on SLOW-32

A port of the tzlookup *runtime* (the O(1) ZIP/phone → IANA timezone lookup) to
the SLOW-32 RISC CPU and toolchain (this tree). The Python
data pipeline stays on the host; this directory only ports the C lookup and CLI
and reuses the generated tables verbatim.

## What's here

| file | role |
|------|------|
| `tzlookup.c` | plain-C port of `../src/tzlookup.cpp` (NULL/`<ctype.h>` idioms) |
| `tz_cli.c` | plain-C port of `../src/tz_cli.cpp` |
| `test_tzlookup.c` | on-emulator assertion suite (mirrors `../tests/`) |
| `Makefile` | builds `tz.s32x` / `tz_test.s32x` with the SLOW-32 toolchain |

The shared header (`../src/tzlookup.h`) and the 101 KB of generated tables
(`../gen/tz_data.h`) are pulled in via `-I` — **no data is duplicated**.

## Build & test

```
make           # -> tz.s32x, tz_test.s32x   (the tree's toolchain + LLVM)
make test      # run the assertion suite on the slow32 emulator
make dist      # copy tz.s32x into ~/s32x (the central artifact repo)
```

`SLOW32` defaults to this tree (`../..`); `make LLVM=...` points at another clang.

## Running

Pass guest args after the program name (the slow32 emulator forwards
unrecognized args to the guest; **do not** use a `--` separator — it is
currently passed through literally as `argv[1]`):

```
$ slow32 tz.s32x --zip 94089 --phone "+1 (408) 555-1212"
America/Los_Angeles
$ slow32 tz.s32x --phone 1-808-555-1212
Pacific/Honolulu
```

Verified bit-for-bit against the host `tz` over a sweep of ZIPs + area codes.

## One long line

LLVM emits `TZ_ZIP_ZONE[100000]` as a single 332 KB `.ascii` line.  Both SLOW-32
assemblers read lines into fixed buffers until 2026-11-08 and refused (or, the
self-hosted one, silently split) it, so a filter here rewrote the string as
`.byte` lines.  The assemblers now take a line of any length, and the filter
is gone.
