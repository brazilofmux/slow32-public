# Memory Size from the Executable

Started 2026-11-02, after the work that let stage08's tables grow
(550625e1, 4076e91b).  That work moved the compiler's own limit from
fixed tables to the heap.  Then the next limit showed: a SLOW-32 program
cannot have more than 256 MB, whatever it asks for.

## How it works today

The linker starts from 256 MB and divides it up.  Code, read-only data,
data and BSS build up from 0.  The stack is pinned at `0x0FFFFFF0`.  The
MMIO window and the heap fill what is left between them, and
`--heap-size max` (a5ecaecf) takes all of it.  Every `.s32x` header
says `mem_size` 0x10000000.  The size options (`--code-size`,
`--stack-size`, `--heap-size`, ...) move boundaries inside the 256 MB;
nothing adds them up to a total.

That came from the code, not from a design decision.  A fixed space
was the easiest thing to divide.  The spec pass (508399de, 2026-10-03)
then wrote down what the code did:

- **§5.1:** a machine must refuse a `mem_size` over 0x10000000.  Every
  executable has exactly that, and what a smaller one means is
  Unspecified.
- **§5.3:** `stack_base` is 0x0FFFFFF0.
- **§6.2:** an access at or above `mem_size` must fault in every
  engine; whether one in a gap between regions faults is Unspecified.

What the machine was meant to keep is the inner loop.  Engines don't do
work on every access that the host can do for them: W^X is an
`mprotect`, not a test.  This plan keeps that.

## The ruling (2026-11-02)

Address space is close to free, so shrinking it to fit a program is
not worth doing.  What does matter is the ceiling: a program larger
than 256 MB cannot run at all.  So:

- **The linker** adds the parts up and is willing to make the space
  larger than 256 MB.
- **The engines** use the numbers in the header.

256 MB stays the floor: `mem_size = max(256 MB, total)`.  An executable
that fits in 256 MB is laid out exactly as it is today, byte for byte.

## The design

**Linker** (`tools/linker/s32-ld.c` and stage08's
`selfhost/src/tools/s32-ld.c`, in step, held to each other by
`run-ld-differential.sh`):

- **The total:** code + rodata + data/BSS + heap + MMIO + stack, each
  rounded up as today.
- **`mem_size`:** the total rounded up to 4 KB (the guest page; hosts
  with 16 KB pages round for themselves when they map), then at least
  0x10000000.
- **`stack_base`:** `mem_size - 16`.  At the floor that is
  0x0FFFFFF0, as now.  The MMIO window sits under the stack, as since
  a5ecaecf.
- **`--mem-size N`:** asks for a space explicitly, raised to the total
  when the parts need more.  `--heap-size max` keeps its meaning: all
  of whatever space there is.
- **Symbols:** `__stack_base`, `__stack_top`, `heap_end` and the
  header all follow `stack_base`, never the constant.

**Engines.** All five already take `stack_base` and `mem_size` from
the header, which is most of the work.  What is left:

- **`tools/emulator/slow32.h`:** `MAX_MEM_SIZE` 256 MB becomes the
  engine's ceiling, 2 GB (see below).  Check `slow32.c:180`, which
  sets `sp` to 0x0FFFFFF0 before the header overrides it, and
  `mmio.h`'s `MMIO_BASE 0x10000000`; both may be dead.
- **`tools/emulator/slow32-fast.c`:** it maps `hdr.mem_size`.  It
  needs the same ceiling check.
- **The DBT** (`tools/dbt/dbt.c`): it re-maps to `hdr.mem_size` at
  load (line 1107), and its translated code checks against
  `cpu->mem_size`, with bounds-check elimination in `translate.c`.
  Confirm that the x86-64 and AArch64 emitters load the bound rather
  than embedding 0x10000000.  Also check the shadow interpreter and the
  hooks (`hooks.c` checks `mem_size`).
- **QEMU** (`hw/slow32/slow32-tcg.c`): it refuses `mem_size` above
  `machine->ram_size`, whose default is 256 MiB.  Either size the RAM
  from the header, or have the `s32run` wrapper pass `-m`.  Change it
  in `~/qemu` first, as always.
- **stage00's `s32-emu`, the cross-compiled DBTs and the FPGA
  testbench** (`fpga/sim/slow32_tb.cpp:54` clamps to 0x10000000):
  each is checked; whatever can't grow refuses a larger executable
  with a clear message.

**Spec** (`docs/SPEC.md`):

- **§5.1:** `mem_size` is a multiple of 4 KB, at least 0x10000000 and
  at most 0x80000000.  A machine may refuse one larger than it
  supports, but must say so.
- **§5.3:** `stack_base` is `mem_size - 16`.
- **§6.2:** the address space is 0 to `mem_size - 1`; the fault rule
  is unchanged.

The clean-room interpreter is rerun from the amended spec alone (see
the machine spec memory).

## Why 2 GB, not 4 GB

The ISA addresses 4 GB, but above 2 GB an address is negative wherever
code holds it in a signed `int`.  There will be such places:

- the self-hosted tools;
- libc's `sbrk` and `malloc` arithmetic;
- the MMIO ring's offset checks;
- `--heap-size max`'s `-1` sentinel in the stage08 linker;
- any `ptrdiff_t` that spans the space.

2 GB is eight times today's room and needs none of that audited.  4 GB
is a later step, taken by auditing for signed addresses, with an
executable that puts the stack above 2 GB as the test.

## Tests

- **Layout unchanged at the floor:** every executable in the tree and
  the kit links to the same bytes as before.  That means the
  regression suite, `run-ld-differential.sh`, the kit apps and the
  tracked `.s32x` files.
- **A large program:** one that mallocs past 256 MB, linked with
  `--mem-size 1G --heap-size max`, runs on every engine and prints
  how far it got.
- **Faults:** an access at `mem_size` faults on every engine, and so
  does one just past the old 256 MB in an executable that sizes
  itself to 256 MB.
- **The compiler itself:** cc.s32x linked with a larger space
  compiles boxing and salvoend at 128 KB chunks.  Those are the two
  inputs `4076e91b` left out of memory.

## Order

1. Linker: the total, `--mem-size`, `stack_base` from `mem_size`.  At
   the floor, nothing changes.
2. The engines' ceilings (interpreters, DBT, QEMU), with the
   large-program test on each.
3. The spec amendment and the clean-room rerun.
4. The kit and images, at the next builder round.  A kit tool can
   then be linked larger where it helps, cc.s32x first.
5. Later, if wanted: 4 GB, after the signed-address audit.

## Status (2026-11-02)

- **Steps 1-3 landed** in 2f7db381: both linkers, `--mem-size`, the
  2GB ceiling in every engine (QEMU via ~/qemu 8cec4b22e8), the FPGA
  testbench refusing, and SPEC §5.1/§5.3/§6.2.  Still to do from
  step 3: the clean-room rerun against the amended spec.
- **Step 4 started:** cc.s32x is linked with `--mem-size 1G` (and so are
  the suite's gen1/gen2/gen3).  With it, boxing at 128KB chunks
  compiles (187 s, ~300MB resident) and prints what its clang build
  prints.  salvoend at 128KB chunks, a single 465KB function, still
  fills the whole 1GB after 20 minutes.  ~3.5M instructions' worth of
  tables is more than the function should need: probably a pass that
  grows without bound now that the near-ceiling guards are gone
  (4076e91b).  To trace with S12CC_TABDBG.
- The kit and images pick all this up at the next builder round.

## Out of scope

- Shrinking the space below 256 MB.  It's allowed by the design, but
  not done: address space is not worth saving.
- Reserving a 4 GB `PROT_NONE` region so the host MMU can replace the
  DBT's bounds checks.  That would be a performance change, and it
  belongs with the DBT, not here.
