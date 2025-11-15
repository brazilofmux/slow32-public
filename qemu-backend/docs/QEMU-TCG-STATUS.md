# SLOW-32 TCG Backend Status

Last updated: 2025-11-14

## Latest Commit

**d532fe1e26** - Enhance SLOW-32 TCG: W^X enforcement, MMIO flags, and TB instrumentation

### Key Improvements

1. **Memory Protection (W^X Enforcement)**
   - Added `code_limit` and `rodata_limit` fields to CPUSlow32State
   - TLB fill now enforces proper permissions:
     - Code region: PAGE_READ | PAGE_EXEC (no write)
     - Read-only data: PAGE_READ only
     - Data region: PAGE_READ | PAGE_WRITE
   - Memory access faults properly logged and halt CPU

2. **MMIO Flag Support**
   - Added S32X_FLAG_W_XOR_X and S32X_FLAG_MMIO definitions
   - MMIO only enabled when S32X_FLAG_MMIO set in executable header
   - Proper .s32x format compliance

3. **Translation Block Instrumentation**
   - New helper: `slow32_tb_enter(env, icount)` tracks TB execution
   - Metrics collected:
     - `tb_translated_bytes` - total bytes translated
     - `tb_translated_insns` - total guest instructions translated
     - `tb_exec_count` - number of TB executions
     - `tb_exec_insns` - total guest instructions executed
   - Enhanced halt statistics with detailed TB metrics

4. **Performance Tuning**
   - Increased default max_insns to 64 for larger translation blocks
   - Reduces dispatch overhead, improves performance

5. **Code Quality**
   - Added `layout_defined` flag to distinguish flat vs structured memory
   - Proper initialization of all new fields in reset handler
   - Memory fault handling with informative error messages

## Patch Files

- `0001-Enhance-SLOW-32-TCG-W-X-enforcement-MMIO-flags-and-T.patch` - Latest commit (15K)
- `slow32-tcg-complete-20251114.patch` - All SLOW-32 commits (144K)

## Commit History

```
d532fe1e26 Enhance SLOW-32 TCG: W^X enforcement, MMIO flags, and TB instrumentation
ade5ab364a Major enhancements: MMIO support, translation timing, enhanced benchmarking
d7f8d1b4af Add instruction counting to TCG backend
0d0bb69f1d Add execution timing and comparison tooling
cb9a848f16 Ignore slow-32 directory (local toolchain copy)
760c379c2f Add SLOW-32 TCG target backend
```

## Files Modified (Latest Commit)

```
 hw/slow32/slow32-tcg.c    | 10 +++++++++-
 target/slow32/cpu-param.h |  1 +
 target/slow32/cpu.c       | 51 ++++++++++++++++++++++++++++++++++++++++++++++-
 target/slow32/cpu.h       |  9 +++++++++
 target/slow32/helper.c    | 22 ++++++++++++++++++++
 target/slow32/helper.h    |  1 +
 target/slow32/mmio.c      |  1 -
 target/slow32/mmio.h      |  2 ++
 target/slow32/translate.c | 15 +++++++++++++-
 9 files changed, 108 insertions(+), 4 deletions(-)
```

## Testing

Build and test:
```bash
cd /ztank/secret/sdennis/qemu
ninja -C build

# Test with a SLOW-32 program
cd slow-32
./tools/compile-c.sh examples/hello.c hello.s32x
cd ..
./build/qemu-system-slow32 -M slow32 -kernel slow-32/hello.s32x -nographic
```

## Next Steps

See `docs/slow32-tcg/todo.md` for implementation roadmap.

## Backup

All work backed up in:
- Git commit: d532fe1e26
- Patch files in ~/slow-32/
- Full repository at /ztank/secret/sdennis/qemu
