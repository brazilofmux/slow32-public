# SLOW-32 QEMU Backend

This directory contains the SLOW-32 TCG (Tiny Code Generator) backend for QEMU, enabling `qemu-system-slow32` to execute SLOW-32 binaries via dynamic translation.

## Directory Structure

```
qemu-backend/
├── target/slow32/       # CPU state, TCG translation, helpers
├── hw/slow32/           # Machine definition and devices
├── configs/             # Build configuration files
│   ├── targets/         # slow32-softmmu.mak
│   └── devices/         # Device selection
├── docs/                # slow32-tcg documentation
├── patches/             # Integration patches (generated)
├── scripts/             # Backup and restore scripts
├── AGENTS.md            # Development guidance
└── CLAUDE.md            # Quick reference for AI assistants
```

## Workflow

The QEMU repository at `~/qemu` is read-only (upstream). Changes are made there, then backed up to this repository which is under version control.

### Making Changes

1. Work in `~/qemu`:
   ```bash
   cd ~/qemu
   # Edit files in target/slow32/, hw/slow32/, etc.
   vim target/slow32/translate.c
   ```

2. Test your changes:
   ```bash
   cd ~/qemu/build
   ninja
   ./qemu-system-slow32 -machine slow32-tcg -kernel test.s32x -nographic
   ```

3. Commit to local QEMU branch:
   ```bash
   cd ~/qemu
   git add target/slow32/ hw/slow32/ ...
   git commit -m "Description of changes"
   ```

### Backing Up to This Repository

4. Run backup script:
   ```bash
   cd ~/slow-32/qemu-backend/scripts
   ./backup.sh
   ```

5. Generate patches:
   ```bash
   ./generate-patches.sh
   ```

6. Review and commit to this repository:
   ```bash
   cd ~/slow-32/qemu-backend
   git status
   git diff
   git add -A
   git commit -m "Backup QEMU backend changes"
   git push
   ```

### Restoring to a New Machine

On a new machine after cloning `~/slow-32`, `~/llvm-project`, and `~/qemu`:

1. Apply patches to QEMU:
   ```bash
   cd ~/slow-32/qemu-backend/scripts
   ./apply-patches.sh ~/qemu
   ```

   This will:
   - Apply integration patches to modify existing files
   - Copy all SLOW-32-specific files (target/slow32/, hw/slow32/, etc.)

2. Resolve any merge conflicts if QEMU has been updated

3. Build QEMU:
   ```bash
   cd ~/qemu
   ./configure --target-list=slow32-softmmu
   make
   ```

### Emergency Restore

If you need to restore from this backup to `~/qemu`:

```bash
cd ~/slow-32/qemu-backend/scripts
./restore.sh
```

**Warning**: This overwrites files in `~/qemu`!

## Files Managed

### Patches (modifications to existing QEMU files)
- `target/Kconfig` - Add SLOW32 target
- `target/meson.build` - Build integration
- `hw/Kconfig` - Add SLOW32 machine
- `hw/meson.build` - Build integration
- `include/system/arch_init.h` - Architecture enum
- `README.rst` - Documentation updates
- `.gitignore` - Ignore slow-32 local copy

### New Files (backed up directly)
- `target/slow32/` - Complete target implementation
- `hw/slow32/` - Machine and device models
- `configs/targets/slow32-softmmu.mak` - Target configuration
- `configs/devices/slow32-softmmu/` - Device selection
- `docs/slow32-tcg/` - Design documentation
- `AGENTS.md` - Development guide
- `CLAUDE.md` - Quick reference

## Current Status

✅ Basic TCG backend working
✅ Simple machine model
✅ Debug output via DEBUG instruction
✅ Arithmetic, loads/stores, branches implemented
⚠️ .s32x loader not yet implemented (raw binary mode)
⚠️ Full ISA not complete

See `docs/todo.md` for detailed implementation checklist.

## Testing

Run the same program in both emulators:

```bash
# Reference interpreter
~/slow-32/tools/emulator/slow32 program.s32x

# QEMU TCG
~/qemu/build/qemu-system-slow32 -machine slow32-tcg -kernel program.s32x -nographic
```

Compare outputs to verify correctness.

## Documentation

- `docs/overview.md` - Architecture requirements
- `docs/todo.md` - Implementation checklist
- `AGENTS.md` - High-level integration guide
- `CLAUDE.md` - Quick commands and context
- Upstream QEMU docs: https://www.qemu.org/docs/master/devel/
