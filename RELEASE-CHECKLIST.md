# SLOW-32 Release Checklist

## Pre-Release Tasks

### Documentation Review
- [ ] All revision history tables updated with current version
- [ ] Installation guide tested on clean system
- [ ] Examples build and run successfully
- [ ] No references to private ~/slow-32 repository
- [ ] Links between documents work correctly
- [ ] README.md is clear and welcoming

### Binary Preparation
- [ ] Run `tools/prepare-release.sh` from ~/slow-32
- [ ] Test binaries on Ubuntu 20.04
- [ ] Test binaries on Ubuntu 22.04
- [ ] Verify all tools have --version or --help
- [ ] Runtime libraries included (crt0.s32o, intrinsics.s32o)
- [ ] File permissions correct (executables marked)

### Code Quality
- [ ] Regression tests pass (`cd ~/slow-32/regression && ./run-tests.sh`)
- [ ] No debug output in release builds
- [ ] Error messages are helpful
- [ ] Tools handle bad input gracefully

### Repository Setup
- [ ] Create GitHub release
- [ ] Upload binary tarball
- [ ] Upload SHA256 checksum
- [ ] Tag release with version number
- [ ] Release notes written

## Release Notes Template

```markdown
# SLOW-32 Tools v1.0.0

First public release of the SLOW-32 toolchain.

## What's Included

**Core Tools:**
- slow32asm - Assembler
- s32-ld - Linker  
- slow32 - Reference emulator with debugging
- slow32-fast - Performance emulator

**Analysis Tools:**
- slow32dis - Disassembler
- s32-objdump - Object file analyzer
- s32-exedump - Executable analyzer

**Runtime Libraries:**
- crt0.s32o - C runtime startup
- intrinsics.s32o - Built-in functions

## Features

- Complete 32-bit RISC instruction set
- Full debugging support in emulator
- ~350M instructions/sec in fast mode
- Basic 64-bit integer support (ADD/SUB)
- Varargs and switch statement support

## Known Limitations

- Requires optimization (-O1 or higher) for C code
- 64-bit multiply/divide not complete
- No floating point support yet
- DEBUG instruction only for I/O

## Installation

See [Installation Guide](docs/02-installation.md)

## Checksums

```
SHA256: [checksum] slow32-tools-1.0.0-linux-x64.tar.gz
```
```

## Post-Release Tasks

- [ ] Announce on relevant forums/communities
- [ ] Update Substack article with links
- [ ] Monitor issues for early problems
- [ ] Start planning next release features

## Version Numbering

Use semantic versioning:
- MAJOR.MINOR.PATCH
- MAJOR: Breaking ISA changes
- MINOR: New features, backward compatible
- PATCH: Bug fixes only

Examples:
- 1.0.0 - First stable release
- 1.1.0 - Add floating point
- 1.2.0 - Complete 64-bit support
- 2.0.0 - ISA revision (if ever needed)