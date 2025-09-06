# CLAUDE.md - Quick Reference for AI Assistants

SLOW-32 is a 32-bit RISC CPU architecture designed for simplicity and education. This public repository contains documentation, specifications, and examples.

## Repository Purpose

This is the public-facing documentation repository for SLOW-32. It contains:
- Architecture specifications and documentation
- Example programs and tutorials
- Links to related articles and resources
- Public API/ABI documentation

## Key Architecture Points

- 32 registers: r0=0 (hardwired zero), r1=return, r3-r10=args, r29=sp, r30=fp, r31=lr
- No condition codes - comparisons return 0/1 in registers
- W^X protection - code segment is execute-only
- Stack grows down from high memory
- DEBUG instruction for simple I/O (character output)
- Fixed 32-bit instruction encoding

## Documentation Structure

- `/docs` - Technical documentation
  - `00-overview.md` - Project story and philosophy
  - `10-quickstart.md` - Getting started guide
  - `20-programmers-guide.md` - ABI and conventions
  - `25-instruction-set.md` - ISA reference
  - `30-object-format.md` - File formats (.s32o, .s32x)
  - `40-emulators.md` - Emulator documentation
  - `50-io-and-host.md` - I/O interfaces
  - `90-roadmap.md` - Future plans
- `/examples` - Sample programs
- `/articles` - Links to Substack articles

## For Contributors

When updating this repository:
1. Keep documentation clear and educational
2. Provide working examples where possible
3. Link to relevant Substack articles for context
4. Maintain consistency with the private implementation

## Related Resources

- Implementation details are in the private repository
- LLVM backend development happens separately
- Substack articles provide narrative context