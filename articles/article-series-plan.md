# SLOW-32 Substack Article Series Plan

## Overview
This document outlines the article series for SLOW-32, a modern educational CPU architecture and toolchain. Rather than massive comprehensive articles, we're taking a focused, story-driven approach where each article solves one interesting problem or explores one key concept.

## Published Articles

### 1. **SLOW‑32: Building an Application‑Level Assembly Sandbox** ✅
   - Introduction to the project philosophy
   - High-level architecture overview
   - Why "less is more" for educational CPUs
   - Application-level vs system-level design choices

### 2. **Building an LLVM Backend: The 64-bit Integer Breakthrough** ✅
   - The carry flag problem on SLOW-32
   - Why ADDC/ADDE failed spectacularly
   - The UADDO/USUBO solution
   - Beautiful 4-instruction i64 operations
   - Lessons about not fighting your architecture

### 3. **Varargs Done Right: A Simple and Elegant Solution** ✅
   - The mystery: arguments appearing in reverse order
   - Register save ordering discoveries
   - How SLOW-32's simple approach beats complexity
   - The elegant char* va_list solution

### 4. **Jump Tables Without Jump Tables** ✅
   - How to implement switch statements without native support
   - LLVM's clever value table optimization
   - Performance that beats traditional jump tables
   - Security benefits of no indirect jumps

## Upcoming LLVM Backend Stories

### 5. **The SELECT Problem: Why Conditional Moves Are Tricky**
   - LLVM's ISD::SELECT operation
   - Why our first attempt generated 8 instructions
   - The branch vs conditional move tradeoff
   - Final 3-instruction solution

### 6. **Calling Convention Crimes: The Register Clobber Mystery**
   - Accidentally destroying registers across calls
   - LLVM's assumptions vs our implementation
   - The importance of callee-saved registers
   - How we caught it with torture tests

## Toolchain Deep Dives

### 7. **The %hi/%lo Problem: Loading 32-bit Constants**
   - Why you can't fit 32 bits in a 32-bit instruction
   - The LUI+ADDI dance
   - Sign extension gotchas
   - How the assembler and linker cooperate

### 8. **Two-Pass Assembly: Why Forward References Matter**
   - The bootstrap problem
   - Symbol resolution strategies
   - Why we kept it simple
   - Real examples that break single-pass

### 9. **Relocations: The Assembler-Linker Contract**
   - What R_SLOW32_HI20 really means
   - Symbol arithmetic and overflow
   - The paired relocation requirement
   - Debug information preservation

### 10. **Building the Linker: Section Tetris**
   - Merging .text from multiple files
   - The W^X boundary calculation
   - Symbol resolution precedence
   - Why layout order matters

### 11. **Object vs Executable: Two File Formats, One Goal**
   - The .s32o format design
   - The .s32x format and why it's different
   - Metadata that survives linking
   - The Interrupt Service Table trick

## Emulator Stories

### 12. **350 Million Instructions Per Second: The Fast Emulator**
   - Computed goto magic
   - Why switch statements are slow
   - Memory protection without MMU
   - The DEBUG instruction fast path

### 13. **The Debug Emulator: When Slow Is Good**
   - Comprehensive tracing architecture
   - Register diff visualization
   - Memory access patterns
   - Finding bugs with -r flag

## Runtime and Architecture Stories

### 14. **crt0.s: Where Programs Begin**
   - Stack pointer initialization
   - The argc/argv setup dance
   - Why _start comes before main
   - The HALT at the end

### 15. **Printf Without an OS: The DEBUG Instruction Story**
   - One instruction for all I/O
   - Building printf on top of putchar
   - Format string parsing
   - Why we kept it minimal

### 16. **Memcpy and the Register Allocation Bug**
   - Hand-optimized assembly gone wrong
   - The clobber list mistake
   - How torture tests saved us
   - Performance vs correctness

### 17. **The Instruction That Isn't: Pseudo-Instructions**
   - Why 'li' is actually LUI+ADDI
   - The 'mv' that's really ADD
   - CALL and its link register magic
   - Assembler convenience vs ISA purity

## Architecture Philosophy

### 18. **No Flags, No Problem: Compare-and-Branch Design**
   - Why condition codes complicate pipelines
   - The RISC philosophy of visible state
   - Comparison results in registers
   - Performance implications

### 19. **W^X Without an MMU: Protection Through Segmentation**
   - Code segment execute-only
   - Data segment no-execute
   - The boundary calculation
   - Why it's enough for education

### 20. **The DEBUG Instruction: Minimalist I/O**
   - One instruction to rule them all
   - Character output simplicity
   - Future MMIO extensions
   - Host-guest boundaries

## War Stories and Bug Hunts

### 21. **The Varargs Clobber Mystery**
   - Symptoms: corrupted variadic arguments
   - The register save order revelation
   - Why it only failed sometimes
   - The fix that simplified everything

### 22. **When i64 Shifts Go Wrong**
   - Shifts >= 32 bits failing silently
   - The undefined behavior trap
   - LLVM's assumptions vs reality
   - The explicit masking solution

### 23. **The Great Symbol Resolution Rewrite**
   - Why weak symbols broke everything
   - Common blocks and FORTRAN legacy
   - The precedence rules that work
   - Linker complexity management

## Testing and Developer Tools

### 24. **Torture Tests: Finding Bugs Before Users Do**
   - The automated test framework
   - Categories: features, regression, torture
   - Real bugs caught and fixed
   - Continuous testing philosophy

### 25. **Building a Disassembler That Doesn't Suck**
   - Symbol-aware disassembly
   - Relocation visualization
   - Section layout display
   - Error messages that actually help

## Formal Toolchain Documentation (Reference Style)

### Technical Reference Series
*These articles provide formal, comprehensive documentation in the style of classic processor manuals and toolchain guides.*

### TR-1. **SLOW-32 Instruction Set Architecture Reference**
   - Complete opcode tables
   - Instruction encoding formats
   - Detailed instruction descriptions
   - Timing and pipeline behavior
   - Assembly language syntax

### TR-2. **SLOW-32 Application Binary Interface (ABI)**
   - Register usage conventions
   - Calling conventions
   - Stack frame layout
   - Data type representations
   - System call interface (DEBUG)

### TR-3. **SLOW-32 Object File Format Specification**
   - File header structure
   - Section headers
   - Symbol table format
   - Relocation entries
   - String table organization

### TR-4. **SLOW-32 Executable Format Specification**
   - Executable header
   - Segment layout
   - Entry point specification
   - Interrupt Service Table
   - Memory map requirements

### TR-5. **SLOW-32 Assembler Reference Manual**
   - Directive reference (.text, .data, etc.)
   - Pseudo-instruction expansions
   - Expression evaluation rules
   - Symbol resolution
   - Macro capabilities (future)

### TR-6. **SLOW-32 Linker Reference Manual**
   - Command-line options
   - Symbol resolution rules
   - Section merging algorithm
   - Relocation processing
   - Map file generation

### TR-7. **SLOW-32 Runtime Library Reference**
   - Function reference (memcpy, printf, etc.)
   - Intrinsics documentation
   - Startup code (crt0)
   - Stack initialization
   - Variable argument handling

### TR-8. **SLOW-32 Emulator User Guide**
   - Command-line options
   - Debug flags and tracing
   - Performance characteristics
   - Memory protection model
   - Host interface (DEBUG instruction)

## Future Directions & Reflections

### 26. **What's Next for SLOW-32**
   - Floating-point extension design
   - MMIO ring buffers for real I/O
   - TRAP instruction considerations
   - Community contributions

### 27. **Lessons from Building a Toolchain**
   - LLVM backend gotchas
   - Regression test importance
   - Human-AI collaboration
   - What I'd do differently

## Special Topics

### The Standalone LLVM IR Compiler
   - Why we built it alongside the backend
   - SSA to register allocation
   - PHI node elimination
   - Performance comparisons

### SLOW-32 vs Other Educational ISAs
   - Comparison with RISC-V RV32I
   - Why simpler than MIPS
   - Learning curve analysis
   - Educational effectiveness

## Writing Strategy

### Two Article Tracks

1. **Story Articles (Substack style)**
   - Problem → struggle → breakthrough narrative
   - Personal development journey
   - "Aha!" moments and lessons learned
   - 5-10 minute reads
   - Published every 2-3 weeks

2. **Technical References (Documentation style)**
   - Formal specifications and manuals
   - Complete reference documentation
   - Professional datasheet format
   - Published as toolchain components mature
   - Living documents in the repository

### Article Guidelines

**For Story Articles:**
- Start with a real problem or bug
- Show the failed attempts
- Build to the breakthrough
- Include working code snippets
- End with lessons learned

**For Technical References:**
- Follow classic manual structure
- Complete tables and specifications
- Formal language and precise definitions
- Cross-references and indices
- Version-controlled with the code

### Publishing Notes

- Story articles go to Substack
- Technical references live in /docs
- Cross-link between the two
- Story articles can preview technical details
- Technical docs can reference story articles for context