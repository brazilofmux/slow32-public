# SLOW-32 Substack Article Series Plan

## Overview
This document outlines the planned article series for SLOW-32, a modern educational CPU architecture and toolchain. Each article will explore different aspects of the system, from high-level design philosophy to deep technical implementation details.

## Published Articles
1. **SLOW‑32: Building an Application‑Level Assembly Sandbox** ✅
   - Introduction to the project philosophy
   - High-level architecture overview
   - Why "less is more" for educational CPUs
   - Application-level vs system-level design choices

## Planned Article Series

### Part 2: The LLVM Backend Journey (Technical Deep Dive)
**Title:** "Building a Custom LLVM Backend from Scratch: The SLOW-32 Story"
- **Motivation:** Why LLVM over custom compiler
- **Key Topics:**
  - Setting up LLVM for a new target
  - TableGen and instruction selection
  - Register allocation for 32 registers
  - The 64-bit integer breakthrough (UADDO/USUBO approach)
  - Jump tables and switch statements
  - Varargs implementation
  - PHI node handling
- **Code Examples:**
  - Walk through a simple function compilation
  - Show LLVM IR → SLOW-32 assembly transformation
  - Demonstrate optimization passes
- **Lessons Learned:**
  - What worked (RISC-V inspiration)
  - What didn't (early ADDC/ADDE attempts)
  - Collaboration stories (Claude + ChatGPT teamwork)

### Part 3: The Assembler & Object File Format
**Title:** "Designing a Modern Assembler: Relocations, Symbols, and Binary Formats"
- **Key Topics:**
  - Object file format (.s32o) design
  - Relocation types (R_SLOW32_ABS32, R_SLOW32_HI20, etc.)
  - Symbol tables and string tables
  - Pseudo-instructions (li, mv, call)
  - Expression evaluation
  - Section handling (.text, .data, .rodata, .bss)
- **Implementation Details:**
  - Two-pass assembly process
  - Handling forward references
  - %hi()/%lo() for 32-bit constants
  - Jump table support (.word with symbols)
- **Code walkthrough:** Assembling a complex program with multiple sections

### Part 4: The Linker - Where Magic Happens
**Title:** "Building a Linker: From Objects to Executables"
- **Key Topics:**
  - Executable format (.s32x) design
  - Symbol resolution across files
  - Relocation processing
  - Section merging and layout
  - The W^X boundary implementation
  - HI20/LO12 relocation pairing
- **Technical Challenges:**
  - Handling weak symbols
  - Common block allocation
  - Optimizing layout for cache
  - Interrupt Service Table metadata
- **Case Study:** Linking a multi-file program with runtime library

### Part 5: Two Emulators, Two Philosophies
**Title:** "Fast vs Debug: Building Complementary Emulators"
- **The Fast Emulator:**
  - Direct threaded interpretation
  - Computed goto dispatch
  - ~350M instructions/second
  - Memory protection boundaries
  - Minimal overhead design
- **The Debug Emulator:**
  - Comprehensive tracing (-t flag)
  - Register change tracking (-r flag)
  - Breakpoints and watchpoints
  - Memory access monitoring
  - Step-by-step execution
- **Implementation techniques:**
  - Efficient instruction decode
  - Branch prediction hints
  - Cache-friendly data structures
- **Performance analysis and benchmarks**

### Part 6: The Runtime Library
**Title:** "Building a Minimal C Runtime: From crt0 to printf"
- **Core Components:**
  - crt0.s - Program startup
  - Stack initialization
  - Intrinsics (memcpy, memset, etc.)
  - The printf implementation saga
- **Varargs on SLOW-32:**
  - Register vs stack arguments
  - The simple char* va_list approach
  - Why it's cleaner than x86
- **Standard library subset:**
  - String operations
  - Memory operations
  - Integer to string conversions
- **Code size optimization techniques**

### Part 7: The Instruction Set Design
**Title:** "Designing a Clean RISC ISA: Lessons from SLOW-32"
- **Design Principles:**
  - Fixed 32-bit instructions
  - No condition codes (compare-and-branch)
  - Simple addressing modes
  - Register-register operations
- **Instruction Formats:**
  - R-type (register-register)
  - I-type (immediate)
  - B-type (branches)
  - J-type (jumps)
  - U-type (upper immediate)
- **Notable Instructions:**
  - DEBUG - The only I/O mechanism
  - HALT vs YIELD
  - The comparison zoo (SEQ, SNE, SLT, etc.)
- **What's NOT there and why:**
  - No carry flag
  - No interrupts
  - No privileged modes
  - No MMU/TLB

### Part 8: Testing & Regression Infrastructure
**Title:** "Ensuring Correctness: Building a Regression Test Suite"
- **The Test Framework:**
  - Automated test discovery
  - Expected output validation
  - Performance regression detection
  - Coverage analysis
- **Test Categories:**
  - Feature tests (arithmetic, branches, etc.)
  - Bug regression tests
  - Torture tests (complex programs)
  - Performance benchmarks
- **Real bugs caught:**
  - The varargs clobber bug
  - 32-bit constant loading issues
  - Memcpy register allocation bug
- **Continuous testing philosophy**

### Part 9: Debugging Tools & Developer Experience
**Title:** "Building Developer Tools: Disassemblers, Dumps, and Diagnostics"
- **Tool Suite:**
  - s32-objdump - Object file analysis
  - s32-exedump - Executable inspection
  - slow32dis - Interactive disassembler
- **Implementation highlights:**
  - Symbol-aware disassembly
  - Relocation visualization
  - Section layout display
- **Developer workflow optimization:**
  - Error messages that help
  - Debugging mysterious crashes
  - Performance profiling support

### Part 10: Real Programs & Benchmarks
**Title:** "From Hello World to CRC32: Real Programs on SLOW-32"
- **Example Programs:**
  - Hello World evolution
  - Recursive factorial
  - CRC32 with lookup tables
  - Printf implementation
  - Sorting algorithms
- **Performance Analysis:**
  - Instruction mix statistics
  - Hot path optimization
  - Register pressure analysis
- **Code generation patterns:**
  - Loop optimizations
  - Function call overhead
  - Switch statement efficiency

### Part 11: The 64-bit Integer Saga
**Title:** "Supporting 64-bit Math Without 64-bit Registers"
- **The Challenge:**
  - Register pairs (R1:R2, R3:R4)
  - Carry/borrow without flags
- **The UADDO/USUBO Breakthrough:**
  - Why ADDC/ADDE failed
  - The elegant comparison approach
  - 4-instruction i64 operations
- **Implementation Details:**
  - Shift-parts for i64 shifts
  - Calling convention changes
  - LLVM backend modifications
- **Remaining work:** i64 multiply/divide

### Part 12: Future Directions & Extensions
**Title:** "What's Next for SLOW-32: Floating Point, SIMD, and Beyond"
- **Near-term plans:**
  - Floating-point extension
  - MMIO ring buffers
  - TRAP instruction
  - Lightweight standard library
- **Experimental ideas:**
  - Basic atomics
  - SIMD for memcpy/crypto
  - JIT compilation with TCC
  - Multi-instance coordination
- **Community & Collaboration:**
  - How to contribute
  - Educational use cases
  - Potential research projects

### Part 13: Lessons Learned & Reflections
**Title:** "What I Learned Building a CPU Toolchain from Scratch"
- **Technical Insights:**
  - LLVM backend gotchas
  - The importance of regression tests
  - Performance vs simplicity tradeoffs
- **Project Management:**
  - Incremental development
  - The value of working examples
  - Documentation as you go
- **Collaboration Stories:**
  - Human-AI pair programming
  - Open source community feedback
  - Educational applications

## Special Topics (Potential Bonus Articles)

### A. The Standalone Compiler
**Title:** "Building a Standalone LLVM IR to Assembly Compiler"
- The translator architecture
- SSA to register allocation
- PHI node elimination
- Why we kept it alongside LLVM backend

### B. Comparison with Other Educational ISAs
**Title:** "SLOW-32 vs RISC-V, ARM, and MIPS: Design Tradeoffs"
- Instruction encoding comparison
- Complexity analysis
- Performance characteristics
- Educational value assessment

### C. Using SLOW-32 for Compiler Education
**Title:** "Teaching Compilers with SLOW-32: A Curriculum Guide"
- Course outline
- Lab exercises
- Student projects
- Assessment strategies

### D. Security in a Sandbox
**Title:** "Crash-Safe by Design: SLOW-32's Security Model"
- Memory protection without MMU
- W^X enforcement
- Sandboxing strategies
- Host-guest boundaries

## Article Writing Guidelines

### Target Audience
- Compiler engineers
- Systems programmers
- CS students (advanced undergraduate/graduate)
- Programming language enthusiasts
- Emulator developers

### Article Structure
1. **Hook:** Real problem or interesting question
2. **Context:** Why this matters
3. **Technical Content:** Deep dive with code
4. **Practical Examples:** Working demonstrations
5. **Lessons Learned:** What worked, what didn't
6. **Next Steps:** What comes after

### Code Examples Strategy
- Show real, working code
- Include command lines to reproduce
- Highlight key insights in comments
- Provide before/after comparisons
- Link to GitHub for full source

### Visual Elements
- Instruction encoding diagrams
- Memory layout illustrations
- Performance graphs
- Architecture block diagrams
- Assembly/machine code comparisons

## Publishing Schedule
- Target: One article every 2-3 weeks
- Allow flexibility for technical articles that need more research
- Coordinate with code releases and feature completions
- Build momentum with consistent publishing

## Cross-Promotion Ideas
- Reference previous articles for context
- Preview upcoming topics
- Create article series collections
- Develop companion GitHub examples
- Consider video walkthroughs for complex topics

## Metrics to Track
- Reader engagement (likes, comments)
- Technical questions raised
- Code contributions inspired
- Educational adoptions
- Performance of different article types

## Notes for Future Articles
- Document interesting bugs as they're fixed
- Save performance optimization stories
- Track "aha!" moments during development
- Collect reader questions for FAQ articles
- Consider guest posts from contributors