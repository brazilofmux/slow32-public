# SLOW-32 Articles

This directory contains links and references to Substack articles about SLOW-32 development.

## Published Articles

*(Articles will be added here as they are published)*

## Article Series Outline

### Introduction Series
- The Philosophy of SLOW-32: Why Another ISA?
- Application-Level Assembly: Rethinking the Emulator-OS Boundary
- Building a Clean Compiler Target from Scratch

### Technical Deep Dives
- The SLOW-32 Instruction Set: Design Decisions
- Building an LLVM Backend for a Custom ISA
- Fast Emulation Without the Baggage
- The W^X Line: Simple Security by Design

### Implementation Stories
- From Zero to Hello World in a New ISA
- Debugging Tools That Actually Help
- Why 64-bit Integers Were Harder Than Expected
- The Joy of No Carry Flags

## Substack Integration Pattern

1. Publish the **story-first** article and link the docs repo
2. Pull short excerpts from **Programmer's Guide** and **Datasheet** into callout boxes (100–200 words each)
3. Include a 3-command **Try it** box pointing at `docs/10-quickstart.md` and `examples/hello-debug.s`

This keeps the article narrative-clean and moves the reference detail into the repo where it belongs.