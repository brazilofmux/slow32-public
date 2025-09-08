# Roadmap

**Completed (v2.0)**
- ✅ MMIO console I/O implementation with ring buffer architecture
- ✅ Memory-efficient emulator with sparse memory management
- ✅ Intelligent linker memory layout management
- ✅ Complete 64-bit integer support

**Near-term**
- Extended MMIO devices (timer, interrupt controller)
- TRAP system call interface
- Disassembler polish, objdump/exedump views

**Mid-term**
- Floating point: scalar encoding, calling convention, NaN/rounding
- Atomics/concurrency: minimal ops vs. host-mediated primitives
- SIMD exploration (64/128-bit lanes)
- Optional privileged/TRAP space
- JIT: explore QEMU Tiny Code Compiler