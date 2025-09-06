# SLOW-32 in one idea: the emulator *is* the OS

I love compilers and emulators, but I don't love hauling 40 years of hardware quirks into a learning project. SLOW-32 is a tiny, friendly ISA that runs **at the application level**. No real/protected/flat modes, no MMU, no paging, no interrupts, no carry bit, no DMA. Just enough to write code, link it, and execute in a clean, bounded sandbox.

**What drops out when you remove the baggage?**
- You can bound addresses and make the emulator fast and predictable.
- Crashes are fine—just spawn a fresh instance. Read-only code/.rodata can be shared (planned), so recovery is cheap.
- I/O becomes explicit: a `DEBUG` instruction today; later, **MMIO ring buffers** and a **TRAP** interface to reach host facilities.
- The **Interrupt Service Table** isn't memory: it's metadata in the `.s32x` file. The loader/emulator routes faults.
- One **W^X** line protects code and `.rodata`; long-term this moves to the host memory controller.

**Why this matters**
- It's a clean surface for LLVM backend work.
- It's easy to emulate well.
- It's readable for humans.

> If you've ever wanted an "application-level assembly" experience—more principled than an old 8/16-bit micro, less fussy than a modern SoC—this is that.