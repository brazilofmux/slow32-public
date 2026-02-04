# SLOW-32 DBT Optimization Roadmap

**Current Status (Stage 4+):** ~6.0 BIPS (approx. 60% of native speed).
**Recent Wins:** Superblocks, Register Cache, Compare-Branch Fusion, and native dispatcher lookup.

The following optimizations represent the "heavy lifting" required to bridge the remaining 40% gap to native performance.

---

## 1. Direct Block Chaining (Code Patching)
Currently, even with the native dispatcher, every unchained block transition performs a hash lookup.
*   **The Idea:** When a block at `PC_A` jumps to `PC_B`, and `PC_B` is already translated, patch the exit of `PC_A` to jump **directly** to the host address of `PC_B`.
*   **Mechanism:** 
    *   Maintain a "patch-back" table (who jumps to this guest PC?).
    *   When a new block is committed to the cache, look up its guest PC in the patch-back table.
    *   Iterate through all predecessors and overwrite their 5-byte `jmp <shared_stub>` with a `jmp <target_host_code>`.
*   **Payoff:** Eliminates the hash lookup overhead entirely for all hot direct branches.

## 2. Trace-Level Register Allocation
We currently use a 6-slot register cache with "Pending Write" (dead temporary elimination), but it effectively flushes or snapshots at every superblock side-exit.
*   **The Idea:** Implement **Linear Scan Register Allocation** over an entire superblock.
*   **Mechanism:** 
    *   Treat the superblock as one large function.
    *   Perform a liveness analysis during the prescan to find "long-lived" guest registers.
    *   Assign the most frequently used registers to fixed host registers (R12-R15, RBX) for the duration of the entire trace.
*   **Payoff:** Massive reduction in `mov [rbp+off], reg` spills inside hot loops.

## 3. Signal-Based Memory Access (The "QEMU Trick")
Even with specialized checks, we emit `cmp/ja` for every load/store to guard the 256MB guest memory boundary.
*   **The Idea:** Use the host CPU's MMU to perform the bounds check for free.
*   **Mechanism:** 
    *   Map guest memory at a fixed host offset.
    *   Use `mprotect` to guard the regions.
    *   Register a host `SIGSEGV` handler.
    *   Emit **zero** check code: just `mov eax, [r14 + guest_addr]`.
    *   If a fault occurs, the handler catches it, finds the guest PC via the `pc_map`, and converts it to a guest `EXIT_FAULT`.
*   **Payoff:** Memory access overhead drops to effectively zero host instructions beyond the access itself.

## 4. Flag-Preserving Side Exits
Our Compare-Branch fusion is great, but superblock side-exits still often re-evaluate conditions.
*   **The Idea:** Use the x86-64 `EFLAGS` register as a first-class citizen in the translator.
*   **Mechanism:** 
    *   If a superblock side-exit is based on the same condition as the fused branch, don't emit a second `cmp`.
    *   Keep the x86 flags "live" between instructions when possible.
*   **Payoff:** Simplifies the logic of superblock "bridges."

## 5. Constant Propagation & Folding
Many SLOW-32 code patterns involve `LUI` + `ADDI` or complex `GEP` scaling.
*   **The Idea:** Track constant values in guest registers at translate-time.
*   **Mechanism:** 
    *   If `r1` is known to be `0x100000`, and we see `addi r2, r1, 4`, don't emit an `add`. Just mark `r2` as the constant `0x100004`.
    *   Fold these constants into the displacements of loads/stores.
*   **Payoff:** Turns many multi-instruction address calculations into zero-instruction host code (folded into the SIB displacement).
