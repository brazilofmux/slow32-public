# slow32-dbt Optimization TODO

Optimization ideas identified from studying benchmark_core.s32x codegen.
Baseline: ~3.5 BIPS on bench_arith/branch/mem (10M iterations, 0.081s).
After item #1: ~4.6 BIPS (0.062s), 23.5% improvement.

## Queue

### 1. In-block back-edge loop optimization
**Status: DONE (commit 8c490d5, bug fix in next commit)**
Detect when a backward branch targets a PC within the same block and emit
a direct jmp to the loop body, skipping register flush/reload entirely.
Result: 0.081s → 0.062s (23.5% faster, ~4.6 BIPS).
Bug found and fixed: deferred side exit dirty snapshots become stale when
the back-edge loop iterates. Fix: conservatively mark all cached registers
dirty in deferred exit snapshots when a back-edge is detected.

### 2. Direct host register ALU operations
**Status: DONE**
The emitter funnels all operations through RAX/ECX scratch:
`mov eax, ebx; mov ecx, r12d; add eax, ecx; mov ebx, eax`
When both operands are in host registers, this could be just `add ebx, r12d`.
Would roughly halve instruction count in inner loops.
Requires refactoring translate_* functions to be register-aware.

### 3. Dead temporary elimination
**Status: DONE**
Pattern: `slli r7, r1, 7` then `xor r1, r7, r1` — r7 is a dead temporary.
The translator stores to EDI (r7's host slot) then immediately reloads it.
A simple "last-write" tracker could keep the value in RAX and skip
the store/load to the host register entirely.

### 4. Enable peephole optimization by default
**Status: DONE**
The `-P` flag previously enabled several patterns (xor/cmp→xor/test,
constant propagation into stores, redundant load elimination).
Now enabled by default; `-P` disables it.

### 5. Native dispatcher trampoline
**Status: DONE**
Block exits currently `ret` to the C dispatcher for every unchained transition.
Emit a dispatcher loop in the code buffer itself that stays in generated code.
The C dispatcher only gets involved for DEBUG/HALT/YIELD/faults.
The trampoline does full linear-probe cache lookup; shared_branch_exit and
inline lookup miss paths jump to it instead of returning to C.

### 6. Cross-block register allocation
**Status: TODO (long-term)**
Currently each block independently allocates registers. Hot loop back-edges
re-load the same registers every entry. If the allocator could coordinate
across chained blocks (at least for self-loops), the prologue/epilogue
could be eliminated entirely for the steady-state path.
(This subsumes item #1 for self-loops specifically.)

### 7. SIB addressing for loads/stores
**Status: DONE**
`emit_compute_addr()` now returns the register holding the effective address.
When the base register is allocated: imm==0 uses the host register directly
(zero instructions), imm!=0 uses LEA (1 instruction instead of 2).
The address register threads through bounds/alignment checks and SIB memory access.

### 8. Compile-time memory check specialization
**Status: DONE**
Memory access checks (bounds, alignment, MMIO, W^X) were always fully emitted
at ~25 x86-64 instructions per access, with runtime branches to skip disabled
features. Now checks are specialized at translation time using cpu state flags
as compile-time constants: only the checks actually needed are emitted, with
limits embedded as immediates. Typical case (no MMIO, no alignment traps)
reduces from ~20-28 instructions to 2-4 per memory access.
