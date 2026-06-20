# Pass 1 Security Audit Report: Memory Safety & Crash Defects
## SLOW-32 Host-Side Toolchain

## Executive Summary

This pass confirmed **20 distinct memory-safety/crash defects** across the host-side toolchain (after deduplicating two read/write mirrors of the same MMIO root cause). The worst cluster is the **linker (`s32-ld.c`)** with 7 confirmed bugs, including an attacker-controlled heap **OOB write** (relocation-count integer overflow) and multiple OOB reads from un-NUL-terminated string tables and unbounded section/symbol indices. The second major cluster is the **DBT JIT (`tools/dbt/`)** with 3 high-severity bugs forming a single exploit chain: an undetected code-buffer overflow whose `code_buffer_used` overshoot underflows the next block's capacity to ~4GB, yielding unbounded OOB writes into an RWX mapping. The remaining findings are spread across the **assembler** (a clean stack-buffer overflow from a hostile `.s` file), the **emulator MMIO path** (bounded 1–3 byte OOB), and the **archiver/dump utilities** (string-table OOB reads). No critical-after-verification findings survived as "critical"; the two originally-critical linker findings were downgraded to **high** because they are read/DoS-dominant in an offline build tool. The dominant theme: **untrusted file-format fields (offsets, counts, sizes) are consumed without 64-bit-safe bounds checks or NUL-termination guarantees.**

---

## Severity-Ranked Findings

| # | Severity | Component | File:line | Title |
|---|----------|-----------|-----------|-------|
| 1 | High | Linker | `tools/linker/s32-ld.c:232` | Integer overflow in archive-member header bounds check → huge OOB read/DoS |
| 2 | High | Linker | `tools/linker/s32-ld.c:518` | Regular-file object loader does zero header validation; `total_relocs` overflow → heap OOB write |
| 3 | High | Linker | `tools/linker/s32-ld.c:195` | `safe_string` does not guarantee NUL termination → OOB read |
| 4 | High | Linker | `tools/linker/s32-ld.c:439` | Archive symbol `member_index` used to index `loaded[]`/`members[]` without bounds check |
| 5 | High | Linker | `tools/linker/s32-ld.c:438` | Archive string-table `name_offset` used without bounds check |
| 6 | High | Linker | `tools/linker/s32-ld.c:261` | Archive member section offsets validated with overflow-prone 32-bit math → OOB read |
| 7 | High | Archiver | `tools/utilities/s32-ar.c:196` | Object-file symbol `name_offset` unbounded → OOB read in `build_symbol_index` |
| 8 | High | Assembler | `tools/assembler/slow32asm.c:117` | Stack buffer overflow in string tokenizer via unchecked backslash-escape write |
| 9 | High | DBT | `tools/dbt/translate.c:5761` | Code-buffer overflow never detected/flushed → OOB writes |
| 10 | High | DBT | `tools/dbt/block_cache.c:565` | `emit_init` capacity underflows to ~4GB after commit overshoot |
| 11 | High | DBT | `tools/dbt/stage5_ssa.c:76` | OOB write in SSA overlay: `value_def_node[]` sized by node count, indexed by value-id |
| 12 | Medium | Emulator | `tools/emulator/mmio_ring.c:984/934` | Unaligned word MMIO access reads/writes 1–3 bytes past the 64KB mmap |
| 13 | Medium | Emulator | `tools/emulator/s32x_loader.h:451` | Non-NUL-terminated symbol string table → OOB read when symbol names printed |
| 14 | Medium | Linker | `tools/linker/s32-ld.c:736` | Symbol section index not upper-bounds-checked before `section_base[]` |
| 15 | Medium | Archiver | `tools/utilities/s32-ar.c:233` | 32-bit overflow in archive table bounds checks → unchecked counts to `malloc`/OOB read |
| 16 | Medium | Archiver | `tools/utilities/slow32dis.c:609` | Section `name_offset` never bounds-checked → OOB read in `printf %s` |
| 17 | Medium | Archiver | `tools/utilities/slow32dis.c:593` | Unchecked `fread` returns leave header/section/strtab buffers uninitialized |
| 18 | Medium | Archiver | `tools/utilities/s32-ar.c:287` | Member name from string table used without NUL-termination guarantee |
| 19 | Medium | Archiver | `tools/utilities/slow32dump.c:195` | String table not NUL-terminated → OOB read printing names |
| 20 | Medium | Assembler | `tools/assembler/slow32asm.c:1631` | Undefined/large shift in `.align` → capacity-check overflow → OOB heap write/OOM |
| 21 | Medium | DBT (shadow) | `tools/dbt/shadow_interp.c:443` | f64 register-pair opcodes with reg==31 read/write `regs[32]` (one past array) |
| 22 | Low | Emulator | `tools/emulator/s32x_loader.h:241` | Non-NUL-terminated section name table → OOB read under `-t`/verbose |
| 23 | Low | Emulator | `tools/emulator/memory_manager.h:181` | Unvalidated header limits feed `mm_setup_from_s32x` → uint32 wrap (hardening) |
| 24 | Low | Archiver | `tools/utilities/s32-ar.c:145` | Unchecked `malloc` of attacker-controlled size → NULL deref/crash |
| 25 | Low | DBT5 (shadow) | `tools/dbt5/shadow_interp.c:499` | f64 register-pair `regs[reg+1]` one past array (aliases shadow `pc`) |

---

## Findings (by severity)

### HIGH

---

#### 1. Integer overflow in archive-member header bounds check → huge OOB read
**Location:** `tools/linker/s32-ld.c:232` (`load_object_from_memory`)
**Trigger:** A `.s32a` archive member whose `.s32o` header has e.g. `nsections=0x08000000` (so `nsections*32` wraps to 0) with a small `sec_offset`.

`sec_end = sec_offset + nsections * sizeof(s32o_section_t)` is assigned to a `uint32_t`, so the product (computed in 64-bit `size_t`) is **truncated** to a small value and the `sec_end > size` guard at line 235 passes. The subsequent `memcpy(..., data + sec_offset, nsections * sizeof(...))` (line 242) uses the **full untruncated** 64-bit length — a ~4GB read from a tiny member buffer. Same pattern for symbols (lines 249–251) and the string table. `calloc`/`malloc` returns are also unchecked. Because the `memcpy` destination and source size both derive from the same count, the dominant outcome is a massive OOB read and/or NULL-deref crash rather than a controllable write, hence high rather than critical.

**Fix:** Do all bounds math in `uint64_t` and reject if any term exceeds `size`, e.g. `if ((uint64_t)sec_offset + (uint64_t)nsections * sizeof(s32o_section_t) > size) fail;`. Cap `nsections`/`nsymbols`/`str_size` to sane maxima and check every allocation result.

---

#### 2. Regular-file object loader performs zero validation; `total_relocs` overflow → heap OOB write
**Location:** `tools/linker/s32-ld.c:518` (`load_object_file`)
**Trigger:** A `.s32o` passed on the command line (any file lacking the archive magic falls back here via `load_archive_file`) with two sections each declaring `nrelocs≈0x80000001`.

Unlike its archive sibling, `load_object_file` never validates header offsets/counts against the file size. The concrete heap overflow: `total_relocs` (uint32_t, lines 548–551) sums attacker-controlled per-section `nrelocs` and **wraps** (e.g. `0x80000001 + 0x80000001 → 2`), so `calloc(total_relocs, ...)` under-allocates. The loop at 555–564 then `fread`s `nrelocs` entries per section into the undersized buffer; `fread` writes as many bytes as the file actually supplies **before** the short-read check at 559 fires — an attacker-controlled heap buffer overflow. `calloc`/`malloc` returns are unchecked throughout.

**Fix:** Stat the file and validate `sec_offset+nsections*32`, `sym_offset+nsymbols*16`, `str_offset+str_size`, and each `reloc_offset+nrelocs*16` against it in 64-bit math (mirror the archive path). Accumulate `total_relocs` in `uint64_t` and reject overflow.

---

#### 3. `safe_string` does not guarantee NUL termination → OOB read
**Location:** `tools/linker/s32-ld.c:195`
**Trigger:** A `.s32o`/`.s32a` whose string table's final byte is non-zero with a `name_offset` at/near `str_size`.

`safe_string` checks only `offset >= str_size`, then returns `&string_table[offset]`. The buffer is exactly `str_size` bytes (`malloc(str_size)` at 254/537) with no appended NUL and no verification that the final byte is zero. Every caller applies `strcmp`/`strlen`/`strdup`/`snprintf("%s")` to the result (`build_symbol_table:701`, `is_symbol_undefined:311/320`, `merge_sections:653`, `collect_relocations:1406`, `create_eh_frame_hdr:1448`), reading past the heap buffer until an unrelated NUL. (The `str_size==0` sub-case is inert — `offset >= 0` rejects all offsets.)

**Fix:** Allocate `str_size+1`, zero the last byte, and treat `str_size` as the readable length. Optionally bound returned strings with `strnlen` against remaining buffer.

---

#### 4. Archive symbol `member_index` used to index `loaded[]`/`members[]` without bounds check
**Location:** `tools/linker/s32-ld.c:439` (`load_archive_file`)
**Trigger:** A `.s32a` whose symbol-index entry has `member_index >= nmembers` (e.g. `0xFFFFFFFF`).

`member_idx = symbols[i].member_index` is read raw from the untrusted symbol index and used directly as `loaded[member_idx]` (442, 474) and `members[member_idx]` (447) with no check that `member_idx < nmembers`. `loaded` is `calloc(nmembers)` and `members` is `malloc(nmembers*24)`. An OOB array read then dereferences `member->name_offset/offset/size` from arbitrary heap memory, driving wild `fseek`/`fread` and likely crash. Notably, surrounding code bounds-checks other untrusted fields (`safe_string`, section index), confirming this is an oversight.

**Fix:** After reading the symbol index, validate every `member_index < hdr.nmembers`; reject the archive otherwise, and re-check inline before each use.

---

#### 5. Archive string-table `name_offset` used without bounds check
**Location:** `tools/linker/s32-ld.c:438` (`load_archive_file`)
**Trigger:** A `.s32a` whose symbol or member `name_offset >= str_size`, or whose string table lacks a trailing NUL.

`strings = malloc(hdr.str_size)`; `symbols[i].name_offset` (438) and `member->name_offset` (448) are added to `strings` with no check against `str_size`. The pointers flow into `is_symbol_undefined`→`strcmp` (445) and `snprintf("%s")` (462), reading past the buffer. The same file already has a `safe_string()` helper used on the object-file path — the archive symbol/member path bypasses it entirely. (One verdict argued low on build-tool trust grounds; the other holds high given a clean attacker-controlled OOB read and the available-but-unused safe helper.)

**Fix:** Validate `name_offset < str_size` for every symbol and member (route through `safe_string`-style checks) and ensure `strings` is NUL-terminated (`str_size+1`, zero the extra byte).

---

#### 6. Archive member section offsets validated with overflow-prone 32-bit math → OOB read
**Location:** `tools/linker/s32-ld.c:261` (`load_object_from_memory`)
**Trigger:** A crafted member `.s32o` with a section `offset=0xFFFFFF00, size=0x200` so `offset+size` wraps below `size`.

The per-section data check `sections[i].offset + sections[i].size > size` adds two `uint32_t` fields with no `sizeof` operand, so the addition stays in 32-bit and wraps mod 2³². A wrapping pair passes the `> size` test; the subsequent `memcpy(section_data[i], data + offset, size)` (265–266) reads far past the member buffer.
**Note:** The reloc check at 280–282 is **not** vulnerable — `sizeof(s32o_reloc_t)` promotes that expression to 64-bit, so the original "nrelocs*16 can overflow independently" sub-claim is incorrect on the 64-bit build. Only the line-261 section check wraps.

**Fix:** Compute the section-extent comparison in 64-bit (`(uint64_t)offset + size > size`); bound section size to the member size before use.

---

#### 7. Object-file symbol `name_offset` unbounded → OOB read in `build_symbol_index`
**Location:** `tools/utilities/s32-ar.c:196`
**Trigger:** Running `s32-ar r/c/d/m` over an archive whose member `.s32o` has a global symbol with `name_offset >= str_size` (or pointing at non-NUL-terminated bytes).

`build_symbol_index` validates the symbol-table extent (184) and string-table extent (185) against `member->size`, but never validates each symbol's `name_offset`. Line 196 does `strdup(obj_strings + symbols_in[i].name_offset)`; `strdup` reads/copies OOB heap memory until a NUL. The over-read bytes are copied into the **new archive's string table** — both a crash risk and a heap-memory info disclosure into the written file. Reachable from every mutating op via `write_archive → build_symbol_index`. The file already bounds-checks the analogous *archive* member names but omits the object-file symbol path.

**Fix:** Before line 196, check `name_offset < hdr->str_size` and that a NUL exists within `[str_offset, str_offset+str_size)`; skip the symbol otherwise. Bound the `strdup` by remaining `str_size`.

---

#### 8. Stack buffer overflow in string tokenizer via unchecked backslash-escape write
**Location:** `tools/assembler/slow32asm.c:117` (`scanner_next`)
**Trigger:** An assembly line with a string operand containing >~127 backslash-escape pairs, e.g. `.word "\a\a\a..."` or `li r1, "\a\a..."`.

`sval` is `char[256]` embedded in a stack-local `scanner_t`. The copy loop bounds **only the second write per iteration** (`if (i < 254) ...` at 119); the escape-branch write at line 117 (`s->curr.sval[i++] = *s->p++;`) has **no** bounds check. Once `i` reaches 254, line 119 stops contributing but line 117 keeps writing `sval[254], sval[255], sval[256], ...`, and the trailing closing-quote/NUL writes (122–123) go further. **Reproduced under AddressSanitizer**: a stack-buffer-overflow WRITE at `slow32asm.c:117` via `parse_expression_all → assemble_line → main`, crashing the process. Reachable from any `.word`/`.uleb128`/`li`/`la`/operand path. This is a genuine controlled stack overflow from untrusted assembly source.

**Fix:** Bound every write into `sval` — apply the `i < 254` (or `sizeof(s->curr.sval)`) guard to the escape branch as well, and guard the closing-quote write.

---

#### 9. DBT code-buffer overflow never detected or flushed → OOB writes
**Location:** `tools/dbt/translate.c:5761` (`translate_block_cached`)
**Trigger:** An untrusted guest binary that translates enough distinct basic blocks (~40K–100K) to fill the 4MB JIT code buffer within one flush epoch.

`cache_get_code_ptr()` always returns `code_buffer+offset` and **never** NULL, so the "buffer full — flush and retry" branches (5748–5757) are dead code; the bounds-checked `cache_alloc_code()` is never called. The only flush trigger, `cache_needs_flush()`, checks **block count** (≥98304), never code-buffer fill — and the 4MB buffer fills first whenever average block size exceeds ~42.7 bytes (always true in practice). `emit_byte` keeps incrementing `offset` past capacity (setting `overflow`), but the cached path never inspects `e->overflow`. Three consequences: (1) `emit_deferred_side_exits → emit_patch_rel32` does an unchecked `memcpy(buf+patch_offset, &rel, 4)` past the mapping; (2) peephole/nop-compact passes operate over an out-of-range `host_size`; (3) `cache_commit_code` sets `code_buffer_used` past `code_buffer_size` (feeding finding #10).

**Fix:** Before translating, compute `remaining = code_buffer_size - code_buffer_used` and flush if below a conservative worst-case block budget. After translation and after `emit_deferred_side_exits`, check `e->overflow`; if set, flush and restart instead of committing. Bounds-check `emit_patch_rel32`. Have `cache_get_code_ptr`/`cache_alloc_code` actually signal NULL on insufficient space.

---

#### 10. `emit_init` capacity underflows to ~4GB after a commit overshoot
**Location:** `tools/dbt/block_cache.c:565` (`cache_commit_code`)
**Trigger:** A prior block whose emission overflowed the code buffer (finding #9) commits `host_size` such that `code_buffer_used > code_buffer_size`.

`cache_commit_code` sets `code_buffer_used = aligned_offset + size` with **no clamp** against `code_buffer_size`. Both fields are `uint32_t`, so the next block's `emit_init` capacity `code_buffer_size - code_buffer_used` **wraps to ~0xFFFFFFFF**, defeating `emit_byte`'s bounds check and turning every subsequent emit into an unbounded OOB write past the 4MB RWX mapping. This is the amplifier escalating a single bounded one-block overflow into arbitrary far OOB writes; the truncated/garbage block is also cached and later executed.

**Fix:** In `cache_commit_code`, if `aligned_offset + size > code_buffer_size`, treat as overflow (flush / refuse to commit) rather than storing an out-of-range count. Compute remaining space with a guarded form: `remaining = (used >= size) ? 0 : (size - used)`.

---

#### 11. OOB write in SSA overlay: `value_def_node[]` sized by node count but indexed by value-id
**Location:** `tools/dbt/stage5_ssa.c:76` (`stage5_ssa_build_overlay`)
**Trigger:** A lifted guest superblock with enough (~225+) `rd`-writing instructions that the SSA value-id exceeds 256.

`value_def_node` is declared `uint16_t[STAGE5_MAX_IR_NODES]` (256) but indexed by SSA **value id** `v`, not node index. The companion `value_*` arrays are correctly sized `STAGE5_SSA_MAX_VALUES` (832); only this one is mis-sized, and the only guard checks against 832, not 256. `value_def_node` is the last struct field, so `value_def_node[v]` for `v > 256` writes past the struct end into adjacent (stack-allocated) memory. (One verdict refuted on the basis that `STAGE5_LIFT_BUDGET=64` caps lifting; the confirming verdict notes `ir_count` is bounded to 256 by the lifter and the per-write value-id can reach ~289, giving a bounded but real ~33-entry overrun. Net: confirmed real with bounded magnitude.)

**Fix:** Size `value_def_node` as `uint16_t[STAGE5_SSA_MAX_VALUES]` (matching the other per-value-id arrays) in `stage5_ssa.h:27`; the `memset` adjusts automatically via `sizeof`. Add a defensive `if (v < STAGE5_SSA_MAX_VALUES)` guard.

---

### MEDIUM

---

#### 12. Unaligned word MMIO access reads/writes up to 3 bytes past the 64KB mmap
**Location:** `tools/emulator/mmio_ring.c:984` (write) and `:934` (read) — *deduplicated read/write mirror of one root cause*
**Trigger:** Guest executes an unaligned 4-byte `stw`/`ldw` at `mmio_base+0xFFFD` (or `+0xFFFE`/`+0xFFFF`).

The MMIO window is a single `mmap` of exactly `0x10000` bytes. The data-buffer branch's gate only requires `rel < 0x10000`, not `rel + 4 <= 0x10000`. So `rel = 0xFFFD..0xFFFF` yields a 4-byte `memcpy` whose end (`0xC001..0xC003` in `data_buffer`) runs 1–3 bytes past the mmap. Unaligned word accesses are not trapped: `S32_TRAP_ON_UNALIGNED` defaults to 0 in `slow32-fast`, and `slow32.c` never consults it on the LD/ST path. Byte/halfword paths word-align via `& ~3` and are safe. Because `0x10000` is page-aligned, the overflow most likely faults on the following guard page (DoS) or, for reads, leaks up to 3 adjacent host bytes into a guest register.

**Fix:** Tighten both gates to be width-aware: reject when `rel + size > S32_MMIO_DATA_BUFFER_OFFSET + S32_MMIO_DATA_CAPACITY` (data branch: `offset + 4 <= S32_MMIO_DATA_CAPACITY`). Cleanest: a single shared helper validating `rel + size <= window_size` for every MMIO sub-region, or force word alignment (`addr &= ~3u`) on the size==4 path as already done for byte/halfword.

---

#### 13. Non-NUL-terminated symbol string table → OOB read when symbol names printed
**Location:** `tools/emulator/s32x_loader.h:451` (`load_object_from_s32x_symtab`)
**Trigger:** A crafted `.s32x` with a SYMTAB+STRTAB whose symbol string table has no terminating NUL, run under `slow32-fast`.

`sym_str = malloc(sym_strtab_size)` + `fread` with no appended NUL. Each symbol name is `&sym_str[name_offset]` guarded only by `name_offset < sym_strtab_size` (bounds the start, not the end). These pointers flow into `build_sorted_symtab` (called unconditionally) and `resolve_symbol`, then into `fprintf("...<%s+...>", sym)` at `slow32-fast.c:1339` (dump_registers, fired on SIGINT/Ctrl+C) and `:1431` (probe). `%s` walks past the heap allocation. The symtab is built on every run; only the printing sink requires a profiling/SIGINT event.

**Fix:** Allocate `sym_strtab_size+1` and set the final byte to 0 after `fread`; additionally require a NUL within the table for the chosen `name_offset` before exposing the pointer.

---

#### 14. Symbol section index not upper-bounds-checked before indexing `section_base[]`
**Location:** `tools/linker/s32-ld.c:736` (`build_symbol_table`); same pattern at 787 and `collect_relocations:1388`
**Trigger:** A `.s32o` with a defined symbol whose 16-bit `section` field is `> nsections` (e.g. `0xFFFF`) while `nsections` is small.

The symbol `section` field is `uint16_t` (0..65535) read verbatim. Both the update branch (736) and new-symbol branch (787) compute `value + section_base[section - 1]`, guarded only by `section != 0`. `section_base` is `calloc(nsections)`. The `<= nsections` guard (742/790) comes **after** the deref and only protects the `safe_string` call. The OOB value is used as a numeric section base (not dereferenced as a pointer), so impact is a heap OOB read corrupting the link output / possible crash, not a write — hence medium.

**Fix:** Before any `section_base[section - 1]` access, require `section >= 1 && section <= nsections`; treat violations as a malformed-file error at all three sites.

---

#### 15. 32-bit overflow in archive table bounds checks → unchecked counts to `malloc`/OOB read
**Location:** `tools/utilities/s32-ar.c:233` (and 268/447/475/529/560)
**Trigger:** A `.s32a` with e.g. `str_offset=0xFFFFFFFF, str_size=2` (wraps), or `mem.offset=0xFFFFFFFC, size=8`.

`hdr.str_offset + hdr.str_size > (uint32_t)archive_size` and `mem.offset + mem.size > archive_size` are 32-bit unsigned additions that wrap, defeating the bounds check for out-of-file regions. The headline "count*size multiplication overflows → undersized buffer → heap write" mechanism is **refuted on the 64-bit build** (`sizeof` promotes those products to 64-bit; huge `malloc` just returns NULL). The genuine residual impact: defeated extent checks plus the unbounded `member_index`/`name_offset` issues (#7, separate) yield OOB reads and bogus sizes fed to `malloc`. `fread`/`malloc` returns are also unchecked.

**Fix:** Perform all extent checks in 64-bit (or check operands separately: `str_offset > size || str_size > size - str_offset`). Check `malloc`/`fread` returns and fail on short reads.

---

#### 16. Section `name_offset` never bounds-checked → OOB read in `printf %s` (slow32dis)
**Location:** `tools/utilities/slow32dis.c:609` (.s32x) and `:651` (.s32o)
**Trigger:** A `.s32x`/`.s32o` with a section `name_offset >= str_size` (e.g. `0xFFFFFFFF`) or pointing at non-NUL-terminated bytes.

`name = &strtab[sect.name_offset]` with no check that `name_offset < str_size`; `strtab` is `malloc(str_size)` and not NUL-terminated. The pointer reaches `printf("%s")` in `disassemble_section` (line 493). **Reproduced under ASan**: SEGV READ in `strlen` via `printf` reached from `main`. The symbol-loading path in the same file *does* bounds-check (lines 408/463), making this an inconsistent omission.

**Fix:** Validate `name_offset < str_size` (fall back to `"<invalid>"` as `slow32dump` does); validate `str_offset+(uint64_t)str_size` and `nsections*sizeof(section)` against file size up front; force-NUL-terminate the buffer.

---

#### 17. Unchecked `fread` returns leave header/section/strtab buffers uninitialized (slow32dis)
**Location:** `tools/utilities/slow32dis.c:593` (and 635/647)
**Trigger:** A truncated `.s32x`/`.s32o` whose header advertises large `str_size`/`nsections` but whose body ends early.

`fread(strtab, 1, str_size, f)` and `fread(sects, sizeof, nsections, f)` ignore their return values; buffers are `malloc`'d (not `calloc`'d) and never validated against real file size. A short file leaves them partly/fully uninitialized, and the code then reads `section.type/size/offset/name_offset` and string bytes from uninitialized heap — feeding the OOB behaviors above. The sibling `load_symbols_s32o` *does* check `fread != count`, confirming the omission.

**Fix:** Check every `fread` return against the requested count and bail on mismatch. Validate `str_offset`/`str_size`/`sec_offset`/`nsections*entry_size` against the `stat`'d file size before allocating.

---

#### 18. Member name from string table used without NUL-termination guarantee (s32-ar)
**Location:** `tools/utilities/s32-ar.c:287` (and `list_archive:487`, `extract_archive:567`)
**Trigger:** A `.s32a` whose string-table final byte is non-NUL with a member `name_offset` pointing near the end (`< str_size` passes, but no NUL within the buffer).

`strings = malloc(str_size)` read raw with no appended NUL; the only guard is `name_offset < str_size`. `strings + name_offset` is then treated as a C string by `basename_simple` (which `strrchr`s to a NUL), `printf("%s")`, `strcmp`, and `fopen(...,"wb")`. Without an in-buffer terminator these read past the heap allocation — info leak via printed/extracted name or crash.

**Fix:** Allocate `str_size+1`, force `strings[str_size]=0` after reading; or verify a NUL exists in `[name_offset, str_size)` before use.

---

#### 19. String table not NUL-terminated → OOB read printing section/symbol names (slow32dump)
**Location:** `tools/utilities/slow32dump.c:195` (and 213/237/264/363/391)
**Trigger:** A `.s32o`/`.s32x` whose string table's last byte is non-NUL with a name pointing to a string running to the end.

`strtab = malloc(str_size)` read raw; names validated only as `name_offset < str_size`, then printed with `%s`. When `strtab[str_size-1]` is non-zero, `printf` walks past the buffer. (`str_size==0` is safe — `name_offset < 0` rejects all.) Bounded heap over-read in a short-lived CLI: adjacent-heap leak to stdout or a crash.

**Fix:** Allocate `str_size+1` and set `strtab[str_size]=0`; use 64-bit arithmetic for the `str_offset+str_size` bounds check.

---

#### 20. Undefined/large shift in `.align` → capacity-check overflow → OOB heap write/OOM
**Location:** `tools/assembler/slow32asm.c:1631`
**Trigger:** A `.align N` directive with large or negative `N` (e.g. `.align 31`, `.align -1`), with a preceding non-aligned address (e.g. a `.byte`).

`align_power = parse_immediate(tokens[1])` is unvalidated; `int align_bytes = 1 << align_power` is UB for `align_power >= 31` or negative. For `.align 31`, `padding` becomes ~2.1 billion. **Reproduced**: `ensure_instruction_capacity`'s `needed = num_instructions + padding` integer-overflows to negative, so the `needed <= capacity` guard passes, the buffer is **not** grown, and the per-byte emission loop writes ~2.1B `instruction_t` entries past the small heap buffer — an OOB heap **write** (reproduced as SIGSEGV). Lower powers (e.g. `.align 28`) instead trigger a multi-GB realloc → OOM. The sibling `.balign` and `.zero/.space` paths take similar unvalidated counts.

**Fix:** Validate `align_power` is in `[0, 30]` before shifting; validate `padding`/byte counts against a sane maximum before allocation; harden `ensure_instruction_capacity` against additive overflow of `needed`. Clamp `.balign`/`.zero`/`.space` similarly.

---

#### 21. f64 register-pair opcodes with reg==31 read/write `regs[32]` (one past array)
**Location:** `tools/dbt/shadow_interp.c:443` (and 484/488/499/501/506/511/524/529/533/537)
**Trigger:** Paranoid/shadow mode (`-P`) executing a guest block with an f64 opcode (0x61–0x78) whose `rd`/`rs1`/`rs2` field equals 31.

`regs` is `uint32_t[32]`; the f64 opcodes access `r[reg+1]`. With `reg==31`, `r[32]` is one element past the array. The reference emulators guard this exact access with `CHECK_F64_REG` (`reg >= 31 || (reg & 1)`), enabled by default; the shadow interpreter omits it. `r[32]` aliases the adjacent in-struct `pc` field, so it is a bounded one-element overrun into a neighboring struct member (corrupts/leaks the shadow PC), and only reachable under the opt-in `-P` mode — hence medium rather than high.

**Fix:** Mirror the reference guard — for f64 opcodes, validate `reg < 31 && (reg & 1) == 0` before forming the pair (or mask the `+1` index to 0..31).

---

### LOW

---

#### 22. Non-NUL-terminated section name table → OOB read under `-t`/verbose
**Location:** `tools/emulator/s32x_loader.h:241` (`load_s32x_file`)
**Trigger:** A crafted `.s32x` whose section-name string table has no NUL, loaded with `slow32 -t` (verbose/trace).

`strtab = malloc(str_size)` + `fread` with no NUL guarantee. `name = &strtab[section.name_offset]` (guarded only at the start by `name_offset < str_size`) is passed to `printf("...'%s'...")` at 247/253. Without a NUL, `printf` walks past the heap allocation. Gated entirely behind `config->verbose`, which is only set by `-t` (trace) — off the default execution path.

**Fix:** Allocate `str_size+1`, force `strtab[str_size]=0`; verify a NUL exists within `[name_offset, str_size)` before use (else `"?"`). Apply the same to the symbol string table (#13).

---

#### 23. Unvalidated header limits feed `mm_setup_from_s32x` → uint32 wrap (hardening)
**Location:** `tools/emulator/memory_manager.h:181`
**Trigger:** A crafted `.s32x` header with `stack_base` near `0xFFFFFFFF` (so `stack_base+0x10` wraps below `stack_end`), or huge `rodata_limit`/`stack_end`.

`load_s32x_header` validates only `code_limit`; `rodata_limit`/`data_limit`/`stack_base`/`stack_end`/`mmio_base` reach `mm_setup_from_s32x` unchecked. `stack_top = stack_base + 0x10` can wrap and `stack_top - stack_end` underflow to ~4GB. **The two verdicts split** (real-low vs refuted): the giant-size cases hit `mmap` `MAP_FAILED` and abort cleanly, and the descriptor invariant `vaddr_end == vaddr_start + mmap_size` is preserved (region/length derive from the same `aligned_size`), so no in-range guest access escapes the allocation. This is a **hardening gap / undefined-shaped arithmetic on attacker input**, not a demonstrated memory-safety violation — hence low.

**Fix:** Validate all memory-layout fields in `load_s32x_header`/`load_s32x_file` before region setup: `code_limit <= rodata_limit <= data_limit <= mem_size`, `stack_end < stack_base <= mem_size`, a sane `mem_size` cap, and reject `vaddr+size`/`aligned_vaddr+aligned_size` overflow in `mm_allocate_region`.

---

#### 24. Unchecked `malloc` of attacker-controlled size → NULL deref/crash (s32-ar)
**Location:** `tools/utilities/s32-ar.c:145` (and 275/579)
**Trigger:** A member entry or input file declaring a very large size, processed by `r`/`x`.

`malloc(st.st_size)` / `malloc(mem.size)` results are passed to `read_fully` (`fread(dst,1,size,f)`) without NULL checks; allocation failure yields a NULL deref. `mem.size` is attacker-controlled from the archive member table (bounded by the on-disk archive size, so a deliberately huge declared size is rejected at the 32-bit extent check, but a genuinely large multi-GB archive still fails `malloc`). Impact is a clean crash/DoS in a local CLI build tool — no corruption.

**Fix:** Check each `malloc` result for NULL and error out; additionally sanity-bound `mem.size`/`st.st_size` against the actual file size before allocating.

---

#### 25. f64 register-pair `regs[reg+1]` one past array, aliases shadow `pc` (dbt5)
**Location:** `tools/dbt5/shadow_interp.c:499` (and the f64 0x61–0x78 cases / `SHADOW_LOAD_F64`/`SHADOW_STORE_F64`)
**Trigger:** Hostile guest code encoding an f64 opcode with `rs1`/`rs2`/`rd` field = 31, executed by the shadow interpreter.

Identical pattern to #21 in the dbt5 tree: `r[reg+1]` with `reg` masked only to 0..31, no even-pair/`reg<31` guard (which `dbt5_fp_helper.c` *does* enforce). `regs[32]` is immediately followed by `pc` within `shadow_state_t`, so the access stays inside the struct allocation — worst case corrupts/leaks the shadow PC (wrong shadow result), not host memory. Low.

**Fix:** Mirror the helper's guard (`reg < 31 && (reg & 1) == 0`) in `shadow_step` for f64 opcodes, or mask the `+1` index.

---

## Recommended Remediation Order

1. **Assembler stack overflow (#8, `slow32asm.c:117`)** — the only confirmed, ASan-reproduced **controlled stack-buffer-overflow write** from untrusted source text; a one-line guard fixes it. Highest exploitability-to-effort ratio.
2. **DBT JIT overflow chain (#9 + #10 together, `translate.c` / `block_cache.c`)** — fix as a unit: detect/flush on code-buffer fill, honor `e->overflow`, and clamp `cache_commit_code`. This closes the ~4GB-underflow → unbounded-RWX-write primitive.
3. **Linker heap-write paths (#2 `total_relocs` overflow, then #1 archive header overflow)** — attacker-controlled heap overflow / massive OOB read from object files and archives a build pipeline routinely consumes.
4. **Linker OOB-read cluster (#3, #4, #5, #6) + assembler `.align` (#20)** — apply the shared remedies in one pass: 64-bit bounds math, `str_size+1` NUL termination + `safe_string` everywhere, `member_index`/`name_offset`/`section` upper-bound checks, and `.align`-power validation (#20 is an OOB heap write and should ride along here despite its medium rating).
5. **`s32-ar` symbol-name OOB (#7)** — heap info-disclosure into written archives; route through a bounds-checked lookup.
6. **Emulator MMIO bound (#12)** — width-aware gate (single shared `rel + size <= window_size` helper) closes both read and write mirrors.
7. **Shared "string table not NUL-terminated / unchecked fread" remediation across utilities (#13, #16, #17, #18, #19, #22)** — implement one hardened loader idiom (`malloc(size+1)`, force trailing NUL, check `fread` returns, validate `name_offset < str_size` and offsets vs file size) and apply it uniformly to `s32x_loader.h`, `slow32dis.c`, `slow32dump.c`, and `s32-ar.c`.
8. **Defense-in-depth / low (#14 numeric, #21, #23, #24, #25)** — add the missing bounds/even-pair/NULL/limit checks; low individual impact but cheap and they remove latent escalation paths (e.g. #23's wrap becomes live if region invariants ever change).