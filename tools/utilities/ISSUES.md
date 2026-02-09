# SLOW-32 Utilities — Issues & Recommendations

This document tracks bugs, architectural limitations, and potential improvements identified during the code review of the SLOW-32 utility tools (`s32-ar`, `slow32dis`, and `slow32dump`).

## Critical Bugs & Safety Issues

### 1. `s32-ar` Memory Leaks
The archiver `s32-ar.c` contains multiple memory leaks:
- `build_symbol_index` calls `strdup` for every global symbol found in every object file, but `write_archive` only frees the `symbols` array and the `name` strings at the very end of the program (lines 485-488). However, if an archive has thousands of symbols, this can consume significant memory during the build process.
- In `load_archive_members`, `strings` and `data` are allocated but not always freed in all error paths.
- **Recommendation**: Ensure consistent use of `free()` in all paths, especially for string allocations and temporary member data.

### 2. `slow32dis` Buffer Overrun in `get_reg_name`
The disassembler uses a rotating static buffer for register names: `static char bufs[4][8]`.
- **Problem**: `sprintf(buf, "r%d", reg)` is used without checking if the result fits in 8 bytes. While register numbers are 0-31 (fitting in 4 bytes: "r31\0"), any future expansion or unexpected register index could overflow the 8-byte buffer.
- **Recommendation**: Increase buffer size to 16 bytes and use `snprintf`.

### 3. `slow32dump` and `slow32dis` Lack of Bounds Checking on Section Data
Both utilities `malloc` a buffer based on `sect.size` and then `fread` into it.
- **Problem**: If `sect.size` is maliciously large or corrupted in the file header, it can lead to massive allocations or failed reads. Furthermore, `fread(code, 1, sect.size, f)` in `slow32dis` doesn't check the return value.
- **Recommendation**: Add a sanity cap on section sizes and always verify `fread` return values.

### 4. `s32-ar` and `slow32dump` Invalid Offset Risks
Both tools calculate pointer offsets using values read directly from file headers:
- `symbols_in = (s32o_symbol_t *)(member->data + hdr->sym_offset)` in `s32-ar`.
- **Problem**: If `hdr->sym_offset` is larger than `member->size`, this will lead to an immediate crash or out-of-bounds read.
- **Recommendation**: Perform rigorous bounds checking on all header offsets before using them as pointer math or in `fseek`.

---

## Architectural Limitations

### 5. `slow32dis` Only Disassembles `.s32x` (Resolved)
The `main` function only handled `S32X_MAGIC`.
- **Status**: Fixed. `slow32dis` now auto-detects `.s32x` vs `.s32o` by reading the magic number and handles both formats. Object file disassembly uses section-relative addresses (vaddr=0) and filters symbols to code sections only.

### 6. `s32-ar` Minimal Symbol Indexing
The current archiver only indexes `S32O_BIND_GLOBAL` symbols that are not in section 0 (undefined).
- **Note**: This is correct for basic linking, but standard `ar` tools often include more metadata.
- **Recommendation**: Consider supporting WEAK symbols in the index to allow the linker to make better decisions.

---

## Quality of Life & Usability

### 7. Inconsistent Register Naming
`slow32dis` and `slow32asm` use a mix of ABI names (`zero`, `sp`, `fp`, `lr`) and raw names (`r0`..`r31`).
- **Recommendation**: Support a `--abi` flag in the disassembler to show standard ABI names (`a0`, `s1`, etc.) for all registers to match compiler output.

### 8. `slow32dump` Verbosity Control
The `-v` (verbose) flag exists but many sections are dumped in full detail even without it.
- **Recommendation**: Use `-v` to gate detailed hex dumps or extensive symbol metadata, keeping the default output concise.

### 9. `s32-ar` Missing "Delete" and "Update" Features
The current tool primarily supports `c` (create/replace) and `t` (list).
- **Opportunity**: Standard `ar` supports deleting members or updating them based on timestamps.
- **Recommendation**: Implement `d` (delete) and `u` (update) commands.

### 10. Disassembler Missing Branch/Jump Target Resolution (Resolved)
`slow32dis` showed the raw hex address for branch targets.
- **Status**: Fixed. The disassembler now reads the symbol table from both `.s32x` (SYMTAB/STRTAB sections) and `.s32o` (header symbol table) files. Function labels are printed before instructions at symbol addresses, and JAL/branch targets are annotated with symbol names when available.
