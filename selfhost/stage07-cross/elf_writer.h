// elf_writer.h — Minimal ELF64 static executable writer
//
// Produces a standalone Linux x86-64 ELF binary with:
//   - .text  (R+X) — executable code
//   - .rodata (R)  — string literals, constants
//   - .data  (R+W) — initialized globals
//   - .bss   (R+W) — uninitialized globals (zero-filled by kernel)
//
// No dynamic linking, no GOT/PLT, no DWARF, no section headers (optional).
// The output is a fully static executable that depends only on Linux syscalls.
//
// Usage:
//   elf_init();
//   // ... fill x64_buf[] with code via x64_encode.h ...
//   elf_set_text(x64_buf, code_len);
//   elf_set_rodata(rodata_buf, rodata_len);
//   elf_set_data(data_buf, data_len);
//   elf_set_bss(bss_len);
//   elf_set_entry(entry_offset);  // offset within .text
//   elf_write("output");

#ifndef ELF_WRITER_H
#define ELF_WRITER_H

// ============================================================================
// ELF64 constants
// ============================================================================

// ELF identification
#define EI_NIDENT    16

// e_type
#define ET_EXEC      2

// e_machine
#define EM_X86_64    62

// p_type
#define PT_LOAD      1

// p_flags
#define PF_X         1
#define PF_W         2
#define PF_R         4

// ============================================================================
// Virtual address layout
//
// Linux default: executables load at 0x400000.
// We use a simple layout with 4K-aligned segments:
//   0x400000 + 0x000: ELF header + program headers
//   0x401000: .text  (R+X)
//   0x402000+: .rodata (R) — after text, page-aligned
//   0x403000+: .data + .bss (R+W) — after rodata, page-aligned
// ============================================================================

#define ELF_BASE_ADDR     0x400000
#define ELF_PAGE_SIZE     0x1000

// ============================================================================
// State
// ============================================================================

static unsigned char *elf_text_buf;
static int elf_text_len;

static unsigned char *elf_rodata_buf;
static int elf_rodata_len;

static unsigned char *elf_data_buf;
static int elf_data_len;

static int elf_bss_len;
static int elf_entry_off;   // offset within .text

// Output buffer — we build the entire ELF in memory then write once
#define ELF_OUT_SIZE 8388608
static unsigned char elf_out[ELF_OUT_SIZE];
static int elf_out_len;

static void elf_init(void) {
    elf_text_buf = 0;
    elf_text_len = 0;
    elf_rodata_buf = 0;
    elf_rodata_len = 0;
    elf_data_buf = 0;
    elf_data_len = 0;
    elf_bss_len = 0;
    elf_entry_off = 0;
    elf_out_len = 0;
}

static void elf_set_text(unsigned char *buf, int len) {
    elf_text_buf = buf;
    elf_text_len = len;
}

static void elf_set_rodata(unsigned char *buf, int len) {
    elf_rodata_buf = buf;
    elf_rodata_len = len;
}

static void elf_set_data(unsigned char *buf, int len) {
    elf_data_buf = buf;
    elf_data_len = len;
}

static void elf_set_bss(int len) {
    elf_bss_len = len;
}

static void elf_set_entry(int offset) {
    elf_entry_off = offset;
}

// ============================================================================
// Output helpers
// ============================================================================

static void elf_emit_byte(int b) {
    if (elf_out_len < ELF_OUT_SIZE)
        elf_out[elf_out_len] = (unsigned char)b;
    elf_out_len++;
}

static void elf_emit_half(int v) {
    elf_emit_byte(v & 0xFF);
    elf_emit_byte((v >> 8) & 0xFF);
}

static void elf_emit_word(int v) {
    elf_emit_byte(v & 0xFF);
    elf_emit_byte((v >> 8) & 0xFF);
    elf_emit_byte((v >> 16) & 0xFF);
    elf_emit_byte((v >> 24) & 0xFF);
}

// Emit 64-bit value as two 32-bit halves
static void elf_emit_xword(int lo, int hi) {
    elf_emit_word(lo);
    elf_emit_word(hi);
}

static void elf_emit_bytes(unsigned char *src, int len) {
    int i;
    for (i = 0; i < len; i++)
        elf_emit_byte(src[i]);
}

// Pad to alignment
static void elf_emit_pad(int target_off) {
    while (elf_out_len < target_off)
        elf_emit_byte(0);
}

// Helper: round up to page boundary
static int elf_page_align(int v) {
    return (v + ELF_PAGE_SIZE - 1) & ~(ELF_PAGE_SIZE - 1);
}

// ============================================================================
// ELF64 Header (64 bytes)
// ============================================================================

static void elf_emit_ehdr(int entry_lo, int entry_hi, int phoff, int phnum) {
    // e_ident[16]
    elf_emit_byte(0x7F);    // magic
    elf_emit_byte('E');
    elf_emit_byte('L');
    elf_emit_byte('F');
    elf_emit_byte(2);       // ELFCLASS64
    elf_emit_byte(1);       // ELFDATA2LSB (little-endian)
    elf_emit_byte(1);       // EV_CURRENT
    elf_emit_byte(0);       // ELFOSABI_NONE
    elf_emit_byte(0); elf_emit_byte(0); elf_emit_byte(0); elf_emit_byte(0);
    elf_emit_byte(0); elf_emit_byte(0); elf_emit_byte(0); elf_emit_byte(0);

    elf_emit_half(ET_EXEC);     // e_type
    elf_emit_half(EM_X86_64);   // e_machine
    elf_emit_word(1);           // e_version = EV_CURRENT

    elf_emit_xword(entry_lo, entry_hi);  // e_entry (64-bit)
    elf_emit_xword(phoff, 0);            // e_phoff (64-bit)
    elf_emit_xword(0, 0);               // e_shoff = 0 (no section headers)

    elf_emit_word(0);           // e_flags
    elf_emit_half(64);          // e_ehsize
    elf_emit_half(56);          // e_phentsize
    elf_emit_half(phnum);       // e_phnum
    elf_emit_half(64);          // e_shentsize (irrelevant, no sections)
    elf_emit_half(0);           // e_shnum
    elf_emit_half(0);           // e_shstrndx
}

// ============================================================================
// Program Header (56 bytes each)
// ============================================================================

static void elf_emit_phdr(int type, int flags,
                          int offset, int vaddr,
                          int filesz, int memsz) {
    elf_emit_word(type);                     // p_type
    elf_emit_word(flags);                    // p_flags
    elf_emit_xword(offset, 0);              // p_offset
    elf_emit_xword(vaddr, 0);               // p_vaddr
    elf_emit_xword(vaddr, 0);               // p_paddr (= vaddr)
    elf_emit_xword(filesz, 0);              // p_filesz
    elf_emit_xword(memsz, 0);               // p_memsz
    elf_emit_xword(ELF_PAGE_SIZE, 0);       // p_align
}

// ============================================================================
// Build and write ELF
// ============================================================================

static int elf_build(void) {
    // Compute layout
    // Headers: ELF header (64) + up to 3 program headers (3 * 56 = 168)
    // Total headers: 64 + 168 = 232 bytes, fits in first page
    int ehdr_size;
    int phdr_size;
    int num_phdr;
    int headers_size;

    int text_file_off;
    int text_vaddr_lo;

    int rodata_file_off;
    int rodata_vaddr_lo;
    int rodata_page;

    int data_file_off;
    int data_vaddr_lo;
    int data_page;
    int data_memsz;

    int entry_lo;

    ehdr_size = 64;
    phdr_size = 56;

    // Count segments: always have text, optionally rodata and data+bss
    num_phdr = 1;  // .text always
    if (elf_rodata_len > 0) num_phdr++;
    if (elf_data_len > 0 || elf_bss_len > 0) num_phdr++;

    headers_size = ehdr_size + phdr_size * num_phdr;

    // .text starts at page boundary after headers
    text_file_off = ELF_PAGE_SIZE;  // file offset
    text_vaddr_lo = (ELF_BASE_ADDR + ELF_PAGE_SIZE) & 0xFFFFFFFF;

    // .rodata after .text (page-aligned)
    rodata_page = elf_page_align(elf_text_len);
    rodata_file_off = text_file_off + rodata_page;
    rodata_vaddr_lo = text_vaddr_lo + rodata_page;

    // .data after .rodata (page-aligned)
    if (elf_rodata_len > 0) {
        data_page = elf_page_align(elf_rodata_len);
        data_file_off = rodata_file_off + data_page;
        data_vaddr_lo = rodata_vaddr_lo + data_page;
    } else {
        data_file_off = rodata_file_off;
        data_vaddr_lo = rodata_vaddr_lo;
    }
    data_memsz = elf_data_len + elf_bss_len;

    // Entry point
    entry_lo = text_vaddr_lo + elf_entry_off;

    // === Emit ===
    elf_out_len = 0;

    // ELF header
    elf_emit_ehdr(entry_lo, 0, ehdr_size, num_phdr);

    // Program header: .text (R+X)
    elf_emit_phdr(PT_LOAD, PF_R | PF_X,
                  text_file_off, text_vaddr_lo,
                  elf_text_len, elf_text_len);

    // Program header: .rodata (R)
    if (elf_rodata_len > 0) {
        elf_emit_phdr(PT_LOAD, PF_R,
                      rodata_file_off, rodata_vaddr_lo,
                      elf_rodata_len, elf_rodata_len);
    }

    // Program header: .data + .bss (R+W)
    if (elf_data_len > 0 || elf_bss_len > 0) {
        elf_emit_phdr(PT_LOAD, PF_R | PF_W,
                      data_file_off, data_vaddr_lo,
                      elf_data_len, data_memsz);
    }

    // Pad to .text file offset
    elf_emit_pad(text_file_off);

    // .text content
    if (elf_text_len > 0)
        elf_emit_bytes(elf_text_buf, elf_text_len);

    // Pad to .rodata file offset
    if (elf_rodata_len > 0) {
        elf_emit_pad(rodata_file_off);
        elf_emit_bytes(elf_rodata_buf, elf_rodata_len);
    }

    // Pad to .data file offset
    if (elf_data_len > 0) {
        elf_emit_pad(data_file_off);
        elf_emit_bytes(elf_data_buf, elf_data_len);
    }

    // BSS is not in the file (memsz > filesz tells the kernel to zero-fill)

    return 0;
}

// Write the ELF to a file.  Returns 0 on success, -1 on error.
// Uses POSIX write() to avoid stdio dependency.
static int elf_write_file(char *filename) {
    int fd;
    int written;
    int total;
    int n;

    elf_build();

    fd = open(filename, 0x241, 0755);  // O_WRONLY|O_CREAT|O_TRUNC, rwxr-xr-x
    if (fd < 0) return -1;

    total = 0;
    while (total < elf_out_len) {
        n = elf_out_len - total;
        written = write(fd, elf_out + total, n);
        if (written <= 0) {
            close(fd);
            return -1;
        }
        total = total + written;
    }

    close(fd);
    return 0;
}

// ============================================================================
// Address computation helpers
//
// After elf_build(), these return the virtual addresses for each section,
// allowing the codegen to compute absolute addresses for global references.
// ============================================================================

static int elf_text_vaddr(void) {
    return (ELF_BASE_ADDR + ELF_PAGE_SIZE);
}

static int elf_rodata_vaddr(void) {
    int rodata_page;
    rodata_page = elf_page_align(elf_text_len);
    return elf_text_vaddr() + rodata_page;
}

static int elf_data_vaddr(void) {
    int v;
    v = elf_rodata_vaddr();
    if (elf_rodata_len > 0)
        v = v + elf_page_align(elf_rodata_len);
    return v;
}

static int elf_bss_vaddr(void) {
    return elf_data_vaddr() + elf_data_len;
}

#endif // ELF_WRITER_H
