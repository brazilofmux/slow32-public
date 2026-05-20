// elf_writer.h — Minimal ELF64 static executable writer (AArch64 target)
//
// Produces a standalone Linux AArch64 ELF binary with:
//   - .text  (R+X)
//   - .rodata (R)
//   - .data  (R+W)
//   - .bss   (R+W, no file image — kernel zero-fills)
//
// No dynamic linking, no GOT/PLT, no DWARF, no section headers in the
// executable (we omit them — only program headers matter at load time).
// The output is fully static: only Linux syscalls are needed at runtime.
//
// Mirror of stage07-cross/elf_writer.h with e_machine = EM_AARCH64.
//
// Usage:
//   elf_init();
//   // ... fill a64_buf[] with code via a64_encode.h ...
//   elf_set_text(a64_buf, code_len);
//   elf_set_rodata(rodata_buf, rodata_len);
//   elf_set_data(data_buf, data_len);
//   elf_set_bss(bss_len);
//   elf_set_entry(entry_offset);
//   elf_write_file("output");

#ifndef ELF_WRITER_H
#define ELF_WRITER_H

#define EI_NIDENT     16
#define ET_EXEC       2
#define EM_AARCH64    183       /* the only meaningful change vs the x64 writer */
#define PT_LOAD       1
#define PF_X          1
#define PF_W          2
#define PF_R          4

/* open(2) flag values vary by OS — Linux and macOS disagree on the bit
 * layout, so we can't hardcode a single numeric literal.  We need
 * O_WRONLY|O_CREAT|O_TRUNC so the second-and-subsequent writes overwrite
 * any existing file instead of partially clobbering or (on macOS, where
 * 0x241 actually means O_WRONLY|O_SHLOCK|O_CREAT) acquiring an advisory
 * lock and skipping truncation. */
#if defined(__APPLE__)
#  define ELFW_OPEN_FLAGS  0x601  /* O_WRONLY|O_CREAT|O_TRUNC on Darwin */
#else
#  define ELFW_OPEN_FLAGS  0x241  /* O_WRONLY|O_CREAT|O_TRUNC on Linux  */
#endif

/* Linux loads static AArch64 executables at 0x400000 (same as x86-64). */
#define ELF_BASE_ADDR 0x400000
#define ELF_PAGE_SIZE 0x1000

static unsigned char *elf_text_buf;
static int            elf_text_len;
static unsigned char *elf_rodata_buf;
static int            elf_rodata_len;
static unsigned char *elf_data_buf;
static int            elf_data_len;
static int            elf_bss_len;
static int            elf_entry_off;

#define ELF_OUT_SIZE 8388608
static unsigned char elf_out[ELF_OUT_SIZE];
static int           elf_out_len;

static void elf_init(void) {
    elf_text_buf   = 0;  elf_text_len   = 0;
    elf_rodata_buf = 0;  elf_rodata_len = 0;
    elf_data_buf   = 0;  elf_data_len   = 0;
    elf_bss_len    = 0;
    elf_entry_off  = 0;
    elf_out_len    = 0;
}

static void elf_set_text(unsigned char *buf, int len)   { elf_text_buf = buf;   elf_text_len = len; }
static void elf_set_rodata(unsigned char *buf, int len) { elf_rodata_buf = buf; elf_rodata_len = len; }
static void elf_set_data(unsigned char *buf, int len)   { elf_data_buf = buf;   elf_data_len = len; }
static void elf_set_bss(int len)                        { elf_bss_len = len; }
static void elf_set_entry(int off)                      { elf_entry_off = off; }

static void elf_emit_byte(int b) {
    if (elf_out_len < ELF_OUT_SIZE)
        elf_out[elf_out_len] = (unsigned char)b;
    elf_out_len = elf_out_len + 1;
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

static void elf_emit_xword(int lo, int hi) {
    elf_emit_word(lo);
    elf_emit_word(hi);
}

static void elf_emit_bytes(unsigned char *src, int len) {
    int i;
    i = 0;
    while (i < len) { elf_emit_byte(src[i]); i = i + 1; }
}

static void elf_emit_pad(int target) {
    while (elf_out_len < target) elf_emit_byte(0);
}

static int elf_page_align(int v) {
    return (v + ELF_PAGE_SIZE - 1) & ~(ELF_PAGE_SIZE - 1);
}

/* ELF64 header (64 bytes) */
static void elf_emit_ehdr(int entry_lo, int entry_hi, int phoff, int phnum) {
    elf_emit_byte(0x7F); elf_emit_byte('E'); elf_emit_byte('L'); elf_emit_byte('F');
    elf_emit_byte(2);    /* ELFCLASS64 */
    elf_emit_byte(1);    /* ELFDATA2LSB */
    elf_emit_byte(1);    /* EV_CURRENT */
    elf_emit_byte(0);    /* ELFOSABI_NONE */
    elf_emit_byte(0); elf_emit_byte(0); elf_emit_byte(0); elf_emit_byte(0);
    elf_emit_byte(0); elf_emit_byte(0); elf_emit_byte(0); elf_emit_byte(0);

    elf_emit_half(ET_EXEC);
    elf_emit_half(EM_AARCH64);
    elf_emit_word(1);                       /* e_version */
    elf_emit_xword(entry_lo, entry_hi);
    elf_emit_xword(phoff, 0);
    elf_emit_xword(0, 0);                   /* e_shoff = 0 */
    elf_emit_word(0);                       /* e_flags */
    elf_emit_half(64);                      /* e_ehsize */
    elf_emit_half(56);                      /* e_phentsize */
    elf_emit_half(phnum);
    elf_emit_half(64);                      /* e_shentsize (irrelevant) */
    elf_emit_half(0);
    elf_emit_half(0);
}

/* Program header (56 bytes) */
static void elf_emit_phdr(int type, int flags, int offset, int vaddr,
                          int filesz, int memsz) {
    elf_emit_word(type);
    elf_emit_word(flags);
    elf_emit_xword(offset, 0);
    elf_emit_xword(vaddr, 0);
    elf_emit_xword(vaddr, 0);
    elf_emit_xword(filesz, 0);
    elf_emit_xword(memsz, 0);
    elf_emit_xword(ELF_PAGE_SIZE, 0);
}

static int elf_build(void) {
    int ehdr_size; int phdr_size; int num_phdr; int headers_size;
    int text_file_off; int text_vaddr_lo;
    int rodata_file_off; int rodata_vaddr_lo; int rodata_page;
    int data_file_off; int data_vaddr_lo; int data_page; int data_memsz;
    int entry_lo;

    ehdr_size = 64;
    phdr_size = 56;

    num_phdr = 1;
    if (elf_rodata_len > 0) num_phdr = num_phdr + 1;
    if (elf_data_len > 0 || elf_bss_len > 0) num_phdr = num_phdr + 1;

    headers_size = ehdr_size + phdr_size * num_phdr;

    text_file_off = ELF_PAGE_SIZE;
    text_vaddr_lo = (ELF_BASE_ADDR + ELF_PAGE_SIZE) & 0xFFFFFFFF;

    rodata_page = elf_page_align(elf_text_len);
    rodata_file_off = text_file_off + rodata_page;
    rodata_vaddr_lo = text_vaddr_lo + rodata_page;

    if (elf_rodata_len > 0) {
        data_page = elf_page_align(elf_rodata_len);
        data_file_off = rodata_file_off + data_page;
        data_vaddr_lo = rodata_vaddr_lo + data_page;
    } else {
        data_file_off = rodata_file_off;
        data_vaddr_lo = rodata_vaddr_lo;
    }
    data_memsz = elf_data_len + elf_bss_len;

    entry_lo = text_vaddr_lo + elf_entry_off;

    /* Suppress unused-variable warning on hosts that complain. */
    (void)headers_size;

    elf_out_len = 0;

    elf_emit_ehdr(entry_lo, 0, ehdr_size, num_phdr);

    elf_emit_phdr(PT_LOAD, PF_R | PF_X,
                  text_file_off, text_vaddr_lo,
                  elf_text_len, elf_text_len);

    if (elf_rodata_len > 0)
        elf_emit_phdr(PT_LOAD, PF_R,
                      rodata_file_off, rodata_vaddr_lo,
                      elf_rodata_len, elf_rodata_len);

    if (elf_data_len > 0 || elf_bss_len > 0)
        elf_emit_phdr(PT_LOAD, PF_R | PF_W,
                      data_file_off, data_vaddr_lo,
                      elf_data_len, data_memsz);

    elf_emit_pad(text_file_off);
    if (elf_text_len > 0) elf_emit_bytes(elf_text_buf, elf_text_len);

    if (elf_rodata_len > 0) {
        elf_emit_pad(rodata_file_off);
        elf_emit_bytes(elf_rodata_buf, elf_rodata_len);
    }

    if (elf_data_len > 0) {
        elf_emit_pad(data_file_off);
        elf_emit_bytes(elf_data_buf, elf_data_len);
    }
    /* .bss has memsz > filesz; nothing to write. */

    return 0;
}

static int elf_write_file(char *filename) {
    int fd; int total; int n; int written;
    elf_build();
    fd = open(filename, ELFW_OPEN_FLAGS, 0755);   /* mode rwxr-xr-x */
    if (fd < 0) return -1;
    total = 0;
    while (total < elf_out_len) {
        n = elf_out_len - total;
        written = write(fd, elf_out + total, n);
        if (written <= 0) { close(fd); return -1; }
        total = total + written;
    }
    close(fd);
    return 0;
}

/* Address-computation helpers — let the codegen compute global vaddrs. */
static int elf_text_vaddr(void) { return ELF_BASE_ADDR + ELF_PAGE_SIZE; }

static int elf_rodata_vaddr(void) {
    return elf_text_vaddr() + elf_page_align(elf_text_len);
}

static int elf_data_vaddr(void) {
    int v;
    v = elf_rodata_vaddr();
    if (elf_rodata_len > 0) v = v + elf_page_align(elf_rodata_len);
    return v;
}

static int elf_bss_vaddr(void) {
    return elf_data_vaddr() + elf_data_len;
}

#endif /* ELF_WRITER_H */
