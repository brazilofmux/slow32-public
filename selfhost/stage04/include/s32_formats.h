#ifndef S32_FORMATS_H
#define S32_FORMATS_H

#include <stdint.h>

// ============================================================================
// SLOW-32 Object File Format (.s32o)
// ============================================================================

#define S32O_MAGIC 0x5333324F  // "S32O" in little-endian

// Object file header (40 bytes)
typedef struct {
    uint32_t magic;        // 0x00: Magic number 0x5333324F ("S32O")
    uint16_t version;      // 0x04: Format version (1)
    uint8_t  endian;       // 0x06: Endianness (0x01=little)
    uint8_t  machine;      // 0x07: Machine type (0x32 for SLOW-32)
    uint32_t flags;        // 0x08: Object file flags
    uint32_t nsections;    // 0x0C: Number of sections
    uint32_t sec_offset;   // 0x10: Offset to section table
    uint32_t nsymbols;     // 0x14: Number of symbols
    uint32_t sym_offset;   // 0x18: Offset to symbol table
    uint32_t str_offset;   // 0x1C: Offset to string table
    uint32_t str_size;     // 0x20: Size of string table
    uint32_t checksum;     // 0x24: CRC32 of all sections (0 = no checksum)
} s32o_header_t;

// Object file flags
#define S32O_FLAG_PIC        0x0001  // Position-independent code
#define S32O_FLAG_DEBUG      0x0002  // Contains debug information
#define S32O_FLAG_STRIPPED   0x0004  // Local symbols stripped

// Section entry (32 bytes)
typedef struct {
    uint32_t name_offset;  // Offset into string table
    uint32_t type;         // Section type
    uint32_t flags;        // Section flags (R/W/X permissions)
    uint32_t size;         // Section size
    uint32_t offset;       // File offset to section data
    uint32_t align;        // Required alignment (power of 2)
    uint32_t nrelocs;      // Number of relocations
    uint32_t reloc_offset; // Offset to relocation entries
} s32o_section_t;

// Symbol entry (16 bytes)
typedef struct {
    uint32_t name_offset;  // Offset into string table
    uint32_t value;        // Symbol value (offset in section)
    uint16_t section;      // Section index (0 = undefined)
    uint8_t  type;         // Symbol type
    uint8_t  binding;      // Symbol binding
    uint32_t size;         // Size of object
} s32o_symbol_t;

// Symbol types
#define S32O_SYM_NOTYPE   0x00  // No type
#define S32O_SYM_FUNC     0x01  // Function
#define S32O_SYM_OBJECT   0x02  // Data object
#define S32O_SYM_SECTION  0x03  // Section symbol

// Symbol binding
#define S32O_BIND_LOCAL   0x00  // Local symbol
#define S32O_BIND_GLOBAL  0x01  // Global symbol
#define S32O_BIND_WEAK    0x02  // Weak symbol

// Relocation entry (16 bytes)
typedef struct {
    uint32_t offset;       // Offset in section where to apply
    uint32_t symbol;       // Symbol index
    uint32_t type;         // Relocation type
    int32_t  addend;       // Constant addend (full 32-bit)
} s32o_reloc_t;

// Relocation types
#define S32O_REL_NONE     0x0000  // No relocation
#define S32O_REL_32       0x0001  // Direct 32-bit reference
#define S32O_REL_HI20     0x0002  // High 20 bits (for LUI)
#define S32O_REL_LO12     0x0003  // Low 12 bits (for ADDI/LD/ST)
#define S32O_REL_BRANCH   0x0004  // 13-bit PC-relative branch
#define S32O_REL_JAL      0x0005  // 21-bit PC-relative jump
#define S32O_REL_CALL     0x0006  // Function call (HI20+LO12 pair)
#define S32O_REL_PCREL_HI20 0x0007 // PC-relative high 20 bits (for AUIPC)
#define S32O_REL_PCREL_LO12 0x0008 // PC-relative low 12 bits (paired with PCREL_HI20)

// ============================================================================
// SLOW-32 Executable Format (.s32x)
// ============================================================================

#define S32X_MAGIC 0x53333258  // "S32X" in little-endian

// Executable header (64 bytes)
typedef struct {
    uint32_t magic;        // 0x00: Magic number 0x53333258 ("S32X")
    uint16_t version;      // 0x04: Format version (1)
    uint8_t  endian;       // 0x06: Endianness (0x01=little)
    uint8_t  machine;      // 0x07: Machine type (0x32 for SLOW-32)
    uint32_t entry;        // 0x08: Entry point address
    uint32_t nsections;    // 0x0C: Number of sections
    uint32_t sec_offset;   // 0x10: Offset to section table
    uint32_t str_offset;   // 0x14: Offset to string table
    uint32_t str_size;     // 0x18: Size of string table
    uint32_t flags;        // 0x1C: Executable flags
    // Memory layout configuration
    uint32_t code_limit;   // 0x20: End of code region
    uint32_t rodata_limit; // 0x24: End of read-only region
    uint32_t data_limit;   // 0x28: End of data region
    uint32_t stack_base;   // 0x2C: Initial stack pointer (top of stack)
    uint32_t mem_size;     // 0x30: Total memory to allocate
    uint32_t heap_base;    // 0x34: Start of heap
    uint32_t stack_end;    // 0x38: Bottom of stack (stack grows down to here)
    uint32_t mmio_base;    // 0x3C: MMIO region base (if S32X_FLAG_MMIO set)
} s32x_header_t;

// Executable flags
#define S32X_FLAG_W_XOR_X    0x0001  // W^X protection enabled
#define S32X_FLAG_HAS_EVT    0x0002  // Has Exception Vector Table
#define S32X_FLAG_HAS_TSR    0x0004  // Has Thread Service Routines
#define S32X_FLAG_HAS_DEBUG  0x0008  // Contains debug information
#define S32X_FLAG_STRIPPED   0x0010  // Symbols stripped
#define S32X_FLAG_PIC        0x0020  // Position-independent code
#define S32X_FLAG_COMPRESSED 0x0040  // Section data is compressed
#define S32X_FLAG_MMIO       0x0080  // Has MMIO region enabled

// Section entry (20 bytes)
typedef struct {
    uint32_t name_offset;  // Offset into string table
    uint32_t type;         // Section type
    uint32_t vaddr;        // Virtual address to load at
    uint32_t offset;       // File offset to section data
    uint32_t size;         // Size in file (compressed if flag set)
    uint32_t mem_size;     // Size in memory (uncompressed)
    uint32_t flags;        // Section flags
} s32x_section_t;

// Section types (shared between .s32o and .s32x)
#define S32_SEC_NULL     0x0000  // Null section
#define S32_SEC_CODE     0x0001  // Executable code
#define S32_SEC_DATA     0x0002  // Initialized data
#define S32_SEC_BSS      0x0003  // Uninitialized data (zero-filled)
#define S32_SEC_RODATA   0x0004  // Read-only data
#define S32_SEC_EVT      0x0010  // Exception Vector Table
#define S32_SEC_TSR      0x0011  // Thread Service Routines table
#define S32_SEC_DEBUG    0x0020  // Debug information
#define S32_SEC_SYMTAB   0x0021  // Symbol table
#define S32_SEC_STRTAB   0x0022  // String table for symbols

// Section flags (shared)
#define S32_SEC_FLAG_EXEC   0x0001  // Executable
#define S32_SEC_FLAG_WRITE  0x0002  // Writable
#define S32_SEC_FLAG_READ   0x0004  // Readable
#define S32_SEC_FLAG_ALLOC  0x0008  // Allocate memory for this section

// Common constants
#define S32_ENDIAN_LITTLE 0x01
#define S32_ENDIAN_BIG    0x02
#define S32_MACHINE_SLOW32 0x32

#endif // S32_FORMATS_H