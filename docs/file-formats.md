# SLOW-32 File Format Specification

## Overview

This document defines the binary file formats used by the SLOW-32 architecture:
- **`.s32x`** - SLOW-32 eXecutable format (fully linked executable)
- **`.s32o`** - SLOW-32 Object format (relocatable object file)

## Design Principles

1. **Simplicity**: Formats should be easy to parse and generate
2. **Extensibility**: Room for future features without breaking compatibility
3. **Alignment**: All sections aligned to 4-byte boundaries
4. **Self-describing**: Files contain enough metadata to be introspectable

## SLOW-32 Executable Format (.s32x)

The `.s32x` format is the final executable format loaded by the emulator.

### File Structure

```
+-------------------+
| Header            | 32 bytes
+-------------------+
| Section Table     | Variable (nsections * 16 bytes)
+-------------------+
| String Table      | Variable (aligned to 4 bytes)
+-------------------+
| Section Data      | Variable (each section aligned)
+-------------------+
```

### Header Structure (64 bytes)

```c
typedef struct {
    uint32_t magic;        // 0x00: Magic number 0x53333258 ("S32X")
    uint16_t version;      // 0x04: Format version (1)
    uint8_t  endian;       // 0x06: Endianness (0x01=little, 0x02=big)
    uint8_t  machine;      // 0x07: Machine type (0x32 for SLOW-32)
    uint32_t entry;        // 0x08: Entry point address (flexible, not fixed at 0)
    uint32_t nsections;    // 0x0C: Number of sections
    uint32_t sec_offset;   // 0x10: Offset to section table
    uint32_t str_offset;   // 0x14: Offset to string table
    uint32_t str_size;     // 0x18: Size of string table
    uint32_t flags;        // 0x1C: Executable flags (see below)
    // Memory layout configuration (new in v1)
    uint32_t code_limit;   // 0x20: End of code region (e.g., 0x00100000 for 1MB)
    uint32_t rodata_limit; // 0x24: End of read-only region (e.g., 0x00200000)
    uint32_t data_limit;   // 0x28: End of data region (e.g., 0x10000000)
    uint32_t stack_base;   // 0x2C: Initial stack pointer (e.g., 0x0FFFFFF0)
    uint32_t mem_size;     // 0x30: Total memory to allocate (0 = use data_limit)
    uint32_t heap_base;    // 0x34: Start of heap (0 = no heap, use sbrk)
    uint32_t checksum;     // 0x38: CRC32 of all sections (0 = no checksum)
    uint32_t mmio_base;    // 0x3C: MMIO region base (if S32X_FLAG_MMIO set)
} s32x_header_t;
```

**Note on Entry Point**: The entry point is completely flexible and determined by the linker. 
Typically it will be the address of `_start` or `main`, but can be any valid code address.
If an Exception Vector Table exists with a RESET handler, that takes precedence.

**Note on Memory Layout**: The memory region limits allow the linker to configure the memory
map within constraints. The emulator enforces these boundaries for protection.

**Note on Memory Allocation**: 
- `mem_size` specifies total physical memory to allocate (0 means use data_limit as size)
- For a 64KB program, set mem_size=0x10000 to allocate only 64KB
- The emulator allocates `mem_size` bytes (or data_limit if mem_size=0)
- Virtual addresses above mem_size are invalid (trap on access)
- This enables efficient sandboxing with minimal memory footprint

**Note on Heap**:
- `heap_base` marks the start of the heap (grows up toward stack)
- If heap_base=0, no heap is pre-allocated (program must use syscalls)
- Stack grows down from stack_base, heap grows up from heap_base
- Collision between heap and stack triggers exception

**Note on Endianness**:
- All SLOW-32 files are **little-endian** (endian field = 0x01)
- Big-endian (0x02) is defined but not currently supported
- This matches x86/ARM defaults and simplifies implementation
- Future emulators MAY support big-endian but MUST support little-endian

**Note on Checksum**:
- CRC32 checksum covers all section data (not headers)
- Checksum=0 means no integrity checking
- Computed after compression (if any)
- Helps detect corruption in storage/transmission

**Note on Memory Limits**:
- Architecture supports full 32-bit addressing (4GB)
- Default configurations use 256MB for compatibility
- mem_size can specify up to 4GB if needed
- MMIO base is configurable (not fixed at 256MB)

### Executable Flags

```c
#define S32X_FLAG_W_XOR_X    0x0001  // W^X protection enabled
#define S32X_FLAG_HAS_EVT    0x0002  // Has Exception Vector Table
#define S32X_FLAG_HAS_TSR    0x0004  // Has Thread Service Routines
#define S32X_FLAG_HAS_DEBUG  0x0008  // Contains debug information
#define S32X_FLAG_STRIPPED   0x0010  // Symbols stripped
#define S32X_FLAG_PIC        0x0020  // Position-independent code
#define S32X_FLAG_COMPRESSED 0x0040  // Section data is compressed
#define S32X_FLAG_MMIO       0x0080  // Has MMIO region enabled
```

### Section Entry (20 bytes)

```c
typedef struct {
    uint32_t name_offset;  // Offset into string table
    uint32_t type;         // Section type (see S32_SEC_*)
    uint32_t vaddr;        // Virtual address to load at
    uint32_t offset;       // File offset to section data
    uint32_t size;         // Size in file (compressed if flag set)
    uint32_t mem_size;     // Size in memory (uncompressed)
    uint32_t flags;        // Section flags
} s32x_section_t;
```

**Note on Compression**:
- If S32X_FLAG_COMPRESSED is set, sections may be compressed
- `size` = compressed size in file, `mem_size` = uncompressed size
- Compression format: zlib (deflate) for simplicity and availability
- Small sections (<1KB) should not be compressed (overhead exceeds benefit)

### Section Types

```c
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
```

### Section Flags

```c
#define S32_SEC_FLAG_EXEC   0x0001  // Executable
#define S32_SEC_FLAG_WRITE  0x0002  // Writable
#define S32_SEC_FLAG_READ   0x0004  // Readable
#define S32_SEC_FLAG_ALLOC  0x0008  // Allocate memory for this section
```

### Exception Vector Table Section

When `S32X_FLAG_HAS_EVT` is set, there's an EVT section containing exception handlers.
The EVT is **not** mapped into memory - it's metadata used by the emulator to handle exceptions.

```c
typedef struct {
    uint32_t num_entries;  // Number of exception vectors
    struct {
        uint32_t vector;   // Exception vector number (see below)
        uint32_t handler;  // Handler address in code
        uint32_t flags;    // Handler flags
    } entries[];
} s32x_evt_section_t;
```

### Exception Vectors

```c
// Core CPU exceptions (0x00-0x0F)
#define S32X_EXC_RESET       0x00  // Reset/Power-on (entry point)
#define S32X_EXC_ILLEGAL     0x01  // Illegal instruction
#define S32X_EXC_ALIGN       0x02  // Alignment fault
#define S32X_EXC_DIV_ZERO    0x03  // Division by zero
#define S32X_EXC_OVERFLOW    0x04  // Integer overflow (if enabled)
#define S32X_EXC_PRIVILEGE   0x05  // Privilege violation
#define S32X_EXC_BREAKPOINT  0x06  // Breakpoint/Debug trap
#define S32X_EXC_SYSCALL     0x07  // System call

// Memory exceptions (0x10-0x1F)
#define S32X_EXC_PAGE_FAULT  0x10  // Page fault (future MMU)
#define S32X_EXC_WRITE_PROT  0x11  // Write to protected memory
#define S32X_EXC_EXEC_PROT   0x12  // Execute from non-exec memory
#define S32X_EXC_STACK_OVER  0x13  // Stack overflow

// External interrupts (0x20-0xFF)
#define S32X_INT_TIMER       0x20  // Timer interrupt
#define S32X_INT_EXTERNAL    0x21  // External interrupt pin
// 0x22-0xFF available for devices
```

### Exception Handling

When an exception occurs:
1. Save PC to a designated register (e.g., r31 or exception-specific)
2. Save processor state if needed
3. Look up handler address from EVT
4. Jump to handler
5. Handler must save/restore any registers it uses
6. Return with special instruction (TBD: ERET?)

### Thread Service Routines (TSR) Section

As mentioned in the architecture docs, TSR is an alternative to interrupts for thread-friendly 
embedded use. TSR handlers are **cooperative** - they're called at safe points, not preemptively.

```c
typedef struct {
    uint32_t num_routines;  // Number of service routines
    struct {
        uint32_t id;         // Service routine ID
        uint32_t handler;    // Handler address
        uint32_t priority;   // Priority (lower = higher priority)
        uint32_t flags;      // TSR flags
    } routines[];
} s32x_tsr_section_t;
```

TSRs are invoked via YIELD instruction with a service ID, allowing cooperative multitasking
without the complexity of preemptive interrupts. Perfect for embedding SLOW-32 as a sandboxed
compute engine within larger applications.

## Memory Layout Configuration

The executable format allows flexible memory layout configuration within these constraints:

### Memory Regions

```c
// Default memory layout (traditional)
#define DEFAULT_CODE_LIMIT    0x00100000  // 1MB code
#define DEFAULT_RODATA_LIMIT  0x00200000  // 1MB read-only data  
#define DEFAULT_DATA_LIMIT    0x10000000  // ~254MB data
#define DEFAULT_STACK_BASE    0x0FFFFFF0  // Stack at top of 256MB

// Minimum sizes
#define MIN_CODE_SIZE         0x00010000  // 64KB minimum code
#define MIN_RODATA_SIZE       0x00010000  // 64KB minimum rodata
#define MIN_DATA_SIZE         0x00100000  // 1MB minimum data
#define MIN_STACK_SPACE       0x00010000  // 64KB minimum stack
```

### Memory Protection Model

Based on the configured limits, the emulator enforces:

```
[0x00000000 - code_limit):    Code Region (R+X)
  - Readable and Executable
  - NOT Writable (prevents self-modifying code)
  
[code_limit - rodata_limit):   Read-Only Data Region (R)
  - Readable only
  - NOT Writable or Executable
  
[rodata_limit - data_limit):   Data Region (R+W)
  - Readable and Writable
  - NOT Executable (prevents code injection)
  
[data_limit - 0x10000000):     Reserved
  - MMIO and future expansion
  
[stack_base - grows down]:     Stack (R+W)
  - Readable and Writable
  - NOT Executable
```

### Linker Configuration Examples

**Tiny Embedded** (minimal memory):
```c
code_limit   = 0x00004000  // 16KB code
rodata_limit = 0x00004000  // No rodata
data_limit   = 0x00010000  // 48KB data
stack_base   = 0x0000FFF0  // Stack at 64KB boundary
mem_size     = 0x00010000  // Allocate only 64KB total!
heap_base    = 0x00008000  // 32KB heap space
```

**Standard Application** (balanced):
```c
code_limit   = 0x00100000  // 1MB code  
rodata_limit = 0x00200000  // 1MB read-only data
data_limit   = 0x01000000  // 14MB data
stack_base   = 0x00FFFF00  // Stack at 16MB
mem_size     = 0x01000000  // Allocate only 16MB (not 256MB)
heap_base    = 0x00400000  // Heap starts at 4MB
```

**Large Application** (memory-hungry):
```c
code_limit   = 0x00400000  // 4MB code
rodata_limit = 0x00600000  // 2MB read-only data
data_limit   = 0x10000000  // ~250MB data
stack_base   = 0x0FFFFFF0  // Stack at top
mem_size     = 0           // Use full data_limit size
heap_base    = 0x01000000  // Large heap starting at 16MB
```

**Sandboxed Instance** (many concurrent):
```c
code_limit   = 0x00010000  // 64KB code
rodata_limit = 0x00020000  // 64KB rodata
data_limit   = 0x00100000  // 896KB data
stack_base   = 0x000FFFF0  // Stack at 1MB
mem_size     = 0x00100000  // Exactly 1MB per instance
heap_base    = 0x00040000  // Small heap
```

### Validation Rules

The loader/emulator MUST validate:

**Memory Layout:**
1. `code_limit >= MIN_CODE_SIZE`
2. `rodata_limit >= code_limit` (can be equal if no rodata)
3. `data_limit > rodata_limit + MIN_DATA_SIZE`
4. `stack_base >= data_limit + MIN_STACK_SPACE`
5. All limits must be 4-byte aligned
6. `data_limit <= 0x10000000` (MMIO boundary)

**Section Placement:**
1. All code sections must load entirely within `[0, code_limit)`
2. All rodata sections must load within `[code_limit, rodata_limit)`
3. All data/bss sections must load within `[rodata_limit, data_limit)`
4. No sections may overlap

**Entry Points and Vectors:**
1. Entry point must be within code region `[0, code_limit)`
2. All exception handlers in EVT must point within code region
3. All TSR handlers must point within code region
4. Symbol addresses must be within appropriate regions (code symbols in code, etc.)

**Security Checks:**
1. No writable section may load into code region
2. No executable section may load into data region
3. Section permissions must match region permissions
4. Reject if any relocation would violate region boundaries

## SLOW-32 Object Format (.s32o)

The `.s32o` format is used for relocatable object files that can be linked together.

### File Structure

```
+-------------------+
| Header            | 32 bytes
+-------------------+
| Section Table     | Variable
+-------------------+
| Symbol Table      | Variable
+-------------------+
| Relocation Table  | Variable
+-------------------+
| String Table      | Variable
+-------------------+
| Section Data      | Variable
+-------------------+
```

### Header Structure (40 bytes)

```c
typedef struct {
    uint32_t magic;        // 0x00: Magic number 0x5333324F ("S32O")
    uint16_t version;      // 0x04: Format version (1)
    uint8_t  endian;       // 0x06: Endianness (0x01=little, must match .s32x)
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
```

### Object File Flags

```c
#define S32O_FLAG_PIC        0x0001  // Position-independent code
#define S32O_FLAG_DEBUG      0x0002  // Contains debug information
#define S32O_FLAG_STRIPPED   0x0004  // Local symbols stripped
```

### Section Entry (32 bytes)

```c
typedef struct {
    uint32_t name_offset;  // Offset into string table
    uint32_t type;         // Section type (same as S32_SEC_*)
    uint32_t flags;        // Section flags (R/W/X permissions)
    uint32_t size;         // Section size
    uint32_t offset;       // File offset to section data
    uint32_t align;        // Required alignment (power of 2)
    uint32_t nrelocs;      // Number of relocations
    uint32_t reloc_offset; // Offset to relocation entries
} s32o_section_t;
```

**Note**: Section types and flags match the executable format for consistency.

### Symbol Entry (16 bytes)

```c
typedef struct {
    uint32_t name_offset;  // Offset into string table
    uint32_t value;        // Symbol value (offset in section)
    uint16_t section;      // Section index (0 = undefined)
    uint8_t  type;         // Symbol type
    uint8_t  binding;      // Symbol binding
    uint32_t size;         // Size of object
} s32o_symbol_t;
```

### Symbol Types

```c
#define S32O_SYM_NOTYPE   0x00  // No type
#define S32O_SYM_FUNC     0x01  // Function
#define S32O_SYM_OBJECT   0x02  // Data object
#define S32O_SYM_SECTION  0x03  // Section symbol
```

### Symbol Binding

```c
#define S32O_BIND_LOCAL   0x00  // Local symbol
#define S32O_BIND_GLOBAL  0x01  // Global symbol
#define S32O_BIND_WEAK    0x02  // Weak symbol
```

### Relocation Entry (16 bytes)

```c
typedef struct {
    uint32_t offset;       // Offset in section where to apply
    uint32_t symbol;       // Symbol index
    uint32_t type;         // Relocation type
    int32_t  addend;       // Constant addend (full 32-bit)
} s32o_reloc_t;
```

### Relocation Types

```c
#define S32O_REL_NONE     0x0000  // No relocation
#define S32O_REL_32       0x0001  // Direct 32-bit reference
#define S32O_REL_HI20     0x0002  // High 20 bits (for LUI)
#define S32O_REL_LO12     0x0003  // Low 12 bits (for ADDI/LD/ST)
#define S32O_REL_BRANCH   0x0004  // 13-bit PC-relative branch
#define S32O_REL_JAL      0x0005  // 21-bit PC-relative jump
#define S32O_REL_CALL     0x0006  // Function call (HI20+LO12 pair)
#define S32O_REL_PCREL_HI20 0x0007 // PC-relative high 20 bits (for AUIPC)
#define S32O_REL_PCREL_LO12 0x0008 // PC-relative low 12 bits (paired with PCREL_HI20)
```

### Common Section Names

Object files typically contain these sections:
- `.text` - Code (type=S32X_SEC_CODE, flags=R+X)
- `.rodata` - Read-only data (type=S32X_SEC_RODATA, flags=R)
- `.data` - Initialized data (type=S32X_SEC_DATA, flags=R+W)
- `.bss` - Uninitialized data (type=S32X_SEC_BSS, flags=R+W, size in file=0)
- `.symtab` - Symbol table (not loaded)
- `.strtab` - String table (not loaded)
- `.reloc.text` - Relocations for .text section

## Linking Process

### Symbol Resolution

1. Collect all symbols from input object files
2. Resolve undefined symbols against defined symbols
3. Report undefined symbols as errors
4. Handle weak symbols (can remain undefined)

### Relocation Processing

For each relocation:

1. **S32O_REL_32**: Write full 32-bit address
2. **S32O_REL_HI20**: Write upper 20 bits to LUI instruction
3. **S32O_REL_LO12**: Write lower 12 bits to I-type instruction
4. **S32O_REL_BRANCH**: Calculate PC-relative offset, check 13-bit range
5. **S32O_REL_JAL**: Calculate PC-relative offset, check 21-bit range
6. **S32O_REL_CALL**: Generate LUI+JALR sequence for long calls

### Address Assignment

The linker assigns addresses based on the memory layout configuration:

1. Code sections start at 0x00000000, packed sequentially
2. Read-only data starts at next page after code (or code_limit if specified)
3. Data sections start at rodata_limit, packed sequentially
4. BSS follows initialized data in the data region
5. Stack starts at stack_base (configured in header, default 0x0FFFFFF0)

The linker can choose memory boundaries based on:
- Total size of each section type
- Alignment requirements
- Security preferences (gaps between regions)
- Target environment (embedded vs application)

## Debug Information

**Current Status**: Debug format is TBD. Options being considered:

1. **Minimal Line Table**: Just map PC → source:line for basic debugging
2. **Symbol Table Only**: Function and variable names with addresses
3. **DWARF Subset**: Use simplified DWARF for compatibility with existing tools
4. **Custom Format**: Design something specific to SLOW-32's needs

For now, debug sections (S32X_SEC_DEBUG) are reserved but undefined. The immediate
priority is getting the core toolchain working. Debug info can be added later without
breaking the format (version bump if needed).

## Migration Path

### Phase 1: Executable Format
1. Update emulator to support .s32x format
2. Keep backward compatibility with old .bin format
3. Update assembler to generate .s32x directly for now

### Phase 2: Object Files
1. Update assembler to generate .s32o files
2. Implement linker to combine .s32o → .s32x
3. Update slow32cc to use assembler + linker

### Phase 3: Advanced Features
1. Add debug information support
2. Implement dynamic linking capabilities
3. Add compression for sections

## Example File Layouts

### Minimal .s32x File

```
Offset  Size  Description
0x0000  32    Header (magic=0x53333258, entry=0x0000, nsections=2)
0x0020  32    Section table (2 entries: .text, .data)
0x0040  16    String table (".text\0.data\0")
0x0050  N     Code section data
0x????  M     Data section data
```

### Minimal .s32o File

```
Offset  Size  Description
0x0000  32    Header (magic=0x5333324F, nsections=1, nsymbols=2)
0x0020  24    Section table (1 entry: .text)
0x0038  32    Symbol table (2 symbols: main, external_func)
0x0058  12    Relocation table (1 relocation)
0x0064  32    String table
0x0084  N     Code section data
```

## SLOW-32 Archive Format (.s32a)

The `.s32a` format is used to bundle multiple `.s32o` object files into a single library, with a symbol index for efficient linking.

### File Structure

```
+-------------------+
| Header            | 32 bytes
+-------------------+
| Symbol Index      | Variable (nsymbols * 8 bytes)
+-------------------+
| Member Table      | Variable (nmembers * 24 bytes)
+-------------------+
| String Table      | Variable
+-------------------+
| Member Data       | Variable (packed .s32o files)
+-------------------+
```

### Header Structure (32 bytes)

```c
typedef struct {
    uint32_t magic;         // 0x00: Magic number 0x53333241 ("S32A")
    uint16_t version;       // 0x04: Format version (1)
    uint8_t  endian;        // 0x06: Endianness (0x01=little)
    uint8_t  reserved;      // 0x07: Reserved (must be 0)
    uint32_t nmembers;      // 0x08: Number of archive members
    uint32_t mem_offset;    // 0x0C: Offset to member table
    uint32_t nsymbols;      // 0x10: Number of symbols in index
    uint32_t sym_offset;    // 0x14: Offset to symbol index
    uint32_t str_offset;    // 0x18: Offset to string table
    uint32_t str_size;      // 0x1C: Size of string table
} s32a_header_t;
```

### Symbol Index Entry (8 bytes)

Maps global symbols to the archive members that define them.

```c
typedef struct {
    uint32_t name_offset;   // Offset into string table
    uint32_t member_index;  // Index of member in member table
} s32a_symbol_t;
```

### Member Entry (24 bytes)

```c
typedef struct {
    uint32_t name_offset;   // Offset into string table
    uint32_t offset;        // File offset to member data (.s32o file)
    uint32_t size;          // Size of member data
    uint32_t timestamp;     // Modification time (Unix timestamp)
    uint32_t uid;           // User ID
    uint32_t gid;           // Group ID
} s32a_member_t;
```

## Compatibility Notes

- Magic numbers chosen to be human-readable and unique
- Version field allows format evolution
- All multi-byte values are little-endian
- Padding bytes should be zero
- Unknown section types should be preserved but not loaded
- Unknown flags should trigger warnings but not errors