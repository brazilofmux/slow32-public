# SLOW-32 Object and Executable Formats

## Overview

SLOW-32 uses two binary formats:
- **.s32o** - Relocatable object files (output from assembler)
- **.s32x** - Executable files (output from linker)

Both formats are designed for simplicity and ease of parsing.

## Object File Format (.s32o)

### File Structure

```
+------------------+
| Header (32 bytes)|
+------------------+
| Section Headers  |
| (n × 32 bytes)   |
+------------------+
| Symbol Table     |
| (m × 24 bytes)   |
+------------------+
| Relocation Table |
| (r × 16 bytes)   |
+------------------+
| String Table     |
| (variable size)  |
+------------------+
| Section Data     |
| (variable size)  |
+------------------+
```

### Header Format

```c
struct S32O_Header {
    uint32_t magic;           // 0x5333324F ('S32O')
    uint32_t version;         // Format version (currently 1)
    uint32_t num_sections;    // Number of sections
    uint32_t num_symbols;     // Number of symbols
    uint32_t num_relocations; // Number of relocations
    uint32_t string_table_size; // Size of string table
    uint32_t reserved[2];     // Reserved for future use
};
```

### Section Header

```c
struct Section_Header {
    uint32_t name_offset;     // Offset in string table
    uint32_t type;            // Section type
    uint32_t flags;           // Section flags
    uint32_t address;         // Load address (0 for .o files)
    uint32_t offset;          // File offset to data
    uint32_t size;            // Section size in bytes
    uint32_t alignment;       // Required alignment
    uint32_t reserved;        // Reserved
};
```

#### Section Types
- `0x01` - Code (.text)
- `0x02` - Data (.data)
- `0x03` - Read-only data (.rodata)
- `0x04` - BSS (.bss - uninitialized data)

#### Section Flags
- `0x01` - Executable
- `0x02` - Writable
- `0x04` - Allocated in memory

### Symbol Table Entry

```c
struct Symbol_Entry {
    uint32_t name_offset;     // Offset in string table
    uint32_t value;           // Symbol value (address/offset)
    uint32_t size;            // Symbol size
    uint32_t section;         // Section index (0 = undefined)
    uint8_t  type;            // Symbol type
    uint8_t  binding;         // Symbol binding
    uint16_t reserved;        // Reserved
};
```

#### Symbol Types
- `0x00` - No type
- `0x01` - Object (variable)
- `0x02` - Function
- `0x03` - Section
- `0x04` - File

#### Symbol Binding
- `0x00` - Local
- `0x01` - Global
- `0x02` - Weak

### Relocation Entry

```c
struct Relocation_Entry {
    uint32_t offset;          // Offset in section
    uint32_t type;            // Relocation type
    uint32_t symbol;          // Symbol index
    uint32_t addend;          // Constant addend
};
```

See [Relocations Reference](26-relocations.md) for relocation types.

## Executable Format (.s32x)

### File Structure

```
+------------------+
| Header (32 bytes)|
+------------------+
| Segment Headers  |
| (n × 24 bytes)   |
+------------------+
| Segment Data     |
| (variable size)  |
+------------------+
```

### Header Format

```c
struct S32X_Header {
    uint32_t magic;           // 0x53333258 ('S32X')
    uint32_t version;         // Format version (currently 1)
    uint32_t entry_point;     // Entry point address
    uint32_t num_segments;    // Number of segments
    uint32_t flags;           // Executable flags
    uint32_t reserved[3];     // Reserved
};
```

### Segment Header

```c
struct Segment_Header {
    uint32_t type;            // Segment type
    uint32_t address;         // Load address
    uint32_t file_offset;     // Offset in file
    uint32_t file_size;       // Size in file
    uint32_t memory_size;     // Size in memory
    uint32_t flags;           // Segment flags
};
```

#### Segment Types
- `0x01` - Code segment
- `0x02` - Data segment
- `0x03` - Stack segment (optional marker)

#### Segment Flags
- `0x01` - Executable
- `0x02` - Writable
- `0x04` - Readable

## Memory Layout

Standard executable memory layout:

```
0x00000000 - 0x000FFFFF    Code segment (1MB, execute-only)
0x00100000 - 0x0FFFFFFF    Data segment (255MB, read/write)
0x0FFFFFF0 - 0x0FFFFFFF    Initial stack (grows down)
```

## String Table Format

The string table is a sequence of null-terminated strings:

```
+--------+--------+--------+--------+
| \0 | s | t | r | i | n | g | 1 | \0 |
+--------+--------+--------+--------+
| s | t | r | i | n | g | 2 | \0 |
+--------+--------+--------+--------+
```

First byte is always null. String offsets reference byte position.

## Alignment Requirements

- Headers: 4-byte aligned
- Sections: Aligned per section header
- Segments: Page aligned (4096 bytes) recommended

## Tool Usage

### Creating Object Files

```bash
# Assemble to object file
$SLOW32_HOME/bin/slow32asm -o file.s32o file.s

# Examine object file
$SLOW32_HOME/bin/s32-objdump file.s32o
```

### Creating Executables

```bash
# Link object files
$SLOW32_HOME/bin/s32-ld -o program.s32x \
    $SLOW32_HOME/lib/crt0.s32o \
    main.s32o \
    other.s32o

# Examine executable
$SLOW32_HOME/bin/s32-exedump program.s32x
```

## Example Hexdump

### Object File Header
```
00000000  4F 32 33 53 01 00 00 00  03 00 00 00 0A 00 00 00  |O23S............|
00000010  05 00 00 00 80 00 00 00  00 00 00 00 00 00 00 00  |................|
```
- Magic: 0x5333324F ('S32O')
- Version: 1
- 3 sections, 10 symbols, 5 relocations
- String table: 128 bytes

### Executable Header
```
00000000  58 32 33 53 01 00 00 00  00 00 00 00 02 00 00 00  |X23S............|
00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
```
- Magic: 0x53333258 ('S32X')
- Version: 1
- Entry point: 0x00000000
- 2 segments

## Binary Compatibility

### Version Compatibility
- Tools check version field
- Version 1 is current standard
- Future versions will maintain backward compatibility

### Endianness
- All multi-byte values are little-endian
- Matches SLOW-32 architecture

## Common Patterns

### Simple Executable
```
1. Code segment at 0x00000000
2. Data segment at 0x00100000
3. BSS included in data segment (zero-initialized)
4. Stack implicit (not in file)
```

### Library Object
```
1. Multiple .text sections (one per function)
2. Global symbols exported
3. Relocations for external references
4. No assigned addresses
```

## Error Detection

### Object Files
- Check magic number
- Verify section offsets within file
- Validate symbol references
- Check relocation targets

### Executables
- Check magic number
- Verify segment addresses don't overlap
- Validate entry point in code segment
- Check total memory requirements

## Limitations

1. **32-bit addresses only** - No 64-bit support
2. **No compression** - Files are uncompressed
3. **No digital signatures** - No authentication
4. **No metadata** - No version info, dependencies
5. **No dynamic linking** - Static only

## See Also

- [Toolchain Overview](15-toolchain-overview.md) - Tool descriptions
- [Relocations](26-relocations.md) - Relocation details
- [Programmer's Guide](20-programmers-guide.md) - Memory model