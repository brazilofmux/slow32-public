# Linker Memory Layout Configuration Options

## Current State
The linker currently has hardcoded memory layout with limited options:
- Fixed regions: code (1MB), rodata (1MB), data (1MB)
- Stack at 0x0FFFFFF0
- Options: --mmio SIZE, --wxorx/--no-wxorx, -e SYMBOL

## Proposed Enhancements

### 1. Configurable Memory Regions
```bash
# Size controls
--code-size SIZE      # Max code segment size (default: 1MB)
--rodata-size SIZE    # Max rodata size (default: 1MB)  
--data-size SIZE      # Max data+bss size (default: 1MB)
--stack-size SIZE     # Stack size (default: 64KB)
--heap-size SIZE      # Heap space reservation (default: auto)

# Base address controls
--code-base ADDR      # Code segment start (default: 0x0)
--data-base ADDR      # Data segment start (default: after rodata)
--stack-base ADDR     # Stack top address (default: 0x0FFFFFF0)
```

### 2. Automatic Layout Optimizations
```bash
--pack-sections       # Minimize gaps between sections
--page-align          # Align sections to 4KB pages (for mmap)
--merge-rodata        # Merge small rodata sections
--gc-sections         # Remove unreferenced sections
```

### 3. Memory Layout Scripts (Future)
```bash
--script layout.ld    # Use custom linker script
```

Example script format:
```
MEMORY {
    CODE   : ORIGIN = 0x00000000, LENGTH = 2M
    RODATA : ORIGIN = 0x00200000, LENGTH = 1M  
    DATA   : ORIGIN = 0x00300000, LENGTH = 4M
    STACK  : ORIGIN = 0x0FF00000, LENGTH = 1M
}

SECTIONS {
    .text : { *(.text) } > CODE
    .rodata : { *(.rodata) } > RODATA
    .data : { *(.data) } > DATA
    .bss : { *(.bss) } > DATA
}
```

### 4. Memory Protection Options
```bash
--guard-pages         # Add guard pages between regions
--stack-guard SIZE    # Guard page below stack
--nx-heap            # Make heap non-executable
```

### 5. Debug/Analysis Options
```bash
--print-map          # Output memory map file
--warn-overflow      # Warn if sections overflow limits
--stats              # Print size statistics
```

## Implementation Priority

### Phase 1 (Immediate)
- [ ] --stack-size option
- [ ] --code-size, --rodata-size, --data-size options
- [ ] --print-map to generate .map file
- [ ] --pack-sections for tighter packing

### Phase 2 (Next)
- [ ] --gc-sections (dead code elimination)
- [ ] --page-align for mmap efficiency
- [ ] Base address configuration options
- [ ] Guard page support

### Phase 3 (Future)
- [ ] Linker script support
- [ ] Overlay support
- [ ] Dynamic section placement

## Benefits

1. **Flexibility**: Different programs have different needs
   - Small utilities: minimize memory
   - Large programs: need more code/data space
   - Embedded: specific address requirements

2. **Optimization**: Automatic packing can reduce memory footprint
   - Page alignment enables efficient mmap
   - Dead code elimination saves space

3. **Security**: Guard pages detect overflows
   - Configurable W^X regions
   - Stack protection

4. **Debugging**: Memory maps help understand layout
   - Overflow warnings catch issues early
   - Statistics show memory usage