# MMIO Implementation Status

## Current Status (Fully Implemented)

MMIO is now functional in both `slow32` and `slow32-fast` emulators with linker-controlled configuration.

### Key Features
1. **Linker Control**: Use `--mmio <size>` flag to enable MMIO (e.g., `--mmio 64k`)
2. **Header Flag**: S32X_FLAG_MMIO (0x80) indicates MMIO is enabled
3. **Immediate Init**: MMIO initializes at program load, not lazily
4. **Memory Protection**: Programs without MMIO flag cannot access MMIO region

### Current Implementation
- **Detection**: Controlled by linker `--mmio` flag which sets S32X_FLAG_MMIO in header
- **Initialization**: Immediate initialization when S32X_FLAG_MMIO is detected
- **Memory Layout**: MMIO at `0x10000000`, 64KB region
- **Ring Buffers**: Request/response queues for host-guest communication
- **Guest Symbols**: Linker creates `__mmio_base` and `__mmio_end` symbols

### Test Programs
- `tests/test_mmio_simple.s` - Basic test that writes to MMIO and uses YIELD
- Link with MMIO: `s32-ld --mmio 64k -o test.s32x test.o23s`
- Link without: `s32-ld -o test.s32x test.o23s`

## Implementation Details (2025-09-08)

### What Was Implemented
1. ✅ **S32X Header Format**: Repurposed reserved field as `mmio_base` (offset 0x3C)
2. ✅ **Header Flag**: Added S32X_FLAG_MMIO (0x80) to indicate MMIO enabled
3. ✅ **Linker Updates**: Set flag and mmio_base when `--mmio` specified
4. ✅ **Emulator Updates**: Check S32X_FLAG_MMIO instead of using heuristics
5. ✅ **Immediate Init**: MMIO initialized at load time when flag is set

### Benefits Over Previous Implementation
- Explicit control via linker flags
- No magic constants or auto-detection heuristics
- Clear failure when MMIO not enabled (memory protection fault)
- Consistent with how other memory regions are handled
- Easier to debug and understand

### Technical Details
The S32X header now includes:
```c
typedef struct {
    // ... existing fields ...
    uint32_t flags;      // Includes S32X_FLAG_MMIO (0x80)
    // ... memory layout fields ...
    uint32_t mmio_base;  // 0x3C: MMIO base address (if flag set)
} s32x_header_t;
```

When `--mmio <size>` is passed to the linker:
1. Sets S32X_FLAG_MMIO in header.flags
2. Calculates and sets header.mmio_base
3. Creates __mmio_base and __mmio_end symbols for guest code
4. Emulator sees flag and initializes MMIO immediately