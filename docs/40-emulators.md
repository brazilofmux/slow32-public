# SLOW-32 Emulators

## Overview

SLOW-32 provides two emulators with different trade-offs:
- **slow32** - Reference emulator with full debugging support
- **slow32-fast** - Optimized emulator for production runs

## Reference Emulator (slow32)

### Features
- Cycle-accurate simulation
- Comprehensive debugging facilities
- Memory protection (W^X enforcement)
- Instruction tracing
- Register tracking
- Breakpoints and watchpoints
- Single-stepping

### Basic Usage

```bash
# Run program normally
$SLOW32_HOME/bin/slow32 program.s32x

# Return value is available in shell
$SLOW32_HOME/bin/slow32 program.s32x
echo $?  # Prints return value (r1 at halt)
```

### Debug Options

#### Trace Mode (-t)
Shows every instruction executed:
```bash
$SLOW32_HOME/bin/slow32 -t program.s32x

# Output format:
# PC: 0x00000000  ADDI r29, r0, 0xFFFF0  r29 = 0x0FFFFF0
# PC: 0x00000004  LUI r3, 0x100          r3 = 0x00100000
# PC: 0x00000008  ADDI r3, r3, 0x234     r3 = 0x00100234
```

#### Register Mode (-r)
Shows only instructions that modify registers:
```bash
$SLOW32_HOME/bin/slow32 -r program.s32x
```

#### Step Mode (-s)
Execute one instruction at a time:
```bash
$SLOW32_HOME/bin/slow32 -s program.s32x

# Commands:
# <Enter>  - Step one instruction
# 'r'      - Show registers
# 'm ADDR' - Show memory at address
# 'c'      - Continue execution
# 'q'      - Quit
```

#### Breakpoints (-b)
Break at specific address:
```bash
$SLOW32_HOME/bin/slow32 -b 0x1000 program.s32x

# Multiple breakpoints
$SLOW32_HOME/bin/slow32 -b 0x1000 -b 0x2000 program.s32x
```

#### Watchpoints (-w)
Watch memory range for access:
```bash
# Watch single address
$SLOW32_HOME/bin/slow32 -w 0x100000 program.s32x

# Watch range
$SLOW32_HOME/bin/slow32 -w 0x100000-0x101000 program.s32x
```

#### Cycle Limit (-c)
Limit execution cycles (catch infinite loops):
```bash
$SLOW32_HOME/bin/slow32 -c 10000 program.s32x
```

### Debug Output Format

#### Register Dump
```
Registers:
r0=00000000 r1=0000002A r2=00100234 r3=00000001
r4=00000000 r5=00000000 r6=00000000 r7=00000000
...
r28=00000000 r29=0FFFFF0 r30=00000000 r31=00000100
PC=00000050
```

#### Memory Dump
```
Memory at 0x00100000:
00100000: 48 65 6C 6C 6F 2C 20 57  |Hello, W|
00100008: 6F 72 6C 64 21 0A 00 00  |orld!...|
```

## Fast Emulator (slow32-fast)

### Features
- Optimized for speed (~350M instructions/sec)
- Minimal overhead
- No debugging features
- Production performance testing

### Usage

```bash
# Run at maximum speed
$SLOW32_HOME/bin/slow32-fast program.s32x

# Time execution
time $SLOW32_HOME/bin/slow32-fast program.s32x
```

### Performance Tips
1. Use for benchmarking
2. Disable unnecessary I/O
3. Compile with -O2 or -O3
4. Avoid DEBUG instructions in hot loops

## Execution Model

### Memory Layout
```
Code Segment:  0x00000000 - 0x000FFFFF (1MB, execute-only)
Data Segment:  0x00100000 - 0x0FFFFFFF (255MB, read/write)
Stack:         0x0FFFFFF0 (grows down)
```

### Register State
- All registers initialized to 0 except:
  - r29 (sp) = 0x0FFFFFF0
  - r31 (lr) = 0xFFFFFFFF (invalid for detection)

### Instruction Timing
| Instruction Type | Cycles |
|-----------------|--------|
| ALU (ADD, SUB, etc.) | 1 |
| Memory (LDW, STW) | 3 |
| Multiply (MUL) | 32 |
| Divide (DIV, REM) | 64 |
| Branch (taken) | 3 |
| Branch (not taken) | 1 |
| Jump (JAL, JALR) | 2 |

### Halt Conditions
1. **HALT instruction** - Normal termination
2. **Invalid PC** - PC outside code segment
3. **Protection fault** - Write to code, execute data
4. **Cycle limit** - Exceeded -c limit
5. **Invalid instruction** - Undefined opcode
6. **DEBUG termination** - DEBUG with special value

## Debug Instruction

The DEBUG instruction provides simple I/O:

```asm
# Output character
li r3, 'A'
debug r3        # Prints 'A'

# Output string
print_loop:
    ldbu r2, r3+0
    beq r2, r0, done
    debug r2
    addi r3, r3, 1
    jal r0, print_loop
done:
```

Special DEBUG values:
- `0x00-0x7F` - Output ASCII character
- `0x80-0xFF` - Reserved for future use

## Error Handling

### Common Errors

#### Segmentation Fault
```
Error: Memory access violation at PC=0x00001234
Attempted to write to address 0x00000100 (code segment)
```
**Cause**: Writing to code segment  
**Fix**: Check pointer calculations

#### Invalid Instruction
```
Error: Invalid instruction 0xFFFFFFFF at PC=0x00001000
```
**Cause**: Executing data or corrupted code  
**Fix**: Check jump/call targets

#### Stack Overflow
```
Error: Stack overflow - SP=0x000FFFF0
```
**Cause**: Recursion too deep or large locals  
**Fix**: Increase stack size or optimize recursion

#### Infinite Loop
```
Error: Cycle limit exceeded (10000 cycles)
Last PC: 0x00001234
```
**Cause**: Program stuck in loop  
**Fix**: Check loop conditions

## Advanced Debugging

### Trace Analysis
```bash
# Save trace to file
$SLOW32_HOME/bin/slow32 -t program.s32x > trace.txt

# Count instruction frequency
$SLOW32_HOME/bin/slow32 -t program.s32x | \
    awk '{print $3}' | sort | uniq -c | sort -rn
```

### Memory Profiling
```bash
# Watch all data segment
$SLOW32_HOME/bin/slow32 -w 0x100000-0x200000 program.s32x
```

### Performance Analysis
```bash
# Compare implementations
time $SLOW32_HOME/bin/slow32-fast version1.s32x
time $SLOW32_HOME/bin/slow32-fast version2.s32x
```

## Implementation Details

### Interpreter Loop
The reference emulator uses a simple fetch-decode-execute loop:
```
while (running) {
    instruction = fetch(PC);
    decode(instruction);
    execute();
    PC += 4;
    cycles++;
}
```

### Fast Emulator Optimizations
- Computed goto dispatch
- Cached decode tables
- Minimal bounds checking
- No trace overhead

### Memory Protection
- Code segment: Execute-only (W^X)
- Data segment: Read/write, no execute
- Checked on every access in debug mode
- Relaxed checking in fast mode

## Testing Programs

### Test Return Value
```asm
.section .text
.globl _start
_start:
    li r1, 42
    halt
```

### Test Memory Access
```asm
.section .data
value:  .word 100

.section .text
.globl _start
_start:
    lui r3, %hi(value)
    addi r3, r3, %lo(value)
    ldw r1, r3+0
    halt
```

### Test Loop
```asm
.section .text
.globl _start
_start:
    li r3, 0
    li r4, 10
loop:
    addi r3, r3, 1
    bne r3, r4, loop
    mv r1, r3
    halt
```

## Limitations

1. **No floating-point** - FP instructions not supported
2. **No interrupts** - Polling only
3. **No MMU** - Fixed memory layout
4. **No cache simulation** - Perfect memory
5. **No pipeline** - Single instruction at a time

## See Also

- [Getting Started](01-getting-started.md) - Basic usage
- [Debugging Guide](93-troubleshooting.md) - Troubleshooting
- [Performance](60-performance.md) - Optimization tips
- [I/O and Host](50-io-and-host.md) - I/O interfaces