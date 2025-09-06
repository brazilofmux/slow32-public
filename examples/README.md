# SLOW-32 Examples

This directory contains example programs demonstrating SLOW-32 features and programming techniques.

## Assembly Examples

### hello-debug.s
Basic "Hello World" using DEBUG instruction for output.
- Shows string output via DEBUG
- Basic loop structure
- Memory addressing

### fibonacci.s
Compute Fibonacci numbers.
```asm
        .section .text
        .globl _start
_start:
        li r3, 10        # Calculate fib(10)
        li r4, 0         # fib(0) = 0
        li r5, 1         # fib(1) = 1
        
.loop:
        beq r3, r0, .done
        add r6, r4, r5   # next = current + previous
        mv r4, r5        # previous = current
        mv r5, r6        # current = next
        subi r3, r3, 1
        jal r0, .loop
        
.done:
        mv r1, r5        # Return value in r1
        halt
```

## C Examples

### hello.c
"Hello World" in C.
- Shows DEBUG instruction integration
- String handling
- C runtime usage

### factorial.c
Recursive factorial calculation.
```c
int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

int main() {
    return factorial(5);  // Returns 120
}
```

### arrays.c
Array manipulation and sorting.
```c
void bubble_sort(int arr[], int n) {
    for (int i = 0; i < n-1; i++) {
        for (int j = 0; j < n-i-1; j++) {
            if (arr[j] > arr[j+1]) {
                int temp = arr[j];
                arr[j] = arr[j+1];
                arr[j+1] = temp;
            }
        }
    }
}

int main() {
    int data[] = {64, 34, 25, 12, 22, 11, 90};
    bubble_sort(data, 7);
    return data[0];  // Returns 11 (smallest)
}
```

## Building Examples

### Assembly Programs
```bash
# Assemble
../tools/assembler/slow32asm -o example.s32o example.s

# Link (simple program without C runtime)
../tools/linker/s32-ld -o example.s32x example.s32o

# Run
../tools/emulator/slow32 example.s32x
```

### C Programs
```bash
# Compile to LLVM IR
~/llvm-project/build/bin/clang \
    --target slow32-unknown-none \
    -S -emit-llvm -O1 example.c -o example.ll

# Generate assembly
~/llvm-project/build/bin/llc \
    -mtriple=slow32-unknown-none \
    example.ll -o example.s

# Assemble and link
~/slow-32/assembler/slow32asm -o example.s32o example.s
~/slow-32/linker/s32-ld -o example.s32x \
    ~/slow-32/runtime/crt0.s32o \
    example.s32o \
    ~/slow-32/runtime/intrinsics.s32o

# Run
~/slow-32/emulator/slow32 example.s32x
```

## Makefile for Examples

```makefile
# Makefile for SLOW-32 examples
SLOW32 = ~/slow-32
LLVM = ~/llvm-project/build/bin

CC = $(LLVM)/clang
LLC = $(LLVM)/llc
AS = $(SLOW32)/assembler/slow32asm
LD = $(SLOW32)/linker/s32-ld
EMU = $(SLOW32)/emulator/slow32
RUNTIME = $(SLOW32)/runtime

CFLAGS = --target=slow32-unknown-none -S -emit-llvm -O1
LLCFLAGS = -mtriple=slow32-unknown-none

# Build all examples
all: hello.s32x fibonacci.s32x factorial.s32x arrays.s32x

# Pattern rules
%.ll: %.c
	$(CC) $(CFLAGS) $< -o $@

%.s: %.ll
	$(LLC) $(LLCFLAGS) $< -o $@

%.s32o: %.s
	$(AS) -o $@ $<

%.s32x: %.s32o
	$(LD) -o $@ $(RUNTIME)/crt0.s32o $< $(RUNTIME)/intrinsics.s32o

# Run examples
run-%: %.s32x
	$(EMU) $<

# Debug examples
debug-%: %.s32x
	$(EMU) -t $< | head -50

clean:
	rm -f *.ll *.s *.s32o *.s32x

.PHONY: all clean
```

## Advanced Examples

### crc32.c
CRC32 checksum calculation with lookup table.
- Demonstrates bit manipulation
- Large lookup tables
- Performance optimization

### varargs.c
Variable argument functions.
- Shows varargs support
- Custom printf implementation
- Stack manipulation

### switch.c
Switch statements with jump tables.
- Compiler-generated jump tables
- Efficient dispatch
- Complex control flow

## Debugging Tips

1. **Use trace mode** to see execution:
   ```bash
   ~/slow-32/emulator/slow32 -t example.s32x | less
   ```

2. **Watch specific memory ranges**:
   ```bash
   ~/slow-32/emulator/slow32 -w 0x100000-0x101000 example.s32x
   ```

3. **Set breakpoints** at function entry:
   ```bash
   ~/slow-32/emulator/slow32 -b 0x100 example.s32x
   ```

4. **Limit cycles** to catch infinite loops:
   ```bash
   ~/slow-32/emulator/slow32 -c 10000 example.s32x
   ```

## Performance Notes

- Tight loops can achieve ~350M instructions/sec
- Memory alignment matters (use word alignment)
- Multiply takes 32 cycles, divide takes 64 cycles
- Branch prediction is not modeled

## Contributing Examples

When adding new examples:
1. Include clear comments
2. Show specific SLOW-32 features
3. Provide build instructions
4. Test with both emulators
5. Document expected output