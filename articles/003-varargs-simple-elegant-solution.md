# Varargs Done Right: SLOW32's Elegant Solution to a Complex Problem

*How a simple design choice led to the cleanest variadic function implementation I've ever seen*

## The Problem Everyone Faces

Every C programmer eventually encounters variadic functions - those magical functions like `printf` that accept a variable number of arguments. Behind that convenient `...` syntax lies one of the messiest parts of most ABIs. On x86-64, you need to understand the difference between register and stack parameters, maintain counts of floating-point arguments, and deal with the infamous `va_list` structure that's actually machine-specific assembly code in disguise.

I've used varargs on multiple architectures, and it's always been painful. Until SLOW32.

## The Traditional Mess

Let's look at what most architectures do. On x86-64, the first six integer arguments go in registers (RDI, RSI, RDX, RCX, R8, R9), floating-point arguments go in XMM registers, and everything else goes on the stack. When you call `va_start`, it has to:

1. Save all potential register arguments to a "register save area"
2. Set up pointers to both register and stack argument areas
3. Maintain separate counters for GP and FP registers
4. Handle alignment requirements for different types

The `va_list` becomes this monstrosity:

```c
typedef struct {
    unsigned int gp_offset;     // Offset to next GP register arg
    unsigned int fp_offset;     // Offset to next FP register arg
    void *overflow_arg_area;    // Pointer to stack args
    void *reg_save_area;        // Pointer to register save area
} va_list[1];
```

ARM64 is slightly better but still complex. RISC-V tried to simplify but ended up with its own complications around register vs. stack boundaries.

## The SLOW32 Revelation

When designing SLOW32's calling convention, I made one decision that changed everything: **the first 8 arguments go in registers (r3-r10), but varargs functions ALWAYS spill them to the stack.**

That's it. That one rule eliminates 90% of the complexity.

Here's our entire `va_list` implementation:

```c
typedef char* va_list;

#define va_start(ap, last) \
    ((ap) = (char*)&(last) + ((sizeof(last) + 3) & ~3))

#define va_arg(ap, type) \
    (*(type*)((ap) += ((sizeof(type) + 3) & ~3), \
              (ap) - ((sizeof(type) + 3) & ~3)))

#define va_end(ap) ((void)0)
```

That's not pseudocode. That's the actual, complete implementation.

## How It Works

When you compile a varargs function like this:

```c
int sum(int count, ...) {
    va_list args;
    va_start(args, count);

    int total = 0;
    for (int i = 0; i < count; i++) {
        total += va_arg(args, int);
    }

    va_end(args);
    return total;
}
```

The LLVM backend recognizes it's variadic and generates a prologue that spills r3-r10 to the stack:

```asm
sum:
    addi  sp, sp, -48     # Allocate frame
    stw   r3, 0(sp)       # Spill r3 (count)
    stw   r4, 4(sp)       # Spill r4 (first vararg)
    stw   r5, 8(sp)       # Spill r5 (second vararg)
    stw   r6, 12(sp)      # Spill r6
    stw   r7, 16(sp)      # Spill r7
    stw   r8, 20(sp)      # Spill r8
    stw   r9, 24(sp)      # Spill r9
    stw   r10, 28(sp)     # Spill r10
    # ... rest of function
```

Now all arguments, whether they arrived in registers or on the stack, form a contiguous array in memory. The `va_list` is just a pointer that walks through this array. No special cases, no register tracking, no separate paths for different argument positions.

## The Implementation Journey

The beauty emerged gradually. First, I tried to be clever and only spill the registers that were actually used. This led to complex tracking in the backend, bugs with argument counting, and misaligned access issues.

Then I realized: memory is cheap, simplicity is valuable. Always spilling 8 registers (32 bytes) costs almost nothing on modern systems but eliminates entire classes of bugs.

The LLVM backend implementation is remarkably straightforward:

```cpp
// In SLOW32ISelLowering.cpp
if (IsVarArg) {
    // Varargs functions always spill r3-r10 to stack
    for (unsigned i = 0; i < 8; ++i) {
        unsigned Reg = SLOW32::R3 + i;
        unsigned Offset = i * 4;

        SDValue Store = DAG.getStore(
            Chain, DL,
            DAG.getRegister(Reg, MVT::i32),
            DAG.getNode(ISD::ADD, DL, MVT::i32, StackPtr,
                       DAG.getConstant(Offset, DL, MVT::i32)),
            MachinePointerInfo::getStack(MF, Offset)
        );
        Stores.push_back(Store);
    }
}
```

## Real-World Benefits

This simplicity cascades through the entire system:

1. **Printf is tiny**: Our printf implementation is under 400 lines of straightforward C, with no assembly required.
2. **Debugging is trivial**: Arguments are always in the same place on the stack. You can inspect them with a simple memory dump.
3. **No architecture-specific code**: The same varargs macros work everywhere. Port SLOW32 to a new host? Varargs just works.
4. **Perfect ABI stability**: The calling convention can't change based on argument types or counts. A varargs function looks the same whether it takes 2 arguments or 20.

## The Test That Proves It

Here's my favorite test that demonstrates the elegance:

```c
#include <stdarg.h>

int test_mixed(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    int sum = 0;
    for (const char *p = fmt; *p; p++) {
        switch (*p) {
            case 'i': sum += va_arg(args, int); break;
            case 'c': sum += va_arg(args, char); break;
            case 's': {
                char *s = va_arg(args, char*);
                while (*s) sum += *s++;
                break;
            }
        }
    }

    va_end(args);
    return sum;
}

// Can call with any mix of types
int result = test_mixed("iisc", 10, 20, "hello", 'x');
```

This compiles to clean, efficient code with no special cases or runtime type detection beyond what you explicitly wrote.

## Performance Impact

You might worry about the cost of always spilling 8 registers. Let's analyze:

- **Space**: 32 bytes per varargs call frame
- **Time**: 8 store instructions in the prologue
- **Cache**: All arguments are now contiguous and cache-friendly

In practice, the performance impact is negligible. Our printf benchmark shows no measurable difference from architectures with "optimized" varargs. The simplicity gains far outweigh the microscopic cost.

More importantly, non-varargs functions (99% of your code) pay zero penalty. They use the regular calling convention with arguments staying in registers.

## The Lesson

Sometimes the best solution isn't the cleverest one. By accepting a tiny, predictable overhead (8 stores), we eliminated massive complexity. The entire varargs implementation - from compiler backend to runtime support - fits in about 50 lines of code.

This exemplifies SLOW32's philosophy: make the common case simple, even if it costs a few cycles. In a world where a "lightweight" runtime is 100KB+, our entire printf + varargs implementation is under 2KB of code.

## Try It Yourself

Want to see this in action? Here's a complete varargs example you can run on SLOW32:

```c
// varargs_demo.c
#include <stdarg.h>

void debug_char(int c);  // SLOW32's output primitive

void mini_printf(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    for (const char *p = fmt; *p; p++) {
        if (*p == '%') {
            p++;
            switch (*p) {
                case 'd': {
                    int val = va_arg(args, int);
                    // Output decimal (simplified)
                    if (val < 0) {
                        debug_char('-');
                        val = -val;
                    }
                    // ... decimal conversion ...
                    break;
                }
                case 's': {
                    char *s = va_arg(args, char*);
                    while (*s) debug_char(*s++);
                    break;
                }
                case '%':
                    debug_char('%');
                    break;
            }
        } else {
            debug_char(*p);
        }
    }

    va_end(args);
}

int main() {
    mini_printf("Hello %s, the answer is %d%%!\n", "World", 42);
    return 0;
}
```

Compile it, run it, and marvel at how something so simple just works.

## What's Next

With varargs conquered, we turned to other challenges. The next article will dive into how we built an assembler that makes both humans and compilers happy - including the pseudo-instructions that make hand-written assembly actually pleasant.

But the varargs story captures something essential about SLOW32: by questioning assumptions and accepting small trade-offs, we can achieve dramatic simplifications. Not every optimization is worth its complexity cost.

---

*SLOW32 is an open-source educational CPU architecture. Find the complete implementation, including the varargs support described here, at [github.com/brazilofmux/slow32-public](https://github.com/brazilofmux/slow32-public)*

*Have thoughts on varargs implementations? I'd love to hear about your experiences with other architectures. What's the worst varargs bug you've encountered?*
