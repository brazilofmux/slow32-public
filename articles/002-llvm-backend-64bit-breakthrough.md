# Building an LLVM Backend: The 64-bit Integer Breakthrough

*How do you implement 64-bit arithmetic on a 32-bit CPU that deliberately has no carry flag? The SLOW-32 LLVM backend journey started with conventional wisdom, hit a wall, and ended with an elegant solution that generates surprisingly tight code. This is that story.*

---

## The Starting Point: Why LLVM?

When building a new CPU architecture, you face a choice: write your own compiler from scratch, or plug into an existing framework. For SLOW-32, the decision was easy:

- **LLVM gives you a world-class optimizer for free** - SSA form, dead code elimination, loop optimizations, the works
- **You inherit decades of compiler research** - register allocation, instruction scheduling, all battle-tested
- **The learning curve is steep but finite** - TableGen takes time to master, but then you're productive

The [SLOW-32 LLVM backend][1] started in late August 2024, heavily inspired by RISC-V's clean design. Within days, we had basic 32-bit arithmetic working. Functions compiled. Loops ran. Everything looked great.

Then we tried to compile this:

```c
long long add64(long long a, long long b) {
    return a + b;
}
```

And the compiler crashed.

---

## The Carry Flag Problem

Here's what makes 64-bit arithmetic interesting on a 32-bit machine: when you add the low 32 bits, you might generate a carry that needs to be added to the high 32 bits.

On x86, you'd do:
```asm
add eax, ecx    ; Add low parts, sets carry flag
adc ebx, edx    ; Add high parts + carry flag
```

That `adc` instruction is "add with carry" - it reads the carry flag from the previous addition. Simple, elegant, and completely impossible on SLOW-32 because **we don't have a carry flag**.

Why no carry flag? It's a deliberate design choice:

- Flags complicate the pipeline (every instruction potentially modifies global state)
- Flags make instruction reordering harder
- Flags are "hidden state" that makes the architecture less transparent

But now we had to pay the price for that simplicity.

---

## Attempt #1: The ADDC/ADDE Approach (Failed)

LLVM has standard operations for this situation: `ADDC` (add with carry out) and `ADDE` (add with carry in). The conventional approach:

```cpp
setOperationAction(ISD::ADDC, MVT::i32, Custom);
setOperationAction(ISD::ADDE, MVT::i32, Custom);
```

Then implement custom lowering that somehow tracks the carry between instructions. We tried:

- Expanding to complex bit manipulation (slow, ugly)
- Using a special register for carry (violated our architecture principles)  
- Various other hacks (all terrible)

After days of fighting with this, we had bloated, incorrect code. A simple 64-bit add was generating 15+ instructions. Something was fundamentally wrong with our approach.

---

## The Breakthrough: UADDO/USUBO

The insight came from studying how LLVM *really* thinks about carries. There's another set of operations: `UADDO` (unsigned add with overflow) and `USUBO` (unsigned subtract with overflow). These don't use flags - they return two values: the result and whether overflow occurred.

Here's the key insight that changed everything:

**For unsigned addition, overflow detection is simple:**

```c
sum = a + b;
carry = (sum < a);  // If sum wrapped around, it's less than either input
```

**For unsigned subtraction:**

```c
diff = a - b;
borrow = (a < b);   // If we're subtracting more than we have
```

No flags needed. Just comparisons!

---

## The Implementation

Here's the actual implementation that made it work:

```cpp
// In SLOW32ISelLowering.cpp
setOperationAction(ISD::UADDO, MVT::i32, Custom);
setOperationAction(ISD::USUBO, MVT::i32, Custom);

SDValue SLOW32TargetLowering::LowerUADDO(SDValue Op, SelectionDAG &DAG) const {
  SDLoc dl(Op);
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  
  // Perform the addition
  SDValue Sum = DAG.getNode(ISD::ADD, dl, MVT::i32, LHS, RHS);
  
  // Check for overflow: sum < LHS means we wrapped
  SDValue Overflow = DAG.getSetCC(dl, MVT::i32, Sum, LHS, ISD::SETULT);
  
  // Return both results
  return DAG.getMergeValues({Sum, Overflow}, dl);
}
```

The magic is in that `SETULT` (Set on Unsigned Less Than) - it becomes a simple `SLTU` instruction in SLOW-32.

---

## The Result: Beautiful Assembly

Here's what that original 64-bit add compiles to now:

```asm
add64:
    add r2, r4, r6      # Add high 32 bits
    add r1, r3, r5      # Add low 32 bits  
    sltu r3, r1, r3     # Check if low add overflowed (sum < operand)
    add r2, r2, r3      # Add carry to high bits
    jalr r0, lr, 0      # Return
```

**Four instructions** for a 64-bit add on a 32-bit machine with no carry flag. That's remarkably efficient!

For subtraction:

```asm
sub64:
    sub r2, r4, r6      # Subtract high 32 bits
    sltu r3, r3, r5     # Check if we'll need to borrow
    sub r2, r2, r3      # Subtract borrow from high bits
    sub r1, r3, r5      # Subtract low 32 bits
    jalr r0, lr, 0      # Return
```

---

## Testing the Edge Cases

The real test is boundary conditions. Here's our test that confirms it works:

```c
// test_64bit.c
unsigned long long test_carry() {
    unsigned long long a = 0xFFFFFFFFULL;  // Maximum 32-bit value
    unsigned long long b = 1;
    return a + b;  // Should be 0x100000000
}

unsigned long long test_borrow() {
    unsigned long long a = 0x100000000ULL;  // 2^32
    unsigned long long b = 1;
    return a - b;  // Should be 0xFFFFFFFF
}
```

Both compile correctly and produce the right results. The carry propagates properly from the low word to the high word.

---

## Lessons Learned

1. **Don't fight the architecture** - We tried to force SLOW-32 to pretend it had carry flags. Wrong approach.

2. **LLVM has multiple ways to do things** - ADDC/ADDE aren't the only way. UADDO/USUBO are more flexible.

3. **Simple comparisons are powerful** - `(sum < operand)` is a beautiful way to detect overflow.

4. **Clean architecture pays off** - Because SLOW-32 has regular comparisons that put results in registers (not flags), this solution just works.

---

## What's Next in the Backend Story

This 64-bit breakthrough was just one chapter in the LLVM backend journey. In upcoming articles, we'll dive into:

- **Varargs Mystery**: Why were arguments appearing in reverse order? (Spoiler: register save ordering)
- **Jump Tables**: How to implement switch statements without a native jump table instruction
- **The SELECT Problem**: Why conditional moves are trickier than they look
- **Calling Convention Crimes**: How we were accidentally clobbering registers across function calls

Each of these has its own "aha!" moment and clever solution.

---

## Try It Yourself

The complete LLVM backend is now available in the [SLOW-32 repository][1]. You can:

1. Build LLVM with the SLOW-32 target
2. Compile C code with 64-bit integers
3. See the generated assembly
4. Run it on the emulator

The [integration guide][2] walks through adding SLOW-32 to your LLVM build.

---

## The Bigger Picture

This 64-bit integer story exemplifies what makes SLOW-32 interesting as a teaching architecture. By removing traditional features (carry flags), we force ourselves to think differently about fundamental operations. The resulting solutions are often cleaner and more elegant than the traditional approaches.

Sometimes, less really is more.

*Next: "Varargs and the Case of the Backwards Arguments" - a debugging detective story from the SLOW-32 backend.*

---

## Links

[1]: https://github.com/brazilofmux/slow32-public/tree/main/llvm-backend
[2]: https://github.com/brazilofmux/slow32-public/blob/main/llvm-backend/README.md
