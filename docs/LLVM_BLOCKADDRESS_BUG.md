# LLVM BlockAddress / Computed Goto Bug

## Status: OPEN — workaround in place, fix needed

## Summary

The SLOW-32 LLVM backend does not implement `LowerBlockAddress`, causing
`blockaddress` / `indirectbr` (computed goto) to silently generate incorrect
code. All block-address targets collapse to the same address, making computed
goto dispatch non-functional.

This was discovered during the Lua 5.4.7 port. Lua's VM uses a computed goto
dispatch table (`goto *disptab[opcode]`) when `LUA_USE_JUMPTABLE` is enabled,
which GCC/Clang enable by default when `__GNUC__` is defined.

## Workaround

Lua: `LUA_USE_JUMPTABLE 0` in `lua/src/luaconf.h` forces standard `switch`
dispatch. Any other C code using computed goto (`&&label` / `goto *ptr`) will
hit the same issue.

## Root Cause

In `llvm-backend/SLOW32/SLOW32ISelLowering.cpp`, the following ISD nodes all
have custom lowering via the `%hi`/`%lo` LOAD_ADDR pseudo:

- `ISD::GlobalAddress` → `LowerGlobalAddress()`
- `ISD::ExternalSymbol` → `LowerExternalSymbol()`
- `ISD::JumpTable` → `LowerJumpTable()`
- `ISD::ConstantPool` → `LowerConstantPool()`

**`ISD::BlockAddress` is missing.** It is never registered with
`setOperationAction` and has no `LowerBlockAddress()` implementation.

Without custom lowering, LLVM's default legalization for `BlockAddress` doesn't
know how to materialize the address for SLOW-32's `%hi`/`%lo` addressing mode.
The result is that all `blockaddress(@fn, %label)` values resolve to the same
(wrong) address.

Meanwhile, the MC layer *does* handle `MO_BlockAddress` correctly:
- `SLOW32MCInstLowering.cpp` (line ~110): `GetBlockAddressSymbol()` + offset
- `SLOW32LoadAddrOpt.cpp`: `isBlockAddress()` recognized in operand adjustment

So the assembler/linker path is fine — only the SelectionDAG lowering is missing.

## How `switch` Jump Tables Differ

Standard `switch` statements use `ISD::JumpTable` + `ISD::BRIND`, which works
correctly. The distinction:

| Feature | switch jump table | computed goto |
|---------|------------------|---------------|
| IR | `switch` → jump table | `blockaddress` + `indirectbr` |
| ISD node | `ISD::JumpTable` | `ISD::BlockAddress` |
| Lowering | `LowerJumpTable()` (implemented) | `LowerBlockAddress()` (**missing**) |
| Indirect branch | `BRIND_JALR` (works) | `BRIND_JALR` (works) |

The `BRIND_JALR` pseudo and `brind` pattern are fine. The problem is solely in
materializing the address of a basic block label.

## Proposed Fix

Add `LowerBlockAddress()` following the same pattern as `LowerGlobalAddress()`:

```cpp
// In SLOW32ISelLowering.cpp:

SDValue SLOW32TargetLowering::LowerBlockAddress(SDValue Op,
                                                 SelectionDAG &DAG) const {
  SDLoc DL(Op);
  BlockAddressSDNode *BA = cast<BlockAddressSDNode>(Op);
  EVT VT = Op.getValueType();

  SDValue TargetBA = DAG.getTargetBlockAddress(BA->getBlockAddress(), VT,
                                                BA->getOffset());
  SDNode *Mov = DAG.getMachineNode(SLOW32::LOAD_ADDR, DL, VT, TargetBA);
  return SDValue(Mov, 0);
}
```

And register it:

```cpp
// In the constructor:
setOperationAction(ISD::BlockAddress, MVT::i32, Custom);
```

And add the case to `LowerOperation()`:

```cpp
case ISD::BlockAddress: return LowerBlockAddress(Op, DAG);
```

Don't forget the declaration in `SLOW32ISelLowering.h`:

```cpp
SDValue LowerBlockAddress(SDValue Op, SelectionDAG &DAG) const;
```

## Testing

After implementing the fix:

1. Build a minimal computed goto test:
   ```c
   #include <stdio.h>
   void test(void) {
       void *table[] = { &&L0, &&L1, &&L2 };
       int i = 0;
       goto *table[i]; L0: printf("zero\n"); i=1; goto *table[i];
       L1: printf("one\n"); i=2; goto *table[i];
       L2: printf("two\n"); return;
   }
   int main(void) { test(); return 0; }
   ```

2. Verify each label gets a distinct address in the generated assembly.

3. Re-enable `LUA_USE_JUMPTABLE` in Lua and verify all 10 tests still pass.
   The computed goto dispatch is measurably faster than switch dispatch in
   interpreters, so this is worth enabling.

## Files to Modify

- `llvm-backend/SLOW32/SLOW32ISelLowering.h` — add declaration
- `llvm-backend/SLOW32/SLOW32ISelLowering.cpp` — add `LowerBlockAddress()`,
  register `ISD::BlockAddress`, add case to `LowerOperation()`

## Related

- `BRIND_JALR` pseudo: `SLOW32InstrInfo.td` line 202–206
- `brind` pattern: `SLOW32InstrInfo.td` line 206
- MC BlockAddress handling: `SLOW32MCInstLowering.cpp` lines 110–125
- LoadAddrOpt BlockAddress support: `SLOW32LoadAddrOpt.cpp` lines 59, 66
