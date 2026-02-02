
// CallingConvLower.td is needed for calling convention definitions

// Argument passing: up to eight 32-bit integer GPRs (r3-r10). 64-bit integers
// occupy a pair of registers and otherwise spill to the caller-provided stack
// slots in 4-byte aligned chunks. Byval arguments use the generic support in
// CCState which will carve out register and stack space as needed.
def CC_SLOW32 : CallingConv<[
  CCIfByVal<CCPassByVal<4, 4>>,
  CCIfType<[f64], CCAssignToRegWithShadow<[R3, R5, R7, R9],
                                          [R4, R6, R8, R10]>>,
  CCIfType<[i64], CCAssignToRegWithShadow<[R3, R5, R7, R9],
                                           [R4, R6, R8, R10]>>,
  CCIfType<[f32], CCAssignToReg<[R3, R4, R5, R6, R7, R8, R9, R10]>>,
  CCIfType<[i32], CCAssignToReg<[R3, R4, R5, R6, R7, R8, R9, R10]>>,
  CCIfType<[f64], CCAssignToStack<8, 4>>,
  CCIfType<[i64], CCAssignToStack<8, 4>>,
  CCIfType<[f32], CCAssignToStack<4, 4>>,
  CCIfType<[i32], CCAssignToStack<4, 4>>
]>;

// Return values: scalars in r1 (and r2 for 64-bit). Anything that doesn't fit
// spills to a caller-allocated memory slot.
def RetCC_SLOW32 : CallingConv<[
  CCIfType<[f64], CCAssignToRegWithShadow<[R1], [R2]>>,
  CCIfType<[i64], CCAssignToRegWithShadow<[R1], [R2]>>,
  CCIfType<[f32], CCAssignToReg<[R1]>>,
  CCIfType<[i32], CCAssignToReg<[R1]>>,
  CCIfType<[f64], CCAssignToStack<8, 4>>,
  CCIfType<[i64], CCAssignToStack<8, 4>>,
  CCIfType<[f32], CCAssignToStack<4, 4>>,
  CCIfType<[i32], CCAssignToStack<4, 4>>
]>;

// Callee-saved register list for the default calling convention.
def CSR_SLOW32 : CalleeSavedRegs<(add
  R11, R12, R13, R14, R15, R16, R17, R18, R19, R20,
  R21, R22, R23, R24, R25, R26, R27, R28,
  R30, R31
)>;
