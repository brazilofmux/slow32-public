
// CallingConvLower.td is needed for calling convention definitions
def CC_SLOW32 : CallingConv< [
  // i64 values use register pairs - return value in R1:R2
  CCIfType<[i64], CCAssignToRegWithShadow<[R1, R3, R5, R7, R9], 
                                           [R2, R4, R6, R8, R10]>>,
  // i32 values use single registers
  CCIfType<[i32], CCAssignToReg<[R1]>>,
  CCIfType<[i32], CCAssignToReg<[R3,R4,R5,R6,R7,R8,R9,R10]>>,
  CCIfType<[i32], CCAssignToStack<4, 4>>,
  // i64 values on stack need 8-byte slots with 4-byte alignment
  CCIfType<[i64], CCAssignToStack<8, 4>>
]>;
