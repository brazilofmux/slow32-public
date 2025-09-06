
// No include needed - Target.td is already included in SLOW32.td
// RegisterInfo.td is already included in SLOW32.td
include "SLOW32InstrFormats.td"

// Define branch target operand with custom printer
def brtarget : Operand<OtherVT> {
  let PrintMethod = "printBranchTarget";
}

// Define immediate operands with proper constraints
def simm16 : Operand<i32>, ImmLeaf<i32, [{
  return isInt<16>(Imm);
}]>;

def uimm16 : Operand<i32>, ImmLeaf<i32, [{
  return isUInt<16>(Imm);
}]>;

// Define SLOW32-specific SDNodes
def SLOW32hi : SDNode<"SLOW32ISD::HI", SDTIntUnaryOp>;
def SLOW32lo : SDNode<"SLOW32ISD::LO", SDTIntUnaryOp>;

// Type profiles for callseq nodes
def SDT_CallSeqStart : SDCallSeqStart<[SDTCisVT<0, i32>, SDTCisVT<1, i32>]>;
def SDT_CallSeqEnd   : SDCallSeqEnd<[SDTCisVT<0, i32>, SDTCisVT<1, i32>]>;

// Target-independent nodes with target-specific formats
def callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_CallSeqStart,
                           [SDNPHasChain, SDNPOutGlue]>;
def callseq_end   : SDNode<"ISD::CALLSEQ_END",   SDT_CallSeqEnd,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

// Pseudo instruction for loading global addresses with %hi/%lo
// This ensures LUI and ORI stay together in the correct order
let hasSideEffects = 0, isCodeGenOnly = 1, isPseudo = 1 in {
  def LOAD_ADDR : Pseudo<(outs GPR:$rd), (ins i32imm:$addr),
                         "# LOAD_ADDR $rd, $addr", []>;
}

// Pseudo instructions for call stack adjustment
let Defs = [R29], Uses = [R29] in {
  def ADJCALLSTACKDOWN : Pseudo<(outs), (ins i32imm:$amt1, i32imm:$amt2),
                                "# ADJCALLSTACKDOWN $amt1, $amt2", 
                                [(callseq_start timm:$amt1, timm:$amt2)]>;
  def ADJCALLSTACKUP   : Pseudo<(outs), (ins i32imm:$amt1, i32imm:$amt2),
                                "# ADJCALLSTACKUP $amt1, $amt2",
                                [(callseq_end timm:$amt1, timm:$amt2)]>;
}

// Most arithmetic and logical instructions don't access memory
let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
  def ADD  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "add $rd, $rs1, $rs2">;
  def SUB  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "sub $rd, $rs1, $rs2">;
  def AND  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "and $rd, $rs1, $rs2">;
  def OR   : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "or  $rd, $rs1, $rs2">;
  def XOR  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "xor $rd, $rs1, $rs2">;
  def ADDI : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, simm16:$imm), "addi $rd, $rs1, $imm">;
  def ANDI : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, uimm16:$imm), "andi $rd, $rs1, $imm">;
  def ORI  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, uimm16:$imm), "ori  $rd, $rs1, $imm">;
  def XORI : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, uimm16:$imm), "xori $rd, $rs1, $imm">;
  // Register-register shifts
  def SLL  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "sll $rd, $rs1, $rs2">;
  def SRL  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "srl $rd, $rs1, $rs2">;
  def SRA  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "sra $rd, $rs1, $rs2">;
  // Immediate shifts
  def SLLI : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, i32imm:$imm), "slli $rd, $rs1, $imm">;
  def SRLI : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, i32imm:$imm), "srli $rd, $rs1, $imm">;
  def SRAI : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, i32imm:$imm), "srai $rd, $rs1, $imm">;
  // Comparison instructions
  def SLT  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "slt $rd, $rs1, $rs2">;
  def SLTU : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "sltu $rd, $rs1, $rs2">;
  def SEQ  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "seq $rd, $rs1, $rs2">;
  def SNE  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "sne $rd, $rs1, $rs2">;
  def SGT  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "sgt $rd, $rs1, $rs2">;
  def SGTU : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "sgtu $rd, $rs1, $rs2">;
  def SGE  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "sge $rd, $rs1, $rs2">;
  def SGEU : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "sgeu $rd, $rs1, $rs2">;
  def SLE  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "sle $rd, $rs1, $rs2">;
  def SLEU : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "sleu $rd, $rs1, $rs2">;
  
  def MUL  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "mul $rd, $rs1, $rs2">;
  def DIV  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "div $rd, $rs1, $rs2">;
  def REM  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "rem $rd, $rs1, $rs2">;
  def LUI  : S32Inst<(outs GPR:$rd), (ins i32imm:$imm), "lui $rd, $imm">;
  // LI is a pseudo-instruction that expands to ORI rd, r0, imm for small constants
  let isCodeGenOnly = 1 in
  def LI   : S32Inst<(outs GPR:$rd), (ins i32imm:$imm), "ori $rd, r0, $imm">;
}

// Load instructions
let hasSideEffects = 0, mayLoad = 1, mayStore = 0 in {
  def LDB  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, i32imm:$imm), "ldb $rd, $rs1+$imm">;
  def LDBU : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, i32imm:$imm), "ldbu $rd, $rs1+$imm">;
  def LDH  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, i32imm:$imm), "ldh $rd, $rs1+$imm">;
  def LDHU : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, i32imm:$imm), "ldhu $rd, $rs1+$imm">;
  def LDW  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, i32imm:$imm), "ldw $rd, $rs1+$imm">;
}

// Store instructions 
let hasSideEffects = 0, mayLoad = 0, mayStore = 1 in {
  def STB  : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, i32imm:$imm), "stb $rs1+$imm, $rs2">;
  def STH  : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, i32imm:$imm), "sth $rs1+$imm, $rs2">;
  def STW  : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, i32imm:$imm), "stw $rs1+$imm, $rs2">;
}

// Branch instructions
let hasSideEffects = 0, mayLoad = 0, mayStore = 0, isBranch = 1, isTerminator = 1 in {
  def BEQ  : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, i32imm:$imm), "beq $rs1, $rs2, $imm">;
  def BNE  : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, i32imm:$imm), "bne $rs1, $rs2, $imm">;
}

// Jump instructions
let hasSideEffects = 0, mayLoad = 0, mayStore = 0, isBranch = 1, isTerminator = 1, isBarrier = 1 in {
  def JAL  : S32Inst<(outs), (ins i32imm:$sym), "jal r31, $sym">;
  def JALR : S32Inst<(outs), (ins GPR:$rs1), "jalr r31, $rs1, 0">;
}

// Pattern for indirect branch (brind) - use JALR with R0 as link register
// Since we don't need the return address, we use R0 which always reads 0
def : Pat<(brind GPR:$rs1), (JALR GPR:$rs1)>;

def : Pat<(add GPR:$a, GPR:$b), (ADD GPR:$a, GPR:$b)>;
def : Pat<(sub GPR:$a, GPR:$b), (SUB GPR:$a, GPR:$b)>;
def : Pat<(and GPR:$a, GPR:$b), (AND GPR:$a, GPR:$b)>;
def : Pat<(or  GPR:$a, GPR:$b), (OR  GPR:$a, GPR:$b)>;
def : Pat<(mul GPR:$a, GPR:$b), (MUL GPR:$a, GPR:$b)>;
def : Pat<(sdiv GPR:$a, GPR:$b), (DIV GPR:$a, GPR:$b)>;
def : Pat<(srem GPR:$a, GPR:$b), (REM GPR:$a, GPR:$b)>;
def : Pat<(xor GPR:$a, GPR:$b), (XOR GPR:$a, GPR:$b)>;

// Comparison patterns
def : Pat<(setlt GPR:$a, GPR:$b), (SLT GPR:$a, GPR:$b)>;
def : Pat<(setult GPR:$a, GPR:$b), (SLTU GPR:$a, GPR:$b)>;
def : Pat<(seteq GPR:$a, GPR:$b), (SEQ GPR:$a, GPR:$b)>;
def : Pat<(setne GPR:$a, GPR:$b), (SNE GPR:$a, GPR:$b)>;
def : Pat<(setgt GPR:$a, GPR:$b), (SGT GPR:$a, GPR:$b)>;
def : Pat<(setugt GPR:$a, GPR:$b), (SGTU GPR:$a, GPR:$b)>;
def : Pat<(setge GPR:$a, GPR:$b), (SGE GPR:$a, GPR:$b)>;
def : Pat<(setuge GPR:$a, GPR:$b), (SGEU GPR:$a, GPR:$b)>;
def : Pat<(setle GPR:$a, GPR:$b), (SLE GPR:$a, GPR:$b)>;
def : Pat<(setule GPR:$a, GPR:$b), (SLEU GPR:$a, GPR:$b)>;

// Patterns for loading constants
// Small constants that fit in 16 bits (signed or unsigned)
def : Pat<(i32 simm16:$val), (ADDI R0, simm16:$val)>;

// For any constant (fallback pattern using ORI for now)
// This will truncate large constants but at least won't crash
def : Pat<(i32 imm:$val), (ORI R0, imm:$val)>;

// Patterns for %hi/%lo addressing with global addresses
def : Pat<(SLOW32hi tglobaladdr:$addr), (LUI tglobaladdr:$addr)>;
def : Pat<(SLOW32lo tglobaladdr:$addr), (ORI R0, tglobaladdr:$addr)>;

// Patterns for %hi/%lo addressing with external symbols
def : Pat<(SLOW32hi texternalsym:$addr), (LUI texternalsym:$addr)>;
def : Pat<(SLOW32lo texternalsym:$addr), (ORI R0, texternalsym:$addr)>;

// Patterns for %hi/%lo addressing with jump tables
def : Pat<(SLOW32hi tjumptable:$addr), (LUI tjumptable:$addr)>;
def : Pat<(SLOW32lo tjumptable:$addr), (ORI R0, tjumptable:$addr)>;

// Pattern for combining %hi and %lo into a full address using OR
// Note: The LOAD_ADDR pseudo instruction handles global addresses
// to ensure correct instruction ordering (LUI before ORI).
def : Pat<(or (SLOW32hi tglobaladdr:$addr), (SLOW32lo tglobaladdr:$addr)),
          (LOAD_ADDR tglobaladdr:$addr)>;
def : Pat<(or (SLOW32lo tglobaladdr:$addr), (SLOW32hi tglobaladdr:$addr)),
          (LOAD_ADDR tglobaladdr:$addr)>;

// For external symbols, we still use direct patterns.
def : Pat<(or (SLOW32hi texternalsym:$addr), (SLOW32lo texternalsym:$addr)),
          (ORI (LUI texternalsym:$addr), texternalsym:$addr)>;
def : Pat<(or (SLOW32lo texternalsym:$addr), (SLOW32hi texternalsym:$addr)),
          (ORI (LUI texternalsym:$addr), texternalsym:$addr)>;

// For jump tables
def : Pat<(or (SLOW32hi tjumptable:$addr), (SLOW32lo tjumptable:$addr)),
          (ORI (LUI tjumptable:$addr), tjumptable:$addr)>;
def : Pat<(or (SLOW32lo tjumptable:$addr), (SLOW32hi tjumptable:$addr)),
          (ORI (LUI tjumptable:$addr), tjumptable:$addr)>;

// Patterns for loads and stores
def : Pat<(load GPR:$addr), (LDW GPR:$addr, 0)>;
def : Pat<(store GPR:$val, GPR:$addr), (STW GPR:$addr, GPR:$val, 0)>;

// Patterns for loads with offsets (base + immediate)
def : Pat<(load (add GPR:$base, simm16:$offset)), 
          (LDW GPR:$base, simm16:$offset)>;
def : Pat<(sextloadi8 (add GPR:$base, simm16:$offset)), 
          (LDB GPR:$base, simm16:$offset)>;
def : Pat<(zextloadi8 (add GPR:$base, simm16:$offset)), 
          (LDBU GPR:$base, simm16:$offset)>;
def : Pat<(extloadi8 (add GPR:$base, simm16:$offset)), 
          (LDBU GPR:$base, simm16:$offset)>;
def : Pat<(sextloadi16 (add GPR:$base, simm16:$offset)), 
          (LDH GPR:$base, simm16:$offset)>;
def : Pat<(zextloadi16 (add GPR:$base, simm16:$offset)), 
          (LDHU GPR:$base, simm16:$offset)>;
def : Pat<(extloadi16 (add GPR:$base, simm16:$offset)), 
          (LDHU GPR:$base, simm16:$offset)>;

// Patterns for stores with offsets (base + immediate)
def : Pat<(store GPR:$val, (add GPR:$base, simm16:$offset)), 
          (STW GPR:$base, GPR:$val, simm16:$offset)>;
def : Pat<(truncstorei8 GPR:$val, (add GPR:$base, simm16:$offset)), 
          (STB GPR:$base, GPR:$val, simm16:$offset)>;
def : Pat<(truncstorei16 GPR:$val, (add GPR:$base, simm16:$offset)), 
          (STH GPR:$base, GPR:$val, simm16:$offset)>;

// Patterns for byte loads (sign-extended and zero-extended)
def : Pat<(sextloadi8 GPR:$addr), (LDB GPR:$addr, 0)>;
def : Pat<(zextloadi8 GPR:$addr), (LDBU GPR:$addr, 0)>;
def : Pat<(extloadi8 GPR:$addr), (LDBU GPR:$addr, 0)>;

// Patterns for halfword loads (sign-extended and zero-extended)
def : Pat<(sextloadi16 GPR:$addr), (LDH GPR:$addr, 0)>;
def : Pat<(zextloadi16 GPR:$addr), (LDHU GPR:$addr, 0)>;
def : Pat<(extloadi16 GPR:$addr), (LDHU GPR:$addr, 0)>;

// Patterns for byte and halfword stores
def : Pat<(truncstorei8 GPR:$val, GPR:$addr), (STB GPR:$addr, GPR:$val, 0)>;
def : Pat<(truncstorei16 GPR:$val, GPR:$addr), (STH GPR:$addr, GPR:$val, 0)>;

// Sign/zero extension patterns  
// Sign-extend i8 to i32
def : Pat<(sext_inreg GPR:$src, i8),
          (SRAI (SLLI GPR:$src, 24), 24)>;
// Sign-extend i16 to i32
def : Pat<(sext_inreg GPR:$src, i16),
          (SRAI (SLLI GPR:$src, 16), 16)>;

// Pattern for adding small constant to byte and sign-extending
// This handles the common pattern: (sext_inreg (add reg, small_const), i8)
def : Pat<(sext_inreg (add GPR:$src, simm16:$imm), i8),
          (SRAI (SLLI (ADDI GPR:$src, simm16:$imm), 24), 24)>;
// Removed: LLVM canonicalizes immediates to RHS

// For global addresses, use %hi/%lo for proper 32-bit addressing
// Load from global: use LOAD_ADDR pseudo to form address, then load
def : Pat<(load (i32 tglobaladdr:$addr)), 
          (LDW (LOAD_ADDR tglobaladdr:$addr), 0)>;
          
// Store to global: use LOAD_ADDR pseudo to form address, then store
def : Pat<(store GPR:$val, (i32 tglobaladdr:$addr)),
          (STW (LOAD_ADDR tglobaladdr:$addr), GPR:$val, 0)>;

// Patterns for immediate operations
def : Pat<(add GPR:$a, simm16:$b), (ADDI GPR:$a, simm16:$b)>;
def : Pat<(and GPR:$a, uimm16:$b), (ANDI GPR:$a, uimm16:$b)>;
def : Pat<(or  GPR:$a, uimm16:$b), (ORI  GPR:$a, uimm16:$b)>;
def : Pat<(xor GPR:$a, uimm16:$b), (XORI GPR:$a, uimm16:$b)>;

// For large constants that don't fit in 16 bits, materialize them in a register first
// This will be handled by the constant materialization patterns below

// Helper transforms to extract high and low parts of constants
def HI16 : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant((N->getZExtValue() >> 16) & 0xFFFF, SDLoc(N), MVT::i32);
}]>;
def LO16 : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant(N->getZExtValue() & 0xFFFF, SDLoc(N), MVT::i32);
}]>;

// Patterns for materializing constants
// Small constants that fit in 16 bits (signed)
def : Pat<(i32 simm16:$imm), (ADDI R0, simm16:$imm)>;

// Constants that need LUI only (upper 16 bits, lower 16 bits are 0)
def HI16_ONLY : PatLeaf<(imm), [{
  return (N->getZExtValue() & 0xFFFF) == 0 && (N->getZExtValue() >> 16) != 0;
}]>;
def : Pat<(i32 HI16_ONLY:$imm), (LUI (HI16 $imm))>;

// Patterns for shift operations
// Register-register shifts
def : Pat<(shl GPR:$a, GPR:$b), (SLL GPR:$a, GPR:$b)>;
def : Pat<(srl GPR:$a, GPR:$b), (SRL GPR:$a, GPR:$b)>;
def : Pat<(sra GPR:$a, GPR:$b), (SRA GPR:$a, GPR:$b)>;
// Immediate shifts
def : Pat<(shl GPR:$a, imm:$b), (SLLI GPR:$a, imm:$b)>;
def : Pat<(srl GPR:$a, imm:$b), (SRLI GPR:$a, imm:$b)>;
def : Pat<(sra GPR:$a, imm:$b), (SRAI GPR:$a, imm:$b)>;

// The comparison patterns are already defined above (lines 83-92)
// SLOW32 has all comparison instructions as real hardware instructions

// Patterns for immediate comparisons - use LI to load the immediate first
def : Pat<(setlt GPR:$a, imm:$b), (SLT GPR:$a, (LI imm:$b))>;
def : Pat<(setgt GPR:$a, imm:$b), (SGT GPR:$a, (LI imm:$b))>;
def : Pat<(seteq GPR:$a, imm:$b), (SEQ GPR:$a, (LI imm:$b))>;
def : Pat<(setne GPR:$a, imm:$b), (SNE GPR:$a, (LI imm:$b))>;
def : Pat<(setge GPR:$a, imm:$b), (SGE GPR:$a, (LI imm:$b))>;
def : Pat<(setle GPR:$a, imm:$b), (SLE GPR:$a, (LI imm:$b))>;

// Define custom SDNodes

// Return node: no explicit results or operands (chain/glue handled by SDNPHasChain)
def SDT_SLOW32Ret : SDTypeProfile<0, 0, []>;

def SLOW32ret
  : SDNode<"SLOW32ISD::RET_FLAG", SDT_SLOW32Ret,
           [SDNPHasChain, SDNPOptInGlue]>;

def SLOW32brcc : SDNode<"SLOW32ISD::BR_CC", SDT_SLOW32BrCC,
                        [SDNPHasChain]>;

def SLOW32brne : SDNode<"SLOW32ISD::BR_NE", SDT_SLOW32BrCC,
                        [SDNPHasChain]>;

// Call node uses the SDT_SLOW32Call defined in InstrFormats.td
def SLOW32call : SDNode<"SLOW32ISD::CALL", SDT_SLOW32Call,
                        [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue, SDNPVariadic]>;

// Real, encodable (or at least printable) return insn
// R1 is implicitly used for return values (may be undef for void)
// We always set R1 (to undef for void) so Uses=[R1] is always satisfied
// For i32 returns, only R1 is used
let isReturn = 1, isTerminator = 1, isBarrier = 1, Uses = [R1],
    hasSideEffects = 0, mayStore = 0, mayLoad = 0 in
def RET : S32Inst<(outs), (ins), "jalr r0, r31, 0">;

// For i64 returns, both R1 and R2 are used
let isReturn = 1, isTerminator = 1, isBarrier = 1, Uses = [R1, R2], 
    hasSideEffects = 0, mayStore = 0, mayLoad = 0 in
def RET64 : S32Inst<(outs), (ins), "jalr r0, r31, 0">;

// Pattern: RET_FLAG node is now handled in custom instruction selection
// in SLOW32ISelDAGToDAG.cpp to choose between RET and RET64

// For unconditional branches
let isBranch = 1, isTerminator = 1 in {
  def BR : S32Inst<(outs), (ins brtarget:$target), "jal r0, $target", [(br bb:$target)]>;
}

// Conditional branches - all are terminators
let isBranch = 1, isTerminator = 1 in {
  def BEQ_pat : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, brtarget:$target), 
                        "beq $rs1, $rs2, $target",
                        [(SLOW32brcc GPR:$rs1, GPR:$rs2, bb:$target)]>;
  
  def BNE_pat : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, brtarget:$target), 
                        "bne $rs1, $rs2, $target",
                        [(SLOW32brne GPR:$rs1, GPR:$rs2, bb:$target)]>;
  
  def BLT : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, brtarget:$target), 
                    "blt $rs1, $rs2, $target">;
  
  def BGE : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, brtarget:$target), 
                    "bge $rs1, $rs2, $target">;
}

// Pattern matching for brcond with setcc
// These patterns help LLVM convert comparison+branch combinations into single branch instructions

// brcond (seteq a, b), target => beq a, b, target
def : Pat<(brcond (i32 (seteq GPR:$a, GPR:$b)), bb:$target),
          (BEQ_pat GPR:$a, GPR:$b, bb:$target)>;

// brcond (setne a, b), target => bne a, b, target  
def : Pat<(brcond (i32 (setne GPR:$a, GPR:$b)), bb:$target),
          (BNE_pat GPR:$a, GPR:$b, bb:$target)>;

// brcond (setlt a, b), target => blt a, b, target
def : Pat<(brcond (i32 (setlt GPR:$a, GPR:$b)), bb:$target),
          (BLT GPR:$a, GPR:$b, bb:$target)>;

// brcond (setge a, b), target => bge a, b, target
def : Pat<(brcond (i32 (setge GPR:$a, GPR:$b)), bb:$target),
          (BGE GPR:$a, GPR:$b, bb:$target)>;

// Additional patterns for immediate comparisons with branches
// brcond (setlt a, imm), target => load imm to temp, then blt
def : Pat<(brcond (i32 (setlt GPR:$a, imm:$b)), bb:$target),
          (BLT GPR:$a, (LI imm:$b), bb:$target)>;

def : Pat<(brcond (i32 (setge GPR:$a, imm:$b)), bb:$target),
          (BGE GPR:$a, (LI imm:$b), bb:$target)>;

// For comparisons with zero (common in switch lowering)
// brcond reg, target => bne reg, r0, target (branch if reg != 0)
def : Pat<(brcond GPR:$cond, bb:$target),
          (BNE_pat GPR:$cond, R0, bb:$target)>;

// JAL needs special handling for calls - mark it as a call instruction
// Defs includes ALL caller-saved registers that may be clobbered:
// - R1 (return value)
// - R2 (t0 - temporary) 
// - R3-R10 (a0-a7 - arguments, but also caller-saved)
// - R31 (link register)
// Uses includes R29 (stack pointer) - argument registers will be added dynamically via variable_ops
let isCall = 1, Defs = [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R31], Uses = [R29], usesCustomInserter = 0,
    hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
  def JAL_CALL : S32Inst<(outs), (ins brtarget:$target, variable_ops), "jal r31, $target">;
}

// JALR for indirect calls (calling through a register)
let isCall = 1, Defs = [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R31], Uses = [R29], usesCustomInserter = 0,
    hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
  def JALR_CALL : S32Inst<(outs), (ins GPR:$rs1, variable_ops), "jalr r31, $rs1, 0">;
}

// Note: Call patterns are handled manually in SLOW32ISelDAGToDAG.cpp
// because we need to handle the variadic nature of calls and preserve glue chains

//===----------------------------------------------------------------------===//
// Additional patterns for better code generation
//===----------------------------------------------------------------------===//

// Byte and halfword load patterns with different extensions
def : Pat<(i32 (extloadi8 (add GPR:$rs1, simm16:$off))),
          (LDB GPR:$rs1, simm16:$off)>;
def : Pat<(i32 (extloadi16 (add GPR:$rs1, simm16:$off))),
          (LDH GPR:$rs1, simm16:$off)>;

// Zero extension patterns
def : Pat<(i32 (zextloadi8 (add GPR:$rs1, simm16:$off))),
          (LDBU GPR:$rs1, simm16:$off)>;
def : Pat<(i32 (zextloadi16 (add GPR:$rs1, simm16:$off))),
          (LDHU GPR:$rs1, simm16:$off)>;

// Sign extension from i8/i16 without shift sequences
def : Pat<(i32 (sext_inreg GPR:$rs1, i8)),
          (SRA (SLL GPR:$rs1, 24), 24)>;
def : Pat<(i32 (sext_inreg GPR:$rs1, i16)),
          (SRA (SLL GPR:$rs1, 16), 16)>;

// Patterns for adding immediates to different types
def : Pat<(i32 (add GPR:$rs1, simm16:$imm)),
          (ADDI GPR:$rs1, simm16:$imm)>;


// Load/store pairs for memcpy optimization
// These help LLVM see that it can use word-sized operations
multiclass LoadStorePat<ValueType vt, PatFrag ldop, PatFrag stop, 
                        S32Inst ldinst, S32Inst stinst> {
  def : Pat<(vt (ldop GPR:$base)), (ldinst GPR:$base, 0)>;
  def : Pat<(vt (ldop (add GPR:$base, simm16:$off))), 
            (ldinst GPR:$base, simm16:$off)>;
  def : Pat<(stop vt:$val, GPR:$base), (stinst GPR:$base, 0, vt:$val)>;
  def : Pat<(stop vt:$val, (add GPR:$base, simm16:$off)), 
            (stinst GPR:$base, simm16:$off, vt:$val)>;
}

defm : LoadStorePat<i32, load, store, LDW, STW>;

// Patterns for constant materialization
def : Pat<(i32 0), (ADDI R0, 0)>;
def : Pat<(i32 simm16:$imm), (ADDI R0, simm16:$imm)>;
def : Pat<(i32 uimm16:$imm), (ORI R0, uimm16:$imm)>;

// Upper 16-bit constant patterns
def : Pat<(i32 (shl (i32 imm:$imm), (i32 16))),
          (LUI imm:$imm)>;

// Arithmetic with immediates in either order  

// Pseudo for select - will be expanded later
let usesCustomInserter = 1 in {
  def SELECT_PSEUDO : Pseudo<(outs GPR:$dst), 
                             (ins GPR:$cond, GPR:$t, GPR:$f),
                             "# SELECT_PSEUDO $dst, $cond, $t, $f">;
}

// Select patterns (conditional move emulation)
def : Pat<(select (setne GPR:$cond, 0), GPR:$t, GPR:$f),
          (SELECT_PSEUDO GPR:$cond, GPR:$t, GPR:$f)>;
def : Pat<(select (seteq GPR:$cond, 0), GPR:$f, GPR:$t),
          (SELECT_PSEUDO GPR:$cond, GPR:$t, GPR:$f)>;

// For i64 support, we need pseudo instructions for carry operations
// These will be expanded to proper sequences later
let Defs = [R0], usesCustomInserter = 1 in {
  def ADDC_PSEUDO : Pseudo<(outs GPR:$sum, GPR:$carry), (ins GPR:$lhs, GPR:$rhs),
                           "# ADDC_PSEUDO $sum, $carry, $lhs, $rhs">;
  def ADDE_PSEUDO : Pseudo<(outs GPR:$sum, GPR:$carry), (ins GPR:$lhs, GPR:$rhs, GPR:$carryin),
                           "# ADDE_PSEUDO $sum, $carry, $lhs, $rhs, $carryin">;
  def SUBC_PSEUDO : Pseudo<(outs GPR:$diff, GPR:$borrow), (ins GPR:$lhs, GPR:$rhs),
                           "# SUBC_PSEUDO $diff, $borrow, $lhs, $rhs">;
  def SUBE_PSEUDO : Pseudo<(outs GPR:$diff, GPR:$borrow), (ins GPR:$lhs, GPR:$rhs, GPR:$borrowin),
                           "# SUBE_PSEUDO $diff, $borrow, $lhs, $rhs, $borrowin">;
}

// Patterns for multi-word arithmetic
def : Pat<(addc GPR:$lhs, GPR:$rhs),
          (ADDC_PSEUDO GPR:$lhs, GPR:$rhs)>;
def : Pat<(adde GPR:$lhs, GPR:$rhs),
          (ADDE_PSEUDO GPR:$lhs, GPR:$rhs, R0)>;
def : Pat<(subc GPR:$lhs, GPR:$rhs),
          (SUBC_PSEUDO GPR:$lhs, GPR:$rhs)>;
def : Pat<(sube GPR:$lhs, GPR:$rhs),
          (SUBE_PSEUDO GPR:$lhs, GPR:$rhs, R0)>;

