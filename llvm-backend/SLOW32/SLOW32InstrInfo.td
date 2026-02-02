
// No include needed - Target.td is already included in SLOW32.td
// RegisterInfo.td is already included in SLOW32.td
include "SLOW32InstrFormats.td"

// Define branch target operand with custom printer
def brtarget : Operand<OtherVT> {
  let PrintMethod = "printBranchTarget";
}

// Define immediate operands with proper constraints (RV-style 12-bit window)
def simm12 : Operand<i32>, ImmLeaf<i32, [{
  return isInt<12>(Imm);
}]>;

def uimm12 : Operand<i32>, ImmLeaf<i32, [{
  return isUInt<12>(Imm);
}]>;

// Complex address pattern for load/store instructions
// SelectAddr splits addresses into base + offset
def SLOW32Addr : ComplexPattern<iPTR, 2, "SelectAddr", [], []>;

def GPRPairOp : RegisterOperand<GPRPair> {
  let PrintMethod = "printGPRPairOperand";
}

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
  def ADDI : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, simm12:$imm), "addi $rd, $rs1, $imm">;
  def ANDI : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, simm12:$imm), "andi $rd, $rs1, $imm">;
  def ORI  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, simm12:$imm), "ori  $rd, $rs1, $imm">;
  def XORI : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, simm12:$imm), "xori $rd, $rs1, $imm">;
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

// Floating-point instructions (f32)
let hasSideEffects = 0 in {
  def FADD_S  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "fadd.s $rd, $rs1, $rs2">;
  def FSUB_S  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "fsub.s $rd, $rs1, $rs2">;
  def FMUL_S  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "fmul.s $rd, $rs1, $rs2">;
  def FDIV_S  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "fdiv.s $rd, $rs1, $rs2">;
  def FSQRT_S : S32Inst<(outs GPR:$rd), (ins GPR:$rs1), "fsqrt.s $rd, $rs1">;

  def FEQ_S : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "feq.s $rd, $rs1, $rs2">;
  def FLT_S : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "flt.s $rd, $rs1, $rs2">;
  def FLE_S : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2), "fle.s $rd, $rs1, $rs2">;

  def FCVT_W_S  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1), "fcvt.w.s $rd, $rs1">;
  def FCVT_WU_S : S32Inst<(outs GPR:$rd), (ins GPR:$rs1), "fcvt.wu.s $rd, $rs1">;
  def FCVT_S_W  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1), "fcvt.s.w $rd, $rs1">;
  def FCVT_S_WU : S32Inst<(outs GPR:$rd), (ins GPR:$rs1), "fcvt.s.wu $rd, $rs1">;

  def FNEG_S : S32Inst<(outs GPR:$rd), (ins GPR:$rs1), "fneg.s $rd, $rs1">;
  def FABS_S : S32Inst<(outs GPR:$rd), (ins GPR:$rs1), "fabs.s $rd, $rs1">;
}

// Floating-point instructions (f64 register pairs)
let hasSideEffects = 0 in {
  def FADD_D  : S32Inst<(outs GPRPairOp:$rd), (ins GPRPairOp:$rs1, GPRPairOp:$rs2), "fadd.d $rd, $rs1, $rs2">;
  def FSUB_D  : S32Inst<(outs GPRPairOp:$rd), (ins GPRPairOp:$rs1, GPRPairOp:$rs2), "fsub.d $rd, $rs1, $rs2">;
  def FMUL_D  : S32Inst<(outs GPRPairOp:$rd), (ins GPRPairOp:$rs1, GPRPairOp:$rs2), "fmul.d $rd, $rs1, $rs2">;
  def FDIV_D  : S32Inst<(outs GPRPairOp:$rd), (ins GPRPairOp:$rs1, GPRPairOp:$rs2), "fdiv.d $rd, $rs1, $rs2">;
  def FSQRT_D : S32Inst<(outs GPRPairOp:$rd), (ins GPRPairOp:$rs1), "fsqrt.d $rd, $rs1">;

  def FEQ_D : S32Inst<(outs GPR:$rd), (ins GPRPairOp:$rs1, GPRPairOp:$rs2), "feq.d $rd, $rs1, $rs2">;
  def FLT_D : S32Inst<(outs GPR:$rd), (ins GPRPairOp:$rs1, GPRPairOp:$rs2), "flt.d $rd, $rs1, $rs2">;
  def FLE_D : S32Inst<(outs GPR:$rd), (ins GPRPairOp:$rs1, GPRPairOp:$rs2), "fle.d $rd, $rs1, $rs2">;

  def FCVT_W_D  : S32Inst<(outs GPR:$rd), (ins GPRPairOp:$rs1), "fcvt.w.d $rd, $rs1">;
  def FCVT_WU_D : S32Inst<(outs GPR:$rd), (ins GPRPairOp:$rs1), "fcvt.wu.d $rd, $rs1">;
  def FCVT_D_W  : S32Inst<(outs GPRPairOp:$rd), (ins GPR:$rs1), "fcvt.d.w $rd, $rs1">;
  def FCVT_D_WU : S32Inst<(outs GPRPairOp:$rd), (ins GPR:$rs1), "fcvt.d.wu $rd, $rs1">;
  def FCVT_D_S  : S32Inst<(outs GPRPairOp:$rd), (ins GPR:$rs1), "fcvt.d.s $rd, $rs1">;
  def FCVT_S_D  : S32Inst<(outs GPR:$rd), (ins GPRPairOp:$rs1), "fcvt.s.d $rd, $rs1">;

  def FNEG_D : S32Inst<(outs GPRPairOp:$rd), (ins GPRPairOp:$rs1), "fneg.d $rd, $rs1">;
  def FABS_D : S32Inst<(outs GPRPairOp:$rd), (ins GPRPairOp:$rs1), "fabs.d $rd, $rs1">;
}

// Float/int64 conversions (register pairs) — reached via custom ISD nodes
// (FCVT_L, FCVT_LU, FCVT_FROM_L, FCVT_FROM_LU) selected in ISel.
let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
  def FCVT_L_S  : S32Inst<(outs GPRPairOp:$rd), (ins GPR:$rs1), "fcvt.l.s $rd, $rs1">;
  def FCVT_LU_S : S32Inst<(outs GPRPairOp:$rd), (ins GPR:$rs1), "fcvt.lu.s $rd, $rs1">;
  def FCVT_S_L  : S32Inst<(outs GPR:$rd), (ins GPRPairOp:$rs1), "fcvt.s.l $rd, $rs1">;
  def FCVT_S_LU : S32Inst<(outs GPR:$rd), (ins GPRPairOp:$rs1), "fcvt.s.lu $rd, $rs1">;
  def FCVT_L_D  : S32Inst<(outs GPRPairOp:$rd), (ins GPRPairOp:$rs1), "fcvt.l.d $rd, $rs1">;
  def FCVT_LU_D : S32Inst<(outs GPRPairOp:$rd), (ins GPRPairOp:$rs1), "fcvt.lu.d $rd, $rs1">;
  def FCVT_D_L  : S32Inst<(outs GPRPairOp:$rd), (ins GPRPairOp:$rs1), "fcvt.d.l $rd, $rs1">;
  def FCVT_D_LU : S32Inst<(outs GPRPairOp:$rd), (ins GPRPairOp:$rs1), "fcvt.d.lu $rd, $rs1">;
}

// Load instructions
let hasSideEffects = 0, mayLoad = 1, mayStore = 0 in {
  def LDB  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, simm12:$imm), "ldb $rd, $rs1+$imm">;
  def LDBU : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, simm12:$imm), "ldbu $rd, $rs1+$imm">;
  def LDH  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, simm12:$imm), "ldh $rd, $rs1+$imm">;
  def LDHU : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, simm12:$imm), "ldhu $rd, $rs1+$imm">;
  def LDW  : S32Inst<(outs GPR:$rd), (ins GPR:$rs1, simm12:$imm), "ldw $rd, $rs1+$imm">;
}

// Store instructions 
let hasSideEffects = 0, mayLoad = 0, mayStore = 1 in {
  def STB  : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, simm12:$imm), "stb $rs1+$imm, $rs2">;
  def STH  : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, simm12:$imm), "sth $rs1+$imm, $rs2">;
  def STW  : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, simm12:$imm), "stw $rs1+$imm, $rs2">;
}

// Branch instructions
let hasSideEffects = 0, mayLoad = 0, mayStore = 0, isBranch = 1, isTerminator = 1 in {
  def BEQ  : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, i32imm:$imm), "beq $rs1, $rs2, $imm">;
  def BNE  : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, i32imm:$imm), "bne $rs1, $rs2, $imm">;
}

// Jump instructions
// JAL is primarily used for calls, not branches. Don't mark it as isBranch
// to avoid confusing branch analysis passes.
let hasSideEffects = 0, mayLoad = 0, mayStore = 0, isCall = 1 in {
  def JAL  : S32Inst<(outs), (ins i32imm:$sym), "jal r31, $sym">;
}

// JALR can be used for indirect calls or indirect branches
let hasSideEffects = 0, mayLoad = 0, mayStore = 0, isBranch = 1, isTerminator = 1,
    isBarrier = 1, isIndirectBranch = 1 in {
  def JALR : S32Inst<(outs GPR:$rd), (ins GPR:$rs1), "jalr $rd, $rs1, 0">;
}

// Call-marked encodable forms to preserve call semantics through late passes.
let isCall = 1, Defs = [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R31],
    Uses = [R29], hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
  def JAL_CALLR  : S32Inst<(outs), (ins i32imm:$sym), "jal r31, $sym">;
  def JALR_CALLR : S32Inst<(outs GPR:$rd), (ins GPR:$rs1), "jalr $rd, $rs1, 0">;
}

// Pattern for indirect branch (brind) - use JALR with R0 as link register
// Since we don't need the return address, expand via pseudo to force rd = r0
let isCodeGenOnly = 1, isPseudo = 1, isBranch = 1, isTerminator = 1, isBarrier = 1,
    isIndirectBranch = 1, hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
def BRIND_JALR : S32Inst<(outs), (ins GPR:$rs1), "# BRIND_JALR $rs1">;

def : Pat<(brind (i32 GPR:$rs1)), (BRIND_JALR (i32 GPR:$rs1))>;

def : Pat<(add (i32 GPR:$a), (i32 GPR:$b)), (ADD (i32 GPR:$a), (i32 GPR:$b))>;
def : Pat<(sub (i32 GPR:$a), (i32 GPR:$b)), (SUB (i32 GPR:$a), (i32 GPR:$b))>;
def : Pat<(and (i32 GPR:$a), (i32 GPR:$b)), (AND (i32 GPR:$a), (i32 GPR:$b))>;
def : Pat<(or  (i32 GPR:$a), (i32 GPR:$b)), (OR  (i32 GPR:$a), (i32 GPR:$b))>;
def : Pat<(mul (i32 GPR:$a), (i32 GPR:$b)), (MUL (i32 GPR:$a), (i32 GPR:$b))>;
def : Pat<(sdiv (i32 GPR:$a), (i32 GPR:$b)), (DIV (i32 GPR:$a), (i32 GPR:$b))>;
def : Pat<(srem (i32 GPR:$a), (i32 GPR:$b)), (REM (i32 GPR:$a), (i32 GPR:$b))>;
def : Pat<(xor (i32 GPR:$a), (i32 GPR:$b)), (XOR (i32 GPR:$a), (i32 GPR:$b))>;

// Comparison patterns
def : Pat<(setlt (i32 GPR:$a), (i32 GPR:$b)), (SLT (i32 GPR:$a), (i32 GPR:$b))>;
def : Pat<(setult (i32 GPR:$a), (i32 GPR:$b)), (SLTU (i32 GPR:$a), (i32 GPR:$b))>;
def : Pat<(seteq (i32 GPR:$a), (i32 GPR:$b)), (SEQ (i32 GPR:$a), (i32 GPR:$b))>;
def : Pat<(setne (i32 GPR:$a), (i32 GPR:$b)), (SNE (i32 GPR:$a), (i32 GPR:$b))>;
def : Pat<(setgt (i32 GPR:$a), (i32 GPR:$b)), (SGT (i32 GPR:$a), (i32 GPR:$b))>;
def : Pat<(setugt (i32 GPR:$a), (i32 GPR:$b)), (SGTU (i32 GPR:$a), (i32 GPR:$b))>;
def : Pat<(setge (i32 GPR:$a), (i32 GPR:$b)), (SGE (i32 GPR:$a), (i32 GPR:$b))>;
def : Pat<(setuge (i32 GPR:$a), (i32 GPR:$b)), (SGEU (i32 GPR:$a), (i32 GPR:$b))>;
def : Pat<(setle (i32 GPR:$a), (i32 GPR:$b)), (SLE (i32 GPR:$a), (i32 GPR:$b))>;
def : Pat<(setule (i32 GPR:$a), (i32 GPR:$b)), (SLEU (i32 GPR:$a), (i32 GPR:$b))>;

// Floating-point patterns (f32)
def : Pat<(fadd (f32 GPR:$a), (f32 GPR:$b)), (FADD_S (f32 GPR:$a), (f32 GPR:$b))>;
def : Pat<(fsub (f32 GPR:$a), (f32 GPR:$b)), (FSUB_S (f32 GPR:$a), (f32 GPR:$b))>;
def : Pat<(fmul (f32 GPR:$a), (f32 GPR:$b)), (FMUL_S (f32 GPR:$a), (f32 GPR:$b))>;
def : Pat<(fdiv (f32 GPR:$a), (f32 GPR:$b)), (FDIV_S (f32 GPR:$a), (f32 GPR:$b))>;
def : Pat<(fsqrt (f32 GPR:$a)), (FSQRT_S (f32 GPR:$a))>;
def : Pat<(fneg (f32 GPR:$a)), (FNEG_S (f32 GPR:$a))>;
def : Pat<(fabs (f32 GPR:$a)), (FABS_S (f32 GPR:$a))>;
def : Pat<(seteq (f32 GPR:$a), (f32 GPR:$b)), (FEQ_S (f32 GPR:$a), (f32 GPR:$b))>;
def : Pat<(setlt (f32 GPR:$a), (f32 GPR:$b)), (FLT_S (f32 GPR:$a), (f32 GPR:$b))>;
def : Pat<(setle (f32 GPR:$a), (f32 GPR:$b)), (FLE_S (f32 GPR:$a), (f32 GPR:$b))>;
// Derived f32 comparisons: gt/ge via operand swap, ne via XOR-with-1
def : Pat<(setgt (f32 GPR:$a), (f32 GPR:$b)), (FLT_S (f32 GPR:$b), (f32 GPR:$a))>;
def : Pat<(setge (f32 GPR:$a), (f32 GPR:$b)), (FLE_S (f32 GPR:$b), (f32 GPR:$a))>;
def : Pat<(setne (f32 GPR:$a), (f32 GPR:$b)),
          (XORI (FEQ_S (f32 GPR:$a), (f32 GPR:$b)), 1)>;
// Ordered f32 comparisons
def : Pat<(setoeq (f32 GPR:$a), (f32 GPR:$b)), (FEQ_S (f32 GPR:$a), (f32 GPR:$b))>;
def : Pat<(setone (f32 GPR:$a), (f32 GPR:$b)),
          (XORI (FEQ_S (f32 GPR:$a), (f32 GPR:$b)), 1)>;
def : Pat<(setolt (f32 GPR:$a), (f32 GPR:$b)), (FLT_S (f32 GPR:$a), (f32 GPR:$b))>;
def : Pat<(setole (f32 GPR:$a), (f32 GPR:$b)), (FLE_S (f32 GPR:$a), (f32 GPR:$b))>;
def : Pat<(setogt (f32 GPR:$a), (f32 GPR:$b)), (FLT_S (f32 GPR:$b), (f32 GPR:$a))>;
def : Pat<(setoge (f32 GPR:$a), (f32 GPR:$b)), (FLE_S (f32 GPR:$b), (f32 GPR:$a))>;
// SETO/SETUO — ordered/unordered tests (NaN detection via self-equality)
def : Pat<(seto (f32 GPR:$a), (f32 GPR:$b)),
          (AND (FEQ_S (f32 GPR:$a), (f32 GPR:$a)),
               (FEQ_S (f32 GPR:$b), (f32 GPR:$b)))>;
def : Pat<(setuo (f32 GPR:$a), (f32 GPR:$b)),
          (XORI (AND (FEQ_S (f32 GPR:$a), (f32 GPR:$a)),
                     (FEQ_S (f32 GPR:$b), (f32 GPR:$b))), 1)>;
// Unordered f32 comparisons (same instructions — NaN handling is architectural)
def : Pat<(setueq (f32 GPR:$a), (f32 GPR:$b)), (FEQ_S (f32 GPR:$a), (f32 GPR:$b))>;
def : Pat<(setune (f32 GPR:$a), (f32 GPR:$b)),
          (XORI (FEQ_S (f32 GPR:$a), (f32 GPR:$b)), 1)>;
def : Pat<(setult (f32 GPR:$a), (f32 GPR:$b)), (FLT_S (f32 GPR:$a), (f32 GPR:$b))>;
def : Pat<(setule (f32 GPR:$a), (f32 GPR:$b)), (FLE_S (f32 GPR:$a), (f32 GPR:$b))>;
def : Pat<(setugt (f32 GPR:$a), (f32 GPR:$b)), (FLT_S (f32 GPR:$b), (f32 GPR:$a))>;
def : Pat<(setuge (f32 GPR:$a), (f32 GPR:$b)), (FLE_S (f32 GPR:$b), (f32 GPR:$a))>;
def : Pat<(i32 (fp_to_sint (f32 GPR:$a))), (FCVT_W_S (f32 GPR:$a))>;
def : Pat<(i32 (fp_to_uint (f32 GPR:$a))), (FCVT_WU_S (f32 GPR:$a))>;
def : Pat<(f32 (sint_to_fp (i32 GPR:$a))), (FCVT_S_W (i32 GPR:$a))>;
def : Pat<(f32 (uint_to_fp (i32 GPR:$a))), (FCVT_S_WU (i32 GPR:$a))>;

// Floating-point patterns (f64)
def : Pat<(fadd (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)), (FADD_D GPRPairOp:$a, GPRPairOp:$b)>;
def : Pat<(fsub (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)), (FSUB_D GPRPairOp:$a, GPRPairOp:$b)>;
def : Pat<(fmul (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)), (FMUL_D GPRPairOp:$a, GPRPairOp:$b)>;
def : Pat<(fdiv (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)), (FDIV_D GPRPairOp:$a, GPRPairOp:$b)>;
def : Pat<(fsqrt (f64 GPRPairOp:$a)), (FSQRT_D GPRPairOp:$a)>;
def : Pat<(fneg (f64 GPRPairOp:$a)), (FNEG_D GPRPairOp:$a)>;
def : Pat<(fabs (f64 GPRPairOp:$a)), (FABS_D GPRPairOp:$a)>;
def : Pat<(seteq (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)),
          (FEQ_D (f64 GPRPairOp:$a), (f64 GPRPairOp:$b))>;
def : Pat<(setlt (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)),
          (FLT_D (f64 GPRPairOp:$a), (f64 GPRPairOp:$b))>;
def : Pat<(setle (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)),
          (FLE_D (f64 GPRPairOp:$a), (f64 GPRPairOp:$b))>;
// Derived f64 comparisons
def : Pat<(setgt (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)),
          (FLT_D (f64 GPRPairOp:$b), (f64 GPRPairOp:$a))>;
def : Pat<(setge (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)),
          (FLE_D (f64 GPRPairOp:$b), (f64 GPRPairOp:$a))>;
def : Pat<(setne (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)),
          (XORI (FEQ_D (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)), 1)>;
// Ordered f64 comparisons
def : Pat<(setoeq (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)),
          (FEQ_D (f64 GPRPairOp:$a), (f64 GPRPairOp:$b))>;
def : Pat<(setone (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)),
          (XORI (FEQ_D (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)), 1)>;
def : Pat<(setolt (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)),
          (FLT_D (f64 GPRPairOp:$a), (f64 GPRPairOp:$b))>;
def : Pat<(setole (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)),
          (FLE_D (f64 GPRPairOp:$a), (f64 GPRPairOp:$b))>;
def : Pat<(setogt (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)),
          (FLT_D (f64 GPRPairOp:$b), (f64 GPRPairOp:$a))>;
def : Pat<(setoge (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)),
          (FLE_D (f64 GPRPairOp:$b), (f64 GPRPairOp:$a))>;
// SETO/SETUO for f64
def : Pat<(seto (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)),
          (AND (FEQ_D (f64 GPRPairOp:$a), (f64 GPRPairOp:$a)),
               (FEQ_D (f64 GPRPairOp:$b), (f64 GPRPairOp:$b)))>;
def : Pat<(setuo (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)),
          (XORI (AND (FEQ_D (f64 GPRPairOp:$a), (f64 GPRPairOp:$a)),
                     (FEQ_D (f64 GPRPairOp:$b), (f64 GPRPairOp:$b))), 1)>;
// Unordered f64 comparisons
def : Pat<(setueq (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)),
          (FEQ_D (f64 GPRPairOp:$a), (f64 GPRPairOp:$b))>;
def : Pat<(setune (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)),
          (XORI (FEQ_D (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)), 1)>;
def : Pat<(setult (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)),
          (FLT_D (f64 GPRPairOp:$a), (f64 GPRPairOp:$b))>;
def : Pat<(setule (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)),
          (FLE_D (f64 GPRPairOp:$a), (f64 GPRPairOp:$b))>;
def : Pat<(setugt (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)),
          (FLT_D (f64 GPRPairOp:$b), (f64 GPRPairOp:$a))>;
def : Pat<(setuge (f64 GPRPairOp:$a), (f64 GPRPairOp:$b)),
          (FLE_D (f64 GPRPairOp:$b), (f64 GPRPairOp:$a))>;
def : Pat<(i32 (fp_to_sint GPRPairOp:$a)), (FCVT_W_D GPRPairOp:$a)>;
def : Pat<(i32 (fp_to_uint GPRPairOp:$a)), (FCVT_WU_D GPRPairOp:$a)>;
def : Pat<(f64 (sint_to_fp (i32 GPR:$a))), (FCVT_D_W (i32 GPR:$a))>;
def : Pat<(f64 (uint_to_fp (i32 GPR:$a))), (FCVT_D_WU (i32 GPR:$a))>;
def : Pat<(f64 (fpextend (f32 GPR:$a))), (FCVT_D_S (f32 GPR:$a))>;
def : Pat<(f32 (fpround GPRPairOp:$a)), (FCVT_S_D GPRPairOp:$a)>;
// i64 fp conversion patterns removed — i64 is now illegal; the type legalizer
// splits i64 into two i32 halves before ISel, so these patterns can't match.

// Patterns for loading constants
// Small constants that fit in 16 bits (signed or unsigned)
def : Pat<(i32 simm12:$val), (ADDI (i32 R0), simm12:$val)>;

// Patterns for %hi/%lo addressing with global addresses
def : Pat<(SLOW32hi tglobaladdr:$addr), (LUI tglobaladdr:$addr)>;
def : Pat<(SLOW32hi texternalsym:$addr), (LUI texternalsym:$addr)>;
def : Pat<(SLOW32hi tjumptable:$addr), (LUI tjumptable:$addr)>;

// DAG lowering now materialises globals via the LOAD_ADDR pseudo, so no extra
// combination patterns are required here.


// Patterns for loads and stores
def : Pat<(i32 (load (i32 GPR:$addr))), (LDW (i32 GPR:$addr), 0)>;
def : Pat<(store (i32 GPR:$val), (i32 GPR:$addr)),
          (STW (i32 GPR:$addr), (i32 GPR:$val), 0)>;

// Patterns for loads with offsets (base + immediate)
// Also use ComplexPattern for better address matching
def : Pat<(i32 (load (add (i32 GPR:$base), simm12:$offset))),
          (LDW (i32 GPR:$base), simm12:$offset)>;
def : Pat<(i32 (sextloadi8 (add (i32 GPR:$base), simm12:$offset))),
          (LDB (i32 GPR:$base), simm12:$offset)>;
def : Pat<(i32 (zextloadi8 (add (i32 GPR:$base), simm12:$offset))),
          (LDBU (i32 GPR:$base), simm12:$offset)>;
def : Pat<(i32 (extloadi8 (add (i32 GPR:$base), simm12:$offset))),
          (LDBU (i32 GPR:$base), simm12:$offset)>;
def : Pat<(i32 (sextloadi16 (add (i32 GPR:$base), simm12:$offset))),
          (LDH (i32 GPR:$base), simm12:$offset)>;
def : Pat<(i32 (zextloadi16 (add (i32 GPR:$base), simm12:$offset))),
          (LDHU (i32 GPR:$base), simm12:$offset)>;
def : Pat<(i32 (extloadi16 (add (i32 GPR:$base), simm12:$offset))),
          (LDHU (i32 GPR:$base), simm12:$offset)>;

// Patterns for stores with offsets (base + immediate)
def : Pat<(store (i32 GPR:$val), (add (i32 GPR:$base), simm12:$offset)),
          (STW (i32 GPR:$base), (i32 GPR:$val), simm12:$offset)>;
def : Pat<(truncstorei8 (i32 GPR:$val), (add (i32 GPR:$base), simm12:$offset)),
          (STB (i32 GPR:$base), (i32 GPR:$val), simm12:$offset)>;
def : Pat<(truncstorei16 (i32 GPR:$val), (add (i32 GPR:$base), simm12:$offset)),
          (STH (i32 GPR:$base), (i32 GPR:$val), simm12:$offset)>;

// Patterns for byte loads (sign-extended and zero-extended)
// Keep the simple patterns with explicit addressing modes
def : Pat<(i32 (sextloadi8 (i32 GPR:$addr))), (LDB (i32 GPR:$addr), 0)>;
def : Pat<(i32 (zextloadi8 (i32 GPR:$addr))), (LDBU (i32 GPR:$addr), 0)>;
def : Pat<(i32 (extloadi8 (i32 GPR:$addr))), (LDBU (i32 GPR:$addr), 0)>;

// Patterns for halfword loads (sign-extended and zero-extended)
def : Pat<(i32 (sextloadi16 (i32 GPR:$addr))), (LDH (i32 GPR:$addr), 0)>;
def : Pat<(i32 (zextloadi16 (i32 GPR:$addr))), (LDHU (i32 GPR:$addr), 0)>;
def : Pat<(i32 (extloadi16 (i32 GPR:$addr))), (LDHU (i32 GPR:$addr), 0)>;

// Patterns for byte and halfword stores
def : Pat<(truncstorei8 (i32 GPR:$val), (i32 GPR:$addr)),
          (STB (i32 GPR:$addr), (i32 GPR:$val), 0)>;
def : Pat<(truncstorei16 (i32 GPR:$val), (i32 GPR:$addr)),
          (STH (i32 GPR:$addr), (i32 GPR:$val), 0)>;

// Sign/zero extension patterns  
// Sign-extend i8 to i32
def : Pat<(sext_inreg (i32 GPR:$src), i8),
          (SRAI (SLLI (i32 GPR:$src), 24), 24)>;
// Sign-extend i16 to i32
def : Pat<(sext_inreg (i32 GPR:$src), i16),
          (SRAI (SLLI (i32 GPR:$src), 16), 16)>;

// Pattern for adding small constant to byte and sign-extending
// This handles the common pattern: (sext_inreg (add reg, small_const), i8)
def : Pat<(sext_inreg (add (i32 GPR:$src), simm12:$imm), i8),
          (SRAI (SLLI (ADDI (i32 GPR:$src), simm12:$imm), 24), 24)>;
// Removed: LLVM canonicalizes immediates to RHS

// For global addresses, use %hi/%lo for proper 32-bit addressing
// Pattern for loading a global address (including function addresses) into a register
def : Pat<(i32 tglobaladdr:$addr), (i32 (LOAD_ADDR (i32 tglobaladdr:$addr)))>;

// Load from global: use LOAD_ADDR pseudo to form address, then load
def : Pat<(i32 (load (i32 tglobaladdr:$addr))),
          (LDW (i32 (LOAD_ADDR (i32 tglobaladdr:$addr))), 0)>;
          
// Store to global: use LOAD_ADDR pseudo to form address, then store
def : Pat<(store (i32 GPR:$val), (i32 tglobaladdr:$addr)),
          (STW (i32 (LOAD_ADDR (i32 tglobaladdr:$addr))), (i32 GPR:$val), 0)>;

// Patterns for immediate operations
def : Pat<(add (i32 GPR:$a), simm12:$b), (ADDI (i32 GPR:$a), simm12:$b)>;
def : Pat<(and (i32 GPR:$a), uimm12:$b), (ANDI (i32 GPR:$a), uimm12:$b)>;
def : Pat<(or  (i32 GPR:$a), uimm12:$b), (ORI  (i32 GPR:$a), uimm12:$b)>;
def : Pat<(xor (i32 GPR:$a), uimm12:$b), (XORI (i32 GPR:$a), uimm12:$b)>;

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
def : Pat<(i32 simm12:$imm), (ADDI (i32 R0), simm12:$imm)>;

// Constants that need LUI only (upper 16 bits, lower 16 bits are 0)
def HI16_ONLY : PatLeaf<(imm), [{
  return (N->getZExtValue() & 0xFFFF) == 0 && (N->getZExtValue() >> 16) != 0;
}]>;
def : Pat<(i32 HI16_ONLY:$imm), (LUI (HI16 $imm))>;

// Patterns for shift operations
// Register-register shifts
def : Pat<(shl (i32 GPR:$a), (i32 GPR:$b)), (SLL (i32 GPR:$a), (i32 GPR:$b))>;
def : Pat<(srl (i32 GPR:$a), (i32 GPR:$b)), (SRL (i32 GPR:$a), (i32 GPR:$b))>;
def : Pat<(sra (i32 GPR:$a), (i32 GPR:$b)), (SRA (i32 GPR:$a), (i32 GPR:$b))>;
// Immediate shifts
def : Pat<(shl (i32 GPR:$a), simm12:$b), (SLLI (i32 GPR:$a), simm12:$b)>;
def : Pat<(srl (i32 GPR:$a), simm12:$b), (SRLI (i32 GPR:$a), simm12:$b)>;
def : Pat<(sra (i32 GPR:$a), simm12:$b), (SRAI (i32 GPR:$a), simm12:$b)>;

// The comparison patterns are already defined above (lines 83-92)
// SLOW32 has all comparison instructions as real hardware instructions

// Patterns for immediate comparisons - only for small immediates (12-bit signed)
// Large constants will be materialized separately by ISD::Constant handling
// in Select() which generates proper LUI+ADDI sequences
def : Pat<(setlt (i32 GPR:$a), simm12:$b), (SLT (i32 GPR:$a), (ADDI (i32 R0), simm12:$b))>;
def : Pat<(setgt (i32 GPR:$a), simm12:$b), (SGT (i32 GPR:$a), (ADDI (i32 R0), simm12:$b))>;
def : Pat<(seteq (i32 GPR:$a), simm12:$b), (SEQ (i32 GPR:$a), (ADDI (i32 R0), simm12:$b))>;
def : Pat<(setne (i32 GPR:$a), simm12:$b), (SNE (i32 GPR:$a), (ADDI (i32 R0), simm12:$b))>;
def : Pat<(setge (i32 GPR:$a), simm12:$b), (SGE (i32 GPR:$a), (ADDI (i32 R0), simm12:$b))>;
def : Pat<(setle (i32 GPR:$a), simm12:$b), (SLE (i32 GPR:$a), (ADDI (i32 R0), simm12:$b))>;

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

def SLOW32brlt  : SDNode<"SLOW32ISD::BR_LT",  SDT_SLOW32BrCC, [SDNPHasChain]>;
def SLOW32brge  : SDNode<"SLOW32ISD::BR_GE",  SDT_SLOW32BrCC, [SDNPHasChain]>;
def SLOW32brgt  : SDNode<"SLOW32ISD::BR_GT",  SDT_SLOW32BrCC, [SDNPHasChain]>;
def SLOW32brle  : SDNode<"SLOW32ISD::BR_LE",  SDT_SLOW32BrCC, [SDNPHasChain]>;
def SLOW32brltu : SDNode<"SLOW32ISD::BR_LTU", SDT_SLOW32BrCC, [SDNPHasChain]>;
def SLOW32brgeu : SDNode<"SLOW32ISD::BR_GEU", SDT_SLOW32BrCC, [SDNPHasChain]>;
def SLOW32brgtu : SDNode<"SLOW32ISD::BR_GTU", SDT_SLOW32BrCC, [SDNPHasChain]>;
def SLOW32brleu : SDNode<"SLOW32ISD::BR_LEU", SDT_SLOW32BrCC, [SDNPHasChain]>;

// Call node uses the SDT_SLOW32Call defined in InstrFormats.td
def SLOW32call : SDNode<"SLOW32ISD::CALL", SDT_SLOW32Call,
                        [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue, SDNPVariadic]>;

// Real, encodable (or at least printable) return insn
// R1 is implicitly used for return values (may be undef for void)
// We always set R1 (to undef for void) so Uses=[R1] is always satisfied
// For i32 returns, only R1 is used
// isNotDuplicable prevents tail duplication which can cause Branch Folder issues
let isReturn = 1, isTerminator = 1, isBarrier = 1, isNotDuplicable = 1,
    Uses = [R1], hasSideEffects = 0, mayStore = 0, mayLoad = 0 in
def RET : S32Inst<(outs), (ins), "jalr r0, r31, 0">;

// For i64 returns, both R1 and R2 are used
let isReturn = 1, isTerminator = 1, isBarrier = 1, isNotDuplicable = 1,
    Uses = [R1, R2], hasSideEffects = 0, mayStore = 0, mayLoad = 0 in
def RET64 : S32Inst<(outs), (ins), "jalr r0, r31, 0">;

// Pattern: RET_FLAG node is now handled in custom instruction selection
// in SLOW32ISelDAGToDAG.cpp to choose between RET and RET64

// For unconditional branches
// CRITICAL: isBarrier=1 needed to prevent optimizer thinking there's fallthrough!
let isBranch = 1, isTerminator = 1, isBarrier = 1 in {
  def BR : S32Inst<(outs), (ins brtarget:$target), "jal r0, $target", [(br bb:$target)]>;
}

// Long-branch pseudos (expanded post-RA)
// Carry an explicit scratch register so both the Machine and MC paths can
// agree on which temporary gets clobbered by the materialisation sequence.
let isBranch = 1, isTerminator = 1, isBarrier = 1,
    hasSideEffects = 0, mayLoad = 0, mayStore = 0,
    isCodeGenOnly = 1, isPseudo = 1 in {
  def PseudoLongBR : S32Inst<(outs GPR:$scratch), (ins brtarget:$target),
                             "# PseudoLongBR $target">;
}

// Conditional branches - all are terminators but NOT barriers (can fall through)
let isBranch = 1, isTerminator = 1, isBarrier = 0, hasSideEffects = 0 in {
  def BEQ_pat : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, brtarget:$target), 
                        "beq $rs1, $rs2, $target",
                        [(SLOW32brcc GPR:$rs1, GPR:$rs2, bb:$target)]>;
  
  def BNE_pat : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, brtarget:$target), 
                        "bne $rs1, $rs2, $target",
                        [(SLOW32brne GPR:$rs1, GPR:$rs2, bb:$target)]>;
  
  def BLT : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, brtarget:$target), 
                    "blt $rs1, $rs2, $target",
                    [(SLOW32brlt GPR:$rs1, GPR:$rs2, bb:$target)]>;
  
  def BGE : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, brtarget:$target), 
                    "bge $rs1, $rs2, $target",
                    [(SLOW32brge GPR:$rs1, GPR:$rs2, bb:$target)]>;

  def BGT : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, brtarget:$target),
                    "bgt $rs1, $rs2, $target",
                    [(SLOW32brgt GPR:$rs1, GPR:$rs2, bb:$target)]>;

  def BLE : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, brtarget:$target),
                    "ble $rs1, $rs2, $target",
                    [(SLOW32brle GPR:$rs1, GPR:$rs2, bb:$target)]>;

  def BLTU : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, brtarget:$target),
                     "bltu $rs1, $rs2, $target",
                     [(SLOW32brltu GPR:$rs1, GPR:$rs2, bb:$target)]>;

  def BGEU : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, brtarget:$target),
                     "bgeu $rs1, $rs2, $target",
                     [(SLOW32brgeu GPR:$rs1, GPR:$rs2, bb:$target)]>;

  def BGTU : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, brtarget:$target),
                     "bgtu $rs1, $rs2, $target",
                     [(SLOW32brgtu GPR:$rs1, GPR:$rs2, bb:$target)]>;

  def BLEU : S32Inst<(outs), (ins GPR:$rs1, GPR:$rs2, brtarget:$target),
                     "bleu $rs1, $rs2, $target",
                     [(SLOW32brleu GPR:$rs1, GPR:$rs2, bb:$target)]>;
}

let isBranch = 1, isTerminator = 1, isBarrier = 0, hasSideEffects = 0,
    mayLoad = 0, mayStore = 0,
    isCodeGenOnly = 1, isPseudo = 1 in {
  def PseudoLongBEQ : S32Inst<(outs GPR:$scratch),
                            (ins GPR:$rs1, GPR:$rs2, brtarget:$target),
                            "# PseudoLongBEQ $target">;
  def PseudoLongBNE : S32Inst<(outs GPR:$scratch),
                            (ins GPR:$rs1, GPR:$rs2, brtarget:$target),
                            "# PseudoLongBNE $target">;
  def PseudoLongBLT : S32Inst<(outs GPR:$scratch),
                            (ins GPR:$rs1, GPR:$rs2, brtarget:$target),
                            "# PseudoLongBLT $target">;
  def PseudoLongBGE : S32Inst<(outs GPR:$scratch),
                            (ins GPR:$rs1, GPR:$rs2, brtarget:$target),
                            "# PseudoLongBGE $target">;
  def PseudoLongBGT : S32Inst<(outs GPR:$scratch),
                            (ins GPR:$rs1, GPR:$rs2, brtarget:$target),
                            "# PseudoLongBGT $target">;
  def PseudoLongBLE : S32Inst<(outs GPR:$scratch),
                            (ins GPR:$rs1, GPR:$rs2, brtarget:$target),
                            "# PseudoLongBLE $target">;
  def PseudoLongBLTU : S32Inst<(outs GPR:$scratch),
                             (ins GPR:$rs1, GPR:$rs2, brtarget:$target),
                             "# PseudoLongBLTU $target">;
  def PseudoLongBGEU : S32Inst<(outs GPR:$scratch),
                             (ins GPR:$rs1, GPR:$rs2, brtarget:$target),
                             "# PseudoLongBGEU $target">;
  def PseudoLongBGTU : S32Inst<(outs GPR:$scratch),
                             (ins GPR:$rs1, GPR:$rs2, brtarget:$target),
                             "# PseudoLongBGTU $target">;
  def PseudoLongBLEU : S32Inst<(outs GPR:$scratch),
                             (ins GPR:$rs1, GPR:$rs2, brtarget:$target),
                             "# PseudoLongBLEU $target">;
}

// Pattern matching for brcond with setcc
// These patterns help LLVM convert comparison+branch combinations into single branch instructions

// brcond (seteq a, b), target => beq a, b, target
def : Pat<(brcond (i32 (seteq (i32 GPR:$a), (i32 GPR:$b))), bb:$target),
          (BEQ_pat (i32 GPR:$a), (i32 GPR:$b), bb:$target)>;

// brcond (setne a, b), target => bne a, b, target  
def : Pat<(brcond (i32 (setne (i32 GPR:$a), (i32 GPR:$b))), bb:$target),
          (BNE_pat (i32 GPR:$a), (i32 GPR:$b), bb:$target)>;

// brcond (setlt a, b), target => blt a, b, target
def : Pat<(brcond (i32 (setlt (i32 GPR:$a), (i32 GPR:$b))), bb:$target),
          (BLT (i32 GPR:$a), (i32 GPR:$b), bb:$target)>;

// brcond (setge a, b), target => bge a, b, target
def : Pat<(brcond (i32 (setge (i32 GPR:$a), (i32 GPR:$b))), bb:$target),
          (BGE (i32 GPR:$a), (i32 GPR:$b), bb:$target)>;

// brcond (setgt a, b), target => bgt a, b, target
def : Pat<(brcond (i32 (setgt (i32 GPR:$a), (i32 GPR:$b))), bb:$target),
          (BGT (i32 GPR:$a), (i32 GPR:$b), bb:$target)>;

// brcond (setle a, b), target => ble a, b, target
def : Pat<(brcond (i32 (setle (i32 GPR:$a), (i32 GPR:$b))), bb:$target),
          (BLE (i32 GPR:$a), (i32 GPR:$b), bb:$target)>;

// Unsigned branch patterns
def : Pat<(brcond (i32 (setult (i32 GPR:$a), (i32 GPR:$b))), bb:$target),
          (BLTU (i32 GPR:$a), (i32 GPR:$b), bb:$target)>;

def : Pat<(brcond (i32 (setuge (i32 GPR:$a), (i32 GPR:$b))), bb:$target),
          (BGEU (i32 GPR:$a), (i32 GPR:$b), bb:$target)>;

def : Pat<(brcond (i32 (setugt (i32 GPR:$a), (i32 GPR:$b))), bb:$target),
          (BGTU (i32 GPR:$a), (i32 GPR:$b), bb:$target)>;

def : Pat<(brcond (i32 (setule (i32 GPR:$a), (i32 GPR:$b))), bb:$target),
          (BLEU (i32 GPR:$a), (i32 GPR:$b), bb:$target)>;

// Additional patterns for immediate comparisons with branches
// brcond (setlt a, imm), target => load imm to temp, then blt
def : Pat<(brcond (i32 (setlt (i32 GPR:$a), imm:$b)), bb:$target),
          (BLT (i32 GPR:$a), (LI imm:$b), bb:$target)>;

def : Pat<(brcond (i32 (setge (i32 GPR:$a), imm:$b)), bb:$target),
          (BGE (i32 GPR:$a), (LI imm:$b), bb:$target)>;

def : Pat<(brcond (i32 (setgt (i32 GPR:$a), imm:$b)), bb:$target),
          (BGT (i32 GPR:$a), (LI imm:$b), bb:$target)>;

def : Pat<(brcond (i32 (setle (i32 GPR:$a), imm:$b)), bb:$target),
          (BLE (i32 GPR:$a), (LI imm:$b), bb:$target)>;

def : Pat<(brcond (i32 (setult (i32 GPR:$a), imm:$b)), bb:$target),
          (BLTU (i32 GPR:$a), (LI imm:$b), bb:$target)>;

def : Pat<(brcond (i32 (setuge (i32 GPR:$a), imm:$b)), bb:$target),
          (BGEU (i32 GPR:$a), (LI imm:$b), bb:$target)>;

def : Pat<(brcond (i32 (setugt (i32 GPR:$a), imm:$b)), bb:$target),
          (BGTU (i32 GPR:$a), (LI imm:$b), bb:$target)>;

def : Pat<(brcond (i32 (setule (i32 GPR:$a), imm:$b)), bb:$target),
          (BLEU (i32 GPR:$a), (LI imm:$b), bb:$target)>;

// For comparisons with zero (common in switch lowering)
// brcond reg, target => bne reg, r0, target (branch if reg != 0)
def : Pat<(brcond (i32 GPR:$cond), bb:$target),
          (BNE_pat (i32 GPR:$cond), (i32 R0), bb:$target)>;

// JAL needs special handling for calls - mark it as a call instruction
// Defs includes ALL caller-saved registers that may be clobbered:
// - R1 (return value)
// - R2 (t0 - temporary) 
// - R3-R10 (a0-a7 - arguments, but also caller-saved)
// - R31 (link register)
// R29 (SP) is a reserved register and implicitly available — do NOT list it
// in Uses, or every block containing a call would need R29 as a live-in,
// which breaks when calls appear in non-entry blocks.
// Argument registers will be added dynamically via variable_ops.
let isCall = 1, Defs = [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R31], usesCustomInserter = 0,
    hasSideEffects = 0, mayLoad = 0, mayStore = 0, isCodeGenOnly = 1, isPseudo = 1 in {
  def JAL_CALL : S32Inst<(outs), (ins brtarget:$target, variable_ops), "# JAL_CALL $target">;
}

// JALR for indirect calls (calling through a register)
let isCall = 1, Defs = [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R31], usesCustomInserter = 0,
    hasSideEffects = 0, mayLoad = 0, mayStore = 0, isCodeGenOnly = 1, isPseudo = 1 in {
  def JALR_CALL : S32Inst<(outs), (ins GPR:$rs1, variable_ops), "# JALR_CALL $rs1">;
}

// Note: Call patterns are handled manually in SLOW32ISelDAGToDAG.cpp
// because we need to handle the variadic nature of calls and glue chains properly

//===----------------------------------------------------------------------===//
// Additional patterns for better code generation
//===----------------------------------------------------------------------===//

// Byte and halfword load patterns with different extensions
def : Pat<(i32 (extloadi8 (add (i32 GPR:$rs1), simm12:$off))),
          (LDB (i32 GPR:$rs1), simm12:$off)>;
def : Pat<(i32 (extloadi16 (add (i32 GPR:$rs1), simm12:$off))),
          (LDH (i32 GPR:$rs1), simm12:$off)>;

// Zero extension patterns
def : Pat<(i32 (zextloadi8 (add (i32 GPR:$rs1), simm12:$off))),
          (LDBU (i32 GPR:$rs1), simm12:$off)>;
def : Pat<(i32 (zextloadi16 (add (i32 GPR:$rs1), simm12:$off))),
          (LDHU (i32 GPR:$rs1), simm12:$off)>;

// Sign extension from i8/i16 without shift sequences
def : Pat<(i32 (sext_inreg (i32 GPR:$rs1), i8)),
          (SRA (SLL (i32 GPR:$rs1), 24), 24)>;
def : Pat<(i32 (sext_inreg (i32 GPR:$rs1), i16)),
          (SRA (SLL (i32 GPR:$rs1), 16), 16)>;

// Patterns for adding immediates to different types
def : Pat<(i32 (add (i32 GPR:$rs1), simm12:$imm)),
          (ADDI (i32 GPR:$rs1), simm12:$imm)>;


// Load/store pairs for memcpy optimization
// These help LLVM see that it can use word-sized operations
multiclass LoadStorePat<ValueType vt, PatFrag ldop, PatFrag stop, 
                        S32Inst ldinst, S32Inst stinst> {
  def : Pat<(vt (ldop (i32 GPR:$base))), (ldinst (i32 GPR:$base), 0)>;
  def : Pat<(vt (ldop (add (i32 GPR:$base), simm12:$off))),
            (ldinst (i32 GPR:$base), simm12:$off)>;
  def : Pat<(stop vt:$val, (i32 GPR:$base)), (stinst (i32 GPR:$base), 0, vt:$val)>;
  def : Pat<(stop vt:$val, (add (i32 GPR:$base), simm12:$off)),
            (stinst (i32 GPR:$base), simm12:$off, vt:$val)>;
}

defm : LoadStorePat<i32, load, store, LDW, STW>;

// f32 load/store patterns — f32 shares GPR with i32, uses LDW/STW.
// Written explicitly because the multiclass store patterns need i32-typed operands.
def : Pat<(f32 (load (i32 GPR:$base))), (LDW (i32 GPR:$base), 0)>;
def : Pat<(f32 (load (add (i32 GPR:$base), simm12:$off))),
          (LDW (i32 GPR:$base), simm12:$off)>;
def : Pat<(store (f32 GPR:$val), (i32 GPR:$base)),
          (STW (i32 GPR:$base), (f32 GPR:$val), 0)>;
def : Pat<(store (f32 GPR:$val), (add (i32 GPR:$base), simm12:$off)),
          (STW (i32 GPR:$base), (f32 GPR:$val), simm12:$off)>;

// Patterns for constant materialization
def : Pat<(i32 0), (ADDI (i32 R0), 0)>;
def : Pat<(i32 simm12:$imm), (ADDI (i32 R0), simm12:$imm)>;
def : Pat<(i32 uimm12:$imm), (ORI (i32 R0), uimm12:$imm)>;

// Upper 16-bit constant patterns
def : Pat<(i32 (shl (i32 imm:$imm), (i32 16))),
          (LUI imm:$imm)>;

// Arithmetic with immediates in either order  

// Pseudo for select - will be expanded later
let usesCustomInserter = 1 in {
  let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
  def SELECT_PSEUDO : Pseudo<(outs GPR:$dst), 
                             (ins GPR:$cond, GPR:$t, GPR:$f),
                             "# SELECT_PSEUDO $dst, $cond, $t, $f">;
}

// f64 split/combine pseudos — expanded in EmitInstrWithCustomInserter via stack.
let usesCustomInserter = 1 in {
  let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
    def BuildPairF64Pseudo : Pseudo<(outs GPRPairOp:$dst),
                                    (ins GPR:$lo, GPR:$hi),
                                    "# BuildPairF64 $dst, $lo, $hi">;
    def SplitF64Pseudo : Pseudo<(outs GPR:$lo, GPR:$hi),
                                (ins GPRPairOp:$src),
                                "# SplitF64 $src -> $lo, $hi">;
  }
}

// Select patterns (conditional move emulation)
// SELECT lowering is handled in SLOW32ISelLowering; avoid pattern ambiguity
// now that GPR includes both i32 and f32.

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
def : Pat<(addc (i32 GPR:$lhs), (i32 GPR:$rhs)),
          (ADDC_PSEUDO (i32 GPR:$lhs), (i32 GPR:$rhs))>;
def : Pat<(adde (i32 GPR:$lhs), (i32 GPR:$rhs)),
          (ADDE_PSEUDO (i32 GPR:$lhs), (i32 GPR:$rhs), (i32 R0))>;
def : Pat<(subc (i32 GPR:$lhs), (i32 GPR:$rhs)),
          (SUBC_PSEUDO (i32 GPR:$lhs), (i32 GPR:$rhs))>;
def : Pat<(sube (i32 GPR:$lhs), (i32 GPR:$rhs)),
          (SUBE_PSEUDO (i32 GPR:$lhs), (i32 GPR:$rhs), (i32 R0))>;
