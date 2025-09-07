#include "SLOW32ISelLowering.h"
#include "SLOW32.h"
#include "SLOW32MachineFunctionInfo.h"
#include "SLOW32RegisterInfo.h"
#include "SLOW32Subtarget.h"
#include "SLOW32TargetMachine.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/Debug.h"

using namespace llvm;

SLOW32TargetLowering::SLOW32TargetLowering(const TargetMachine &TM)
  : TargetLowering(TM) {
  // Set up the register classes
  addRegisterClass(MVT::i32, &SLOW32::GPRRegClass);

  // SLOW32 has no floating point support - all float operations
  // must be handled via soft-float library calls
  // We explicitly do NOT add register classes for f32/f64

  // Compute derived properties from the register classes
  const SLOW32Subtarget &STI = static_cast<const SLOW32TargetMachine&>(TM).getSubtarget();
  computeRegisterProperties(STI.getRegisterInfo());

  // Old flag/glue family → expand away
  setOperationAction(ISD::ADDC, MVT::i32, Expand);
  setOperationAction(ISD::ADDE, MVT::i32, Expand);
  setOperationAction(ISD::SUBC, MVT::i32, Expand);
  setOperationAction(ISD::SUBE, MVT::i32, Expand);

  // Value-carry family → we'll lower them
  setOperationAction(ISD::UADDO,        MVT::i32, Custom);
  setOperationAction(ISD::UADDO_CARRY,  MVT::i32, Custom);
  setOperationAction(ISD::USUBO,        MVT::i32, Custom);
  setOperationAction(ISD::USUBO_CARRY,  MVT::i32, Custom);

  // Keep i64 add/sub split
  setOperationAction(ISD::ADD, MVT::i64, Expand);
  setOperationAction(ISD::SUB, MVT::i64, Expand);

  // Booleans come back as 0/1 in a GPR
  setBooleanContents(ZeroOrOneBooleanContent);
  setBooleanVectorContents(ZeroOrOneBooleanContent);
  
  // Configure SETCC to produce 0/1 in a GPR (will use default lowering)
  // This is important for SELECT to work correctly
  setOperationAction(ISD::SETCC, MVT::i32, Legal);
  
  // Configure BRCOND to be Custom (we have a lowering for it)
  setOperationAction(ISD::BRCOND, MVT::Other, Custom);

  // Tell LLVM to expand i64 operations into ADDC/ADDE and SUBC/SUBE pairs
  setOperationAction(ISD::MUL, MVT::i64, Expand);
  
  // Custom lower 32-bit multiply-high operations for i64 multiply expansion
  setOperationAction(ISD::UMUL_LOHI, MVT::i32, Custom);
  setOperationAction(ISD::SMUL_LOHI, MVT::i32, Custom);
  setOperationAction(ISD::MULHS, MVT::i32, Custom);
  setOperationAction(ISD::MULHU, MVT::i32, Custom);
  
  // i64 division/remainder - let LLVM expand these
  // Since SLOW32 doesn't have hardware support, LLVM will generate
  // calls to compiler-rt functions (__divdi3, __udivdi3, __moddi3, __umoddi3)
  setOperationAction(ISD::SDIV, MVT::i64, Expand);
  setOperationAction(ISD::UDIV, MVT::i64, Expand);
  setOperationAction(ISD::SREM, MVT::i64, Expand);
  setOperationAction(ISD::UREM, MVT::i64, Expand);
  
  // SLOW32 is not recognized in LLVM's RuntimeLibcalls infrastructure,
  // so we need to explicitly set which libcall implementations to use for i64 div/rem
  // These use the standard compiler-rt/libgcc implementations
  setLibcallImpl(RTLIB::SDIV_I64, RTLIB::impl___divdi3);
  setLibcallImpl(RTLIB::UDIV_I64, RTLIB::impl___udivdi3);
  setLibcallImpl(RTLIB::SREM_I64, RTLIB::impl___moddi3);
  setLibcallImpl(RTLIB::UREM_I64, RTLIB::impl___umoddi3);
  
  // Set the calling convention for these libcall implementations to use C calling convention
  for (RTLIB::LibcallImpl Impl : {RTLIB::impl___divdi3, RTLIB::impl___udivdi3,
                                   RTLIB::impl___moddi3, RTLIB::impl___umoddi3}) {
    setLibcallImplCallingConv(Impl, CallingConv::C);
  }
  // These will use our shift-parts implementation
  setOperationAction(ISD::SHL, MVT::i64, Custom);
  setOperationAction(ISD::SRA, MVT::i64, Custom);
  setOperationAction(ISD::SRL, MVT::i64, Custom);
  setOperationAction(ISD::AND, MVT::i64, Expand);
  setOperationAction(ISD::OR, MVT::i64, Expand);
  setOperationAction(ISD::XOR, MVT::i64, Expand);
  // SELECT is our canonical operation (ChatGPT 5's one-way street approach)
  // SELECT_CC always expands to SELECT + SETCC, never the other way around
  setOperationAction(ISD::SELECT_CC, MVT::i32, Expand);
  setOperationAction(ISD::SELECT_CC, MVT::i64, Expand);
  setOperationAction(ISD::SELECT, MVT::i32, Custom);
  setOperationAction(ISD::SELECT, MVT::i64, Custom);  // Custom for i64 too to avoid loops
  // Note: We DO support SETCC via SLT/SLTU/SEQ/SNE instructions

  // Support for 64-bit operations - use custom lowering for shifts
  // SLOW32 doesn't have carry flags, so we need to use shift-parts approach like RISC-V
  setOperationAction(ISD::SHL_PARTS, MVT::i32, Custom);
  setOperationAction(ISD::SRA_PARTS, MVT::i32, Custom);
  setOperationAction(ISD::SRL_PARTS, MVT::i32, Custom);

  // Set stack pointer - R29 in SLOW32 is the stack pointer
  setStackPointerRegisterToSaveRestore(SLOW32::R29);

  // Note: SLOW32 has no floating point hardware support
  // Floating point operations will trigger soft-float library calls
  // which are not currently implemented. For now, floating point
  // code will fail to compile.

  // Set scheduling preference
  setSchedulingPreference(Sched::Source);

  // We need to custom lower BR_CC since SLOW32 only has BEQ
  setOperationAction(ISD::BR_CC, MVT::i32, Custom);

  // SLOW32 doesn't have a native jump table instruction, expand it
  setOperationAction(ISD::BR_JT, MVT::Other, Expand);

  // SLOW32 doesn't have sign extension instructions for sub-word types
  // Let LLVM expand these to shifts
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i1, Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i8, Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i16, Expand);

  // SLOW32 doesn't have native rotate instructions
  // Expand to (SHL | SRL) or custom lower if needed
  setOperationAction(ISD::ROTL, MVT::i32, Custom);
  setOperationAction(ISD::ROTR, MVT::i32, Custom);

  // SLOW32 doesn't have conditional move, leave SELECT for default expansion

  // Custom lower global addresses and external symbols
  setOperationAction(ISD::GlobalAddress, MVT::i32, Custom);
  setOperationAction(ISD::ExternalSymbol, MVT::i32, Custom);
  setOperationAction(ISD::JumpTable, MVT::i32, Custom);

  // Varargs support - follow RISC-V pattern
  setOperationAction(ISD::VASTART, MVT::Other, Custom);
  // Let LLVM expand VAARG, VACOPY, VAEND
  setOperationAction(ISD::VAARG, MVT::Other, Expand);
  setOperationAction(ISD::VACOPY, MVT::Other, Expand);
  setOperationAction(ISD::VAEND, MVT::Other, Expand);

  // Remove custom lowering for loads/stores - handle in patterns instead
  // setOperationAction(ISD::LOAD, MVT::i32, Custom);
  // setOperationAction(ISD::STORE, MVT::i32, Custom);

  // Configure memcpy/memmove/memset expansion
  // Now that we allow misaligned i32 access, we can safely expand
  // larger memcpy operations using word loads/stores
  MaxStoresPerMemcpy = 8;        // Allow up to 32 bytes inline (8 word stores)
  MaxStoresPerMemcpyOptSize = 4; // More conservative for size optimization (16 bytes)
  MaxStoresPerMemmove = 8;       // Same as memcpy
  MaxStoresPerMemmoveOptSize = 4;
  MaxStoresPerMemset = 16;       // Memset can be more aggressive (64 bytes)
  MaxStoresPerMemsetOptSize = 8;

  // IMPORTANT: Tell LLVM we don't support misaligned memory accesses
  // This forces word-aligned operations and prevents byte-wise expansion issues
  setMinFunctionAlignment(Align(4));
  setPrefFunctionAlignment(Align(4));
  
  // Note: i64 divide/rem operations are set to Expand above
  // This will cause LLVM to generate libcalls to __divdi3, __udivdi3, __moddi3, __umoddi3
  // These need to be provided by compiler-rt or a similar runtime library
}

const char *SLOW32TargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch ((SLOW32ISD::NodeType)Opcode) {
  case SLOW32ISD::FIRST_NUMBER: break;
  case SLOW32ISD::RET_FLAG: return "SLOW32ISD::RET_FLAG";
  case SLOW32ISD::BR_CC: return "SLOW32ISD::BR_CC";
  case SLOW32ISD::BR_NE: return "SLOW32ISD::BR_NE";
  case SLOW32ISD::CALL: return "SLOW32ISD::CALL";
  case SLOW32ISD::HI: return "SLOW32ISD::HI";
  case SLOW32ISD::LO: return "SLOW32ISD::LO";
  }
  return nullptr;
}

SDValue SLOW32TargetLowering::LowerOperation(SDValue Op, SelectionDAG &DAG) const {
  switch (Op.getOpcode()) {
    case ISD::GlobalAddress:  return LowerGlobalAddress(Op, DAG);
    case ISD::ExternalSymbol: return LowerExternalSymbol(Op, DAG);
    case ISD::JumpTable:      return LowerJumpTable(Op, DAG);
    case ISD::VASTART:        return LowerVASTART(Op, DAG);
    case ISD::BRCOND:         return LowerBRCOND(Op, DAG);
    case ISD::BR_CC:          return LowerBR_CC(Op, DAG);
    case ISD::SELECT:         return LowerSELECT(Op, DAG);
    case ISD::ROTL:           return LowerROTL(Op, DAG);
    case ISD::ROTR:           return LowerROTR(Op, DAG);
    case ISD::SHL_PARTS:      return LowerSHL_PARTS(Op, DAG);
    case ISD::SRA_PARTS:      return LowerSRA_PARTS(Op, DAG);
    case ISD::SRL_PARTS:      return LowerSRL_PARTS(Op, DAG);
    case ISD::UADDO:          return LowerUADDO(Op, DAG);
    case ISD::UADDO_CARRY:    return LowerUADDO_CARRY(Op, DAG);
    case ISD::USUBO:          return LowerUSUBO(Op, DAG);
    case ISD::USUBO_CARRY:    return LowerUSUBO_CARRY(Op, DAG);
    case ISD::UMUL_LOHI:      return LowerUMUL_LOHI(Op, DAG);
    case ISD::SMUL_LOHI:      return LowerSMUL_LOHI(Op, DAG);
    case ISD::MULHU:          return LowerMULHU(Op, DAG);
    case ISD::MULHS:          return LowerMULHS(Op, DAG);
    // For i64 shifts, expand to shift-parts
    case ISD::SHL:
    case ISD::SRA:
    case ISD::SRL:
      if (Op.getValueType() == MVT::i64)
        return LowerI64Shift(Op, DAG);
      return SDValue();
    default: return SDValue();
  }
}
SDValue SLOW32TargetLowering::LowerGlobalAddress(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  GlobalAddressSDNode *GA = cast<GlobalAddressSDNode>(Op);
  EVT VT = Op.getValueType();
  const GlobalValue *GV = GA->getGlobal();

  // For functions, just return the target symbol - JAL will handle it directly
  if (isa<Function>(GV)) {
    return DAG.getTargetGlobalAddress(GV, DL, VT, GA->getOffset());
  }

  // For data (global variables), use %hi/%lo addressing with RISC-V style
  // Create target global address nodes with appropriate flags
  SDValue TargetGAHi = DAG.getTargetGlobalAddress(GV, DL, VT,
                                                   GA->getOffset(), SLOW32II::MO_HI);
  SDValue TargetGALo = DAG.getTargetGlobalAddress(GV, DL, VT,
                                                   GA->getOffset(), SLOW32II::MO_LO);

  // Generate: lui rd, %hi(symbol)
  SDValue Hi = DAG.getNode(SLOW32ISD::HI, DL, VT, TargetGAHi);

  // Generate: addi rd, rd, %lo(symbol)
  // Use ADD to get ADDI instruction with sign-extended 12-bit immediate
  // This properly handles the sign extension and bit 11 adjustment
  SDValue Lo = DAG.getNode(SLOW32ISD::LO, DL, VT, TargetGALo);

  // Combine with ADD to generate ADDI (sign-extended LO12)
  return DAG.getNode(ISD::ADD, DL, VT, Hi, Lo);
}

SDValue SLOW32TargetLowering::LowerLOAD(SDValue Op, SelectionDAG &DAG) const {
  LoadSDNode *Load = cast<LoadSDNode>(Op);
  SDValue Addr = Load->getBasePtr();
  SDLoc DL(Op);

  // Check if we're loading from a global address
  if (Addr.getOpcode() == ISD::GlobalAddress ||
      Addr.getOpcode() == ISD::TargetGlobalAddress) {
    // For global addresses, we need to materialize the address first
    // This should be done with LUI + ORI for full 32-bit addresses
    // For now, just let the default handling take over
    return SDValue();
  }

  // Otherwise, use default lowering
  return SDValue();
}

SDValue SLOW32TargetLowering::LowerSTORE(SDValue Op, SelectionDAG &DAG) const {
  StoreSDNode *Store = cast<StoreSDNode>(Op);
  SDValue Addr = Store->getBasePtr();
  SDLoc DL(Op);

  // Check if we're storing to a global address
  if (Addr.getOpcode() == ISD::GlobalAddress ||
      Addr.getOpcode() == ISD::TargetGlobalAddress) {
    // For global addresses, we need to materialize the address first
    // This should be done with LUI + ORI for full 32-bit addresses
    // For now, just let the default handling take over
    return SDValue();
  }

  // Otherwise, use default lowering
  return SDValue();
}
SDValue SLOW32TargetLowering::LowerExternalSymbol(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  ExternalSymbolSDNode *ES = cast<ExternalSymbolSDNode>(Op);
  EVT VT = Op.getValueType();

  const char *Symbol = ES->getSymbol();
  if (!Symbol) {
    // Handle memcpy/memset/memmove intrinsics that don't have library implementations
    // For now, just fail gracefully
    return SDValue();
  }

  // For function calls, just return the target symbol - JAL will handle it directly
  // For data references, we'd use %hi/%lo but that's rare for external symbols
  return DAG.getTargetExternalSymbol(Symbol, VT);
}

SDValue SLOW32TargetLowering::LowerJumpTable(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  JumpTableSDNode *JT = cast<JumpTableSDNode>(Op);
  EVT VT = Op.getValueType();

  // Similar to global addresses, use %hi/%lo addressing with RISC-V style
  SDValue TargetJTHi = DAG.getTargetJumpTable(JT->getIndex(), VT, SLOW32II::MO_HI);
  SDValue TargetJTLo = DAG.getTargetJumpTable(JT->getIndex(), VT, SLOW32II::MO_LO);

  // Generate: lui rd, %hi(jumptable)
  SDValue Hi = DAG.getNode(SLOW32ISD::HI, DL, VT, TargetJTHi);

  // Generate: addi rd, rd, %lo(jumptable)
  SDValue Lo = DAG.getNode(SLOW32ISD::LO, DL, VT, TargetJTLo);

  // Combine with ADD to generate ADDI (sign-extended LO12)
  return DAG.getNode(ISD::ADD, DL, VT, Hi, Lo);
}

SDValue SLOW32TargetLowering::LowerVASTART(SDValue Op, SelectionDAG &DAG) const {
  MachineFunction &MF = DAG.getMachineFunction();
  SLOW32MachineFunctionInfo *FuncInfo = MF.getInfo<SLOW32MachineFunctionInfo>();

  SDLoc DL(Op);
  SDValue FI = DAG.getFrameIndex(FuncInfo->getVarArgsFrameIndex(),
                                 getPointerTy(MF.getDataLayout()));

  // vastart just stores the address of the VarArgsFrameIndex slot into the
  // memory location argument.
  const Value *SV = cast<SrcValueSDNode>(Op.getOperand(2))->getValue();
  return DAG.getStore(Op.getOperand(0), DL, FI, Op.getOperand(1),
                      MachinePointerInfo(SV));
}

SDValue SLOW32TargetLowering::LowerVAARG(SDValue Op, SelectionDAG &DAG) const { return SDValue(); }

SDValue SLOW32TargetLowering::LowerSELECT(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  EVT VT = Op.getValueType();
  SDValue Cond = Op.getOperand(0);
  SDValue TrueV = Op.getOperand(1);
  SDValue FalseV = Op.getOperand(2);

  // SELECT is: Cond ? TrueV : FalseV
  // Use ChatGPT 5's mask trick: result = F ^ ((T ^ F) & mask)
  // where mask = -C (0->0x00000000, 1->0xFFFFFFFF)
  // This avoids creating SELECT_CC nodes and prevents loops

  // Ensure condition is normalized to i32 (0 or 1)
  if (Cond.getValueType() != MVT::i32) {
    // Normalize to i32 boolean (SETCC already produces 0/1)
    Cond = DAG.getNode(ISD::ZERO_EXTEND, DL, MVT::i32, Cond);
  }

  // For i64 SELECT, split into two i32 SELECTs
  if (VT == MVT::i64) {
    // Split i64 operands into hi/lo parts
    SDValue TrueHi, TrueLo, FalseHi, FalseLo;
    std::tie(TrueLo, TrueHi) = DAG.SplitScalar(TrueV, DL, MVT::i32, MVT::i32);
    std::tie(FalseLo, FalseHi) = DAG.SplitScalar(FalseV, DL, MVT::i32, MVT::i32);
    
    // Select each half independently using the same condition
    SDValue ResLo = DAG.getNode(ISD::SELECT, DL, MVT::i32, Cond, TrueLo, FalseLo);
    SDValue ResHi = DAG.getNode(ISD::SELECT, DL, MVT::i32, Cond, TrueHi, FalseHi);
    
    // Combine results
    return DAG.getNode(ISD::BUILD_PAIR, DL, MVT::i64, ResLo, ResHi);
  }

  // For i32, use the mask trick to avoid branches
  // mask = -C (0 -> 0x00000000, 1 -> 0xFFFFFFFF)
  SDValue Zero = DAG.getConstant(0, DL, MVT::i32);
  SDValue Mask = DAG.getNode(ISD::SUB, DL, MVT::i32, Zero, Cond);

  // result = F ^ ((T ^ F) & mask)
  SDValue TF = DAG.getNode(ISD::XOR, DL, VT, TrueV, FalseV);
  SDValue And = DAG.getNode(ISD::AND, DL, VT, TF, Mask);
  return DAG.getNode(ISD::XOR, DL, VT, FalseV, And);
}


// Lower i64 shift operations to shift-parts
SDValue SLOW32TargetLowering::LowerI64Shift(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue Lo, Hi;

  // Split the i64 value into two i32 parts
  std::tie(Lo, Hi) = DAG.SplitScalar(Op.getOperand(0), DL, MVT::i32, MVT::i32);
  SDValue Shamt = Op.getOperand(1);

  // Ensure shift amount is i32
  if (Shamt.getValueType() != MVT::i32) {
    Shamt = DAG.getNode(ISD::TRUNCATE, DL, MVT::i32, Shamt);
  }

  // Call the appropriate shift-parts operation
  unsigned PartsOpc;
  switch (Op.getOpcode()) {
    case ISD::SHL: PartsOpc = ISD::SHL_PARTS; break;
    case ISD::SRA: PartsOpc = ISD::SRA_PARTS; break;
    case ISD::SRL: PartsOpc = ISD::SRL_PARTS; break;
    default: llvm_unreachable("Unexpected shift opcode");
  }

  SDValue Parts = DAG.getNode(PartsOpc, DL, DAG.getVTList(MVT::i32, MVT::i32),
                              Lo, Hi, Shamt);

  // Combine the parts back into i64
  return DAG.getNode(ISD::BUILD_PAIR, DL, MVT::i64,
                     Parts.getValue(0), Parts.getValue(1));
}

// Lower 64-bit shift left into 32-bit operations
// This implements: (Lo, Hi) = SHL_PARTS(Lo, Hi, Shamt)
SDValue SLOW32TargetLowering::LowerSHL_PARTS(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue Lo = Op.getOperand(0);
  SDValue Hi = Op.getOperand(1);
  SDValue Shamt = Op.getOperand(2);
  EVT VT = Lo.getValueType();

  // Handle both cases: shift < 32 and shift >= 32
  SDValue Zero = DAG.getConstant(0, DL, VT);
  SDValue ThirtyOne = DAG.getConstant(31, DL, VT);
  SDValue ThirtyTwo = DAG.getConstant(32, DL, VT);
  
  // Check if shift >= 32
  SDValue IsGE32 = DAG.getSetCC(DL, MVT::i32, Shamt, ThirtyOne, ISD::SETUGT);
  
  // Case 1: shift < 32
  // NewLo = Lo << Shamt
  // NewHi = (Hi << Shamt) | (Lo >> (32 - Shamt))
  SDValue LoShl = DAG.getNode(ISD::SHL, DL, VT, Lo, Shamt);
  SDValue HiShl = DAG.getNode(ISD::SHL, DL, VT, Hi, Shamt);
  SDValue InvShamt = DAG.getNode(ISD::SUB, DL, VT, ThirtyTwo, Shamt);
  SDValue LoShr = DAG.getNode(ISD::SRL, DL, VT, Lo, InvShamt);
  SDValue HiLess32 = DAG.getNode(ISD::OR, DL, VT, HiShl, LoShr);
  
  // Case 2: shift >= 32
  // NewLo = 0
  // NewHi = Lo << (Shamt - 32)
  SDValue ShamtMinus32 = DAG.getNode(ISD::SUB, DL, VT, Shamt, ThirtyTwo);
  SDValue HiGE32 = DAG.getNode(ISD::SHL, DL, VT, Lo, ShamtMinus32);
  
  // Select based on shift amount
  SDValue NewLo = DAG.getNode(ISD::SELECT, DL, VT, IsGE32, Zero, LoShl);
  SDValue NewHi = DAG.getNode(ISD::SELECT, DL, VT, IsGE32, HiGE32, HiLess32);

  return DAG.getMergeValues({NewLo, NewHi}, DL);
}

// Lower 64-bit logical shift right into 32-bit operations
SDValue SLOW32TargetLowering::LowerSRL_PARTS(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue Lo = Op.getOperand(0);
  SDValue Hi = Op.getOperand(1);
  SDValue Shamt = Op.getOperand(2);
  EVT VT = Lo.getValueType();

  // Handle both cases: shift < 32 and shift >= 32
  SDValue Zero = DAG.getConstant(0, DL, VT);
  SDValue ThirtyOne = DAG.getConstant(31, DL, VT);
  SDValue ThirtyTwo = DAG.getConstant(32, DL, VT);
  
  // Check if shift >= 32
  SDValue IsGE32 = DAG.getSetCC(DL, MVT::i32, Shamt, ThirtyOne, ISD::SETUGT);
  
  // Case 1: shift < 32
  // NewLo = (Lo >> Shamt) | (Hi << (32 - Shamt))
  // NewHi = Hi >> Shamt
  SDValue LoShr = DAG.getNode(ISD::SRL, DL, VT, Lo, Shamt);
  SDValue HiShr = DAG.getNode(ISD::SRL, DL, VT, Hi, Shamt);
  SDValue InvShamt = DAG.getNode(ISD::SUB, DL, VT, ThirtyTwo, Shamt);
  SDValue HiShl = DAG.getNode(ISD::SHL, DL, VT, Hi, InvShamt);
  SDValue LoLess32 = DAG.getNode(ISD::OR, DL, VT, LoShr, HiShl);
  
  // Case 2: shift >= 32
  // NewLo = Hi >> (Shamt - 32)
  // NewHi = 0
  SDValue ShamtMinus32 = DAG.getNode(ISD::SUB, DL, VT, Shamt, ThirtyTwo);
  SDValue LoGE32 = DAG.getNode(ISD::SRL, DL, VT, Hi, ShamtMinus32);
  
  // Select based on shift amount
  SDValue NewLo = DAG.getNode(ISD::SELECT, DL, VT, IsGE32, LoGE32, LoLess32);
  SDValue NewHi = DAG.getNode(ISD::SELECT, DL, VT, IsGE32, Zero, HiShr);

  return DAG.getMergeValues({NewLo, NewHi}, DL);
}

// Lower 64-bit arithmetic shift right into 32-bit operations
SDValue SLOW32TargetLowering::LowerSRA_PARTS(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue Lo = Op.getOperand(0);
  SDValue Hi = Op.getOperand(1);
  SDValue Shamt = Op.getOperand(2);
  EVT VT = Lo.getValueType();

  // Handle both cases: shift < 32 and shift >= 32
  SDValue ThirtyOne = DAG.getConstant(31, DL, VT);
  SDValue ThirtyTwo = DAG.getConstant(32, DL, VT);
  
  // Check if shift >= 32
  SDValue IsGE32 = DAG.getSetCC(DL, MVT::i32, Shamt, ThirtyOne, ISD::SETUGT);
  
  // Case 1: shift < 32
  // NewLo = (Lo >> Shamt) | (Hi << (32 - Shamt))
  // NewHi = Hi >> Shamt (arithmetic)
  SDValue LoShr = DAG.getNode(ISD::SRL, DL, VT, Lo, Shamt);
  SDValue HiSra = DAG.getNode(ISD::SRA, DL, VT, Hi, Shamt);
  SDValue InvShamt = DAG.getNode(ISD::SUB, DL, VT, ThirtyTwo, Shamt);
  SDValue HiShl = DAG.getNode(ISD::SHL, DL, VT, Hi, InvShamt);
  SDValue LoLess32 = DAG.getNode(ISD::OR, DL, VT, LoShr, HiShl);
  
  // Case 2: shift >= 32
  // NewLo = Hi >> (Shamt - 32) (arithmetic)
  // NewHi = Hi >> 31 (all sign bits)
  SDValue ShamtMinus32 = DAG.getNode(ISD::SUB, DL, VT, Shamt, ThirtyTwo);
  SDValue LoGE32 = DAG.getNode(ISD::SRA, DL, VT, Hi, ShamtMinus32);
  SDValue HiGE32 = DAG.getNode(ISD::SRA, DL, VT, Hi, ThirtyOne);
  
  // Select based on shift amount
  SDValue NewLo = DAG.getNode(ISD::SELECT, DL, VT, IsGE32, LoGE32, LoLess32);
  SDValue NewHi = DAG.getNode(ISD::SELECT, DL, VT, IsGE32, HiGE32, HiSra);

  return DAG.getMergeValues({NewLo, NewHi}, DL);
}

SDValue SLOW32TargetLowering::LowerBRCOND(SDValue Op, SelectionDAG &DAG) const {
  SDValue Chain = Op.getOperand(0);
  SDValue Cond = Op.getOperand(1);
  SDValue Dest = Op.getOperand(2);
  SDLoc DL(Op);

  // BRCOND branches when Cond is TRUE (non-zero)
  // We need to branch to Dest when Cond != 0

  // Use BNE to branch when Cond != 0
  SDValue Zero = DAG.getConstant(0, DL, MVT::i32);

  // Use BR_NE: bne Cond, r0, Dest
  return DAG.getNode(SLOW32ISD::BR_NE, DL, MVT::Other,
                     Chain, Cond, Zero, Dest);
}

SDValue SLOW32TargetLowering::LowerBR_CC(SDValue Op, SelectionDAG &DAG) const {
  SDValue Chain = Op.getOperand(0);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(1))->get();
  SDValue LHS = Op.getOperand(2);
  SDValue RHS = Op.getOperand(3);
  SDValue Dest = Op.getOperand(4);
  SDLoc DL(Op);

  // For SETEQ and SETNE, we can directly use BEQ and BNE instructions
  if (CC == ISD::SETEQ) {
    // Direct BEQ: beq LHS, RHS, Dest
    return DAG.getNode(SLOW32ISD::BR_CC, DL, MVT::Other,
                       Chain, LHS, RHS, Dest);
  } else if (CC == ISD::SETNE) {
    // Direct BNE: bne LHS, RHS, Dest
    return DAG.getNode(SLOW32ISD::BR_NE, DL, MVT::Other,
                       Chain, LHS, RHS, Dest);
  }

  // First, generate the comparison for other conditions
  SDValue Cmp;
  switch (CC) {
  case ISD::SETLT:
  case ISD::SETGE:
    // Use SLT directly
    Cmp = DAG.getNode(ISD::SETCC, DL, MVT::i32, LHS, RHS,
                      DAG.getCondCode(ISD::SETLT));
    break;
  case ISD::SETGT:
  case ISD::SETLE:
    // Swap operands and use SLT
    Cmp = DAG.getNode(ISD::SETCC, DL, MVT::i32, RHS, LHS,
                      DAG.getCondCode(ISD::SETLT));
    break;
  default:
    // For unsigned comparisons, we'd need more work
    // For now, just use a simple comparison
    Cmp = DAG.getNode(ISD::SETCC, DL, MVT::i32, LHS, RHS,
                      DAG.getCondCode(CC));
    break;
  }

  // Now handle the branch logic for other conditions
  SDValue Zero = DAG.getConstant(0, DL, MVT::i32);

  // For SETLT and SETGT, the SLT result is 1 if true, 0 if false
  // So we branch when Cmp != 0
  if (CC == ISD::SETLT || CC == ISD::SETGT) {
    return DAG.getNode(SLOW32ISD::BR_NE, DL, MVT::Other,
                       Chain, Cmp, Zero, Dest);
  }
  // For SETGE and SETLE, we inverted the condition
  // So we branch when Cmp == 0
  else if (CC == ISD::SETGE || CC == ISD::SETLE) {
    return DAG.getNode(SLOW32ISD::BR_CC, DL, MVT::Other,
                       Chain, Cmp, Zero, Dest);
  }
  // Default case for other conditions
  else {
    // TODO: Implement proper lowering for unsigned conditions
    return DAG.getNode(SLOW32ISD::BR_NE, DL, MVT::Other,
                       Chain, Cmp, Zero, Dest);
  }
}

SDValue SLOW32TargetLowering::LowerFormalArguments(
    SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &dl,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {

  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  MachineRegisterInfo &RegInfo = MF.getRegInfo();
  SLOW32MachineFunctionInfo *FuncInfo = MF.getInfo<SLOW32MachineFunctionInfo>();

  // Arguments are passed in r3-r10 (a0-a7)
  static const unsigned ArgRegs[] = {
    SLOW32::R3, SLOW32::R4, SLOW32::R5, SLOW32::R6,
    SLOW32::R7, SLOW32::R8, SLOW32::R9, SLOW32::R10
  };
  unsigned NumArgRegs = sizeof(ArgRegs) / sizeof(ArgRegs[0]);

  // Track how many argument registers we've used
  unsigned UsedArgRegs = 0;

  // Process arguments
  // Stack arguments are above the frame pointer, pushed by caller
  // Layout: [arg10][arg9][saved lr][saved fp] <- FP points here
  // So first stack arg (arg9) is at FP+8, next at FP+12, etc.
  unsigned StackOffset = 8; // Skip saved FP and LR
  SmallVector<SDValue, 8> OutChains;

  for (unsigned i = 0; i < Ins.size(); ++i) {
    if (i < NumArgRegs) {
      // Argument passed in register
      unsigned VReg = RegInfo.createVirtualRegister(&SLOW32::GPRRegClass);
      RegInfo.addLiveIn(ArgRegs[i], VReg);
      SDValue ArgValue = DAG.getCopyFromReg(Chain, dl, VReg, MVT::i32);
      InVals.push_back(ArgValue);
      UsedArgRegs++;
    } else {
      // Arguments beyond the first 8 are on the stack
      // They are pushed by the caller before the call
      int FI = MFI.CreateFixedObject(4, StackOffset, true);
      SDValue FIN = DAG.getFrameIndex(FI, MVT::i32);
      SDValue Load = DAG.getLoad(MVT::i32, dl, Chain, FIN,
                                 MachinePointerInfo::getFixedStack(MF, FI));
      InVals.push_back(Load);
      StackOffset += 4;
    }
  }

  // Handle varargs
  if (isVarArg) {
    // Save any unused argument registers to the varargs save area
    int VarArgsSaveSize = 4 * (NumArgRegs - UsedArgRegs);
    int VarArgsFrameIndex;

    if (VarArgsSaveSize == 0) {
      // All registers were used, varargs start on stack
      VarArgsFrameIndex = MFI.CreateFixedObject(4, StackOffset, true);
    } else {
      // Create save area for unused argument registers
      VarArgsFrameIndex = MFI.CreateFixedObject(VarArgsSaveSize, -VarArgsSaveSize, true);

      // Save the unused argument registers
      // IMPORTANT: Save them in sequential memory order for va_arg to work
      SDValue FIN = DAG.getFrameIndex(VarArgsFrameIndex, MVT::i32);

      // Hardcode the stores to ensure correct order (debugging the reordering issue)
      // For sum(count, ...) with count in r3, we need to save r4-r10
      if (UsedArgRegs == 1) {
        // Save r4 at offset 0
        unsigned VReg4 = RegInfo.createVirtualRegister(&SLOW32::GPRRegClass);
        RegInfo.addLiveIn(SLOW32::R4, VReg4);
        SDValue Val4 = DAG.getCopyFromReg(Chain, dl, VReg4, MVT::i32);
        Chain = DAG.getStore(Chain, dl, Val4, FIN, MachinePointerInfo());

        // Save r5 at offset 4
        unsigned VReg5 = RegInfo.createVirtualRegister(&SLOW32::GPRRegClass);
        RegInfo.addLiveIn(SLOW32::R5, VReg5);
        SDValue Val5 = DAG.getCopyFromReg(Chain, dl, VReg5, MVT::i32);
        SDValue Addr5 = DAG.getNode(ISD::ADD, dl, MVT::i32, FIN,
                                    DAG.getConstant(4, dl, MVT::i32));
        Chain = DAG.getStore(Chain, dl, Val5, Addr5, MachinePointerInfo());

        // Save r6 at offset 8
        unsigned VReg6 = RegInfo.createVirtualRegister(&SLOW32::GPRRegClass);
        RegInfo.addLiveIn(SLOW32::R6, VReg6);
        SDValue Val6 = DAG.getCopyFromReg(Chain, dl, VReg6, MVT::i32);
        SDValue Addr6 = DAG.getNode(ISD::ADD, dl, MVT::i32, FIN,
                                    DAG.getConstant(8, dl, MVT::i32));
        Chain = DAG.getStore(Chain, dl, Val6, Addr6, MachinePointerInfo());

        // Save r7-r10 similarly
        unsigned VReg7 = RegInfo.createVirtualRegister(&SLOW32::GPRRegClass);
        RegInfo.addLiveIn(SLOW32::R7, VReg7);
        SDValue Val7 = DAG.getCopyFromReg(Chain, dl, VReg7, MVT::i32);
        SDValue Addr7 = DAG.getNode(ISD::ADD, dl, MVT::i32, FIN,
                                    DAG.getConstant(12, dl, MVT::i32));
        Chain = DAG.getStore(Chain, dl, Val7, Addr7, MachinePointerInfo());

        unsigned VReg8 = RegInfo.createVirtualRegister(&SLOW32::GPRRegClass);
        RegInfo.addLiveIn(SLOW32::R8, VReg8);
        SDValue Val8 = DAG.getCopyFromReg(Chain, dl, VReg8, MVT::i32);
        SDValue Addr8 = DAG.getNode(ISD::ADD, dl, MVT::i32, FIN,
                                    DAG.getConstant(16, dl, MVT::i32));
        Chain = DAG.getStore(Chain, dl, Val8, Addr8, MachinePointerInfo());

        unsigned VReg9 = RegInfo.createVirtualRegister(&SLOW32::GPRRegClass);
        RegInfo.addLiveIn(SLOW32::R9, VReg9);
        SDValue Val9 = DAG.getCopyFromReg(Chain, dl, VReg9, MVT::i32);
        SDValue Addr9 = DAG.getNode(ISD::ADD, dl, MVT::i32, FIN,
                                    DAG.getConstant(20, dl, MVT::i32));
        Chain = DAG.getStore(Chain, dl, Val9, Addr9, MachinePointerInfo());

        unsigned VReg10 = RegInfo.createVirtualRegister(&SLOW32::GPRRegClass);
        RegInfo.addLiveIn(SLOW32::R10, VReg10);
        SDValue Val10 = DAG.getCopyFromReg(Chain, dl, VReg10, MVT::i32);
        SDValue Addr10 = DAG.getNode(ISD::ADD, dl, MVT::i32, FIN,
                                     DAG.getConstant(24, dl, MVT::i32));
        Chain = DAG.getStore(Chain, dl, Val10, Addr10, MachinePointerInfo());
      } else {
        // Generic loop for other cases - properly chain everything
        for (unsigned i = UsedArgRegs; i < NumArgRegs; ++i) {
          unsigned VReg = RegInfo.createVirtualRegister(&SLOW32::GPRRegClass);
          RegInfo.addLiveIn(ArgRegs[i], VReg);

          // Get value from register
          SDValue ArgValue = DAG.getCopyFromReg(Chain, dl, VReg, MVT::i32);

          // Calculate address
          int Offset = (i - UsedArgRegs) * 4;
          SDValue Addr = Offset == 0 ? FIN :
                         DAG.getNode(ISD::ADD, dl, MVT::i32, FIN,
                                    DAG.getConstant(Offset, dl, MVT::i32));

          // Store and update chain
          Chain = DAG.getStore(Chain, dl, ArgValue, Addr, MachinePointerInfo());
        }
      }
    }

    // Record the varargs frame index
    FuncInfo->setVarArgsFrameIndex(VarArgsFrameIndex);
    FuncInfo->setVarArgsSaveSize(VarArgsSaveSize);
  }

  return Chain;
}

SDValue SLOW32TargetLowering::LowerReturn(SDValue Chain, CallingConv::ID CC,
                                          bool IsVarArg,
                                          const SmallVectorImpl<ISD::OutputArg> &Outs,
                                          const SmallVectorImpl<SDValue> &OutVals,
                                          const SDLoc &DL, SelectionDAG &DAG) const {
  SDValue Glue;

  if (!Outs.empty()) {
    // Check if we're returning an i64 value
    if (Outs.size() == 1 && Outs[0].VT == MVT::i64) {
      // i64 return value: split into R1 (low) and R2 (high)
      // The value should already be split by type legalization
      if (OutVals.size() == 2) {
        // Already split into two i32 values
        Chain = DAG.getCopyToReg(Chain, DL, SLOW32::R1, OutVals[0], SDValue());
        Chain = DAG.getCopyToReg(Chain, DL, SLOW32::R2, OutVals[1], Chain.getValue(1));
        Glue = Chain.getValue(1);
      } else if (OutVals.size() == 1 && OutVals[0].getValueType() == MVT::i64) {
        // Still i64, need to extract parts
        SDValue Lo = DAG.getNode(ISD::EXTRACT_ELEMENT, DL, MVT::i32, OutVals[0],
                                DAG.getConstant(0, DL, MVT::i32));
        SDValue Hi = DAG.getNode(ISD::EXTRACT_ELEMENT, DL, MVT::i32, OutVals[0],
                                DAG.getConstant(1, DL, MVT::i32));
        Chain = DAG.getCopyToReg(Chain, DL, SLOW32::R1, Lo, SDValue());
        Chain = DAG.getCopyToReg(Chain, DL, SLOW32::R2, Hi, Chain.getValue(1));
        Glue = Chain.getValue(1);
      } else {
        // Single i32 value
        Chain = DAG.getCopyToReg(Chain, DL, SLOW32::R1, OutVals[0], SDValue());
        Glue = Chain.getValue(1);
      }
    } else if (Outs.size() == 2 && Outs[0].VT == MVT::i32 && Outs[1].VT == MVT::i32) {
      // Two i32 values (from split i64)
      Chain = DAG.getCopyToReg(Chain, DL, SLOW32::R1, OutVals[0], SDValue());
      Chain = DAG.getCopyToReg(Chain, DL, SLOW32::R2, OutVals[1], Chain.getValue(1));
      Glue = Chain.getValue(1);
    } else {
      // Single i32 return value
      Chain = DAG.getCopyToReg(Chain, DL, SLOW32::R1, OutVals[0], SDValue());
      Glue = Chain.getValue(1);
    }
  } else {
    // Void function: define R1 with undef so RET's Uses=[R1] is satisfied
    // This prevents "undefined register" errors
    Chain = DAG.getCopyToReg(Chain, DL, SLOW32::R1, DAG.getUNDEF(MVT::i32), SDValue());
    Glue = Chain.getValue(1);
  }

  // Always use glue to connect CopyToReg and RET
  return DAG.getNode(SLOW32ISD::RET_FLAG, DL, MVT::Other, Chain, Glue);
}

SDValue SLOW32TargetLowering::LowerCall(TargetLowering::CallLoweringInfo &CLI,
                                        SmallVectorImpl<SDValue> &InVals) const {
  SelectionDAG &DAG = CLI.DAG;
  SDLoc &dl = CLI.DL;
  SmallVectorImpl<SDValue> &OutVals = CLI.OutVals;
  SmallVectorImpl<ISD::InputArg> &Ins = CLI.Ins;
  SDValue Chain = CLI.Chain;
  SDValue Callee = CLI.Callee;
  CallingConv::ID CallConv = CLI.CallConv;

  // For now, implement a basic call that:
  // 1. Passes arguments in r3-r10
  // 2. Calls using JAL
  // 3. Returns values in r1

  // Calculate stack space needed for arguments beyond the first 8
  unsigned NumStackArgs = OutVals.size() > 8 ? OutVals.size() - 8 : 0;
  unsigned StackSize = NumStackArgs * 4;

  // Use CALLSEQ_START to mark the beginning of a call sequence
  // This will be lowered to stack adjustment by the frame lowering code
  Chain = DAG.getCALLSEQ_START(Chain, StackSize, 0, dl);

  // Get stack pointer for storing arguments
  SDValue StackPtr = DAG.getRegister(SLOW32::SP, MVT::i32);

  // Push stack arguments (arguments beyond the first 8)
  for (unsigned i = 8; i < OutVals.size(); ++i) {
    unsigned Offset = (i - 8) * 4;
    SDValue Addr = DAG.getNode(ISD::ADD, dl, MVT::i32, StackPtr,
                                DAG.getConstant(Offset, dl, MVT::i32));
    Chain = DAG.getStore(Chain, dl, OutVals[i], Addr,
                          MachinePointerInfo());
  }

  // Pass register arguments (r3-r10) with glue chain
  SDValue Glue;
  for (unsigned i = 0; i < OutVals.size() && i < 8; ++i) {
    unsigned ArgReg = SLOW32::R3 + i;  // r3-r10 for arguments
    Chain = DAG.getCopyToReg(Chain, dl, ArgReg, OutVals[i], Glue);
    Glue = Chain.getValue(1);
  }

  // Create the call
  SmallVector<SDValue, 8> Ops;
  Ops.push_back(Chain);

  // Check if Callee is an external symbol with empty name (memcpy/memset intrinsic)
  if (ExternalSymbolSDNode *ES = dyn_cast<ExternalSymbolSDNode>(Callee)) {
    const char *Symbol = ES->getSymbol();
    if (!Symbol || Symbol[0] == '\0') {
      // Empty symbol - this is likely a memcpy/memset intrinsic
      // We need to provide a real symbol name
      // For now, assume memcpy - ideally we'd check the call signature
      Callee = DAG.getTargetExternalSymbol("memcpy", MVT::i32);
    } else {
      // Convert to target external symbol
      Callee = DAG.getTargetExternalSymbol(Symbol, MVT::i32);
    }
  } else if (GlobalAddressSDNode *GA = dyn_cast<GlobalAddressSDNode>(Callee)) {
    // Convert to target global address
    Callee = DAG.getTargetGlobalAddress(GA->getGlobal(), dl, MVT::i32,
                                         GA->getOffset(), GA->getTargetFlags());
  }

  Ops.push_back(Callee);

  // Add register mask operand
  const TargetRegisterInfo *TRI = DAG.getSubtarget().getRegisterInfo();
  const uint32_t *Mask = TRI->getCallPreservedMask(DAG.getMachineFunction(), CallConv);
  Ops.push_back(DAG.getRegisterMask(Mask));

  // Add glue if we have one (from CopyToReg)
  if (Glue.getNode())
    Ops.push_back(Glue);

  // Create the call node
  SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
  Chain = DAG.getNode(SLOW32ISD::CALL, dl, NodeTys, Ops);
  Glue = Chain.getValue(1);

  // Use CALLSEQ_END to mark the end of the call sequence
  // This will be lowered to stack adjustment by the frame lowering code
  Chain = DAG.getCALLSEQ_END(Chain, StackSize, 0, Glue, dl);
  Glue = Chain.getValue(1);

  // Handle incoming return values
  for (const ISD::InputArg &In : Ins) {
    if (InVals.size() == 0) {
      // First return value comes from r1
      SDValue RetValue = DAG.getCopyFromReg(Chain, dl, SLOW32::R1, In.VT, Glue);
      InVals.push_back(RetValue);
      Chain = RetValue.getValue(1);
      Glue = RetValue.getValue(2);
    } else {
      // Additional return values not supported yet
      InVals.push_back(DAG.getUNDEF(In.VT));
    }
  }

  return Chain;
}

EVT SLOW32TargetLowering::getOptimalMemOpType(
    LLVMContext &Context, const MemOp &Op,
    const AttributeList &FuncAttributes) const {
  // SLOW32 only has efficient 32-bit operations
  // Using i32 for memcpy/memset will generate clean word-sized loads/stores
  return MVT::i32;
}

bool SLOW32TargetLowering::isLegalAddressingMode(const DataLayout &DL,
                                                 const AddrMode &AM,
                                                 Type *Ty, unsigned AS,
                                                 Instruction *I) const {
  // SLOW32 supports [base + simm16] addressing mode
  // No scaled index, no complex addressing

  // Check if offset fits in 16-bit signed immediate
  if (AM.BaseOffs < -32768 || AM.BaseOffs > 32767)
    return false;

  // No scaled indexing
  if (AM.Scale != 0)
    return false;

  // Base register is always allowed
  // Global address is allowed (will be lowered with %hi/%lo)
  return true;
}

SDValue SLOW32TargetLowering::LowerROTL(SDValue Op, SelectionDAG &DAG) const {
  // Rotate left: ROTL(x, n) = (x << n) | (x >> (32 - n))
  SDLoc DL(Op);
  SDValue Val = Op.getOperand(0);
  SDValue Amt = Op.getOperand(1);
  EVT VT = Op.getValueType();

  // Handle constant rotation amounts
  if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Amt)) {
    unsigned RotAmt = C->getZExtValue() & 31;  // Mask to 5 bits
    if (RotAmt == 0)
      return Val;

    SDValue ShlAmt = DAG.getConstant(RotAmt, DL, VT);
    SDValue ShrAmt = DAG.getConstant(32 - RotAmt, DL, VT);

    SDValue Shl = DAG.getNode(ISD::SHL, DL, VT, Val, ShlAmt);
    SDValue Shr = DAG.getNode(ISD::SRL, DL, VT, Val, ShrAmt);
    return DAG.getNode(ISD::OR, DL, VT, Shl, Shr);
  }

  // For variable rotation amounts
  SDValue ThirtyTwo = DAG.getConstant(32, DL, VT);
  SDValue NegAmt = DAG.getNode(ISD::SUB, DL, VT, ThirtyTwo, Amt);

  SDValue Shl = DAG.getNode(ISD::SHL, DL, VT, Val, Amt);
  SDValue Shr = DAG.getNode(ISD::SRL, DL, VT, Val, NegAmt);
  return DAG.getNode(ISD::OR, DL, VT, Shl, Shr);
}

SDValue SLOW32TargetLowering::LowerConstant(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  ConstantSDNode *CN = cast<ConstantSDNode>(Op);
  EVT VT = Op.getValueType();
  int64_t Val = CN->getSExtValue();

  // For small constants, just return the constant node
  if (Val >= -32768 && Val <= 65535) {
    return Op;  // Let normal patterns handle it
  }

  // For large constants, we need LUI + ORI
  // Split into high and low parts
  int32_t Hi = (Val >> 16) & 0xFFFF;
  int32_t Lo = Val & 0xFFFF;

  // LUI loads upper 16 bits
  SDValue HiVal = DAG.getConstant(Hi, DL, VT);
  SDValue LUI = DAG.getNode(ISD::SHL, DL, VT, HiVal,
                            DAG.getConstant(16, DL, VT));

  // If low part is zero, we're done
  if (Lo == 0) {
    return LUI;
  }

  // Otherwise OR in the low bits
  SDValue LoVal = DAG.getConstant(Lo, DL, VT);
  return DAG.getNode(ISD::OR, DL, VT, LUI, LoVal);
}

SDValue SLOW32TargetLowering::LowerROTR(SDValue Op, SelectionDAG &DAG) const {
  // Rotate right: ROTR(x, n) = (x >> n) | (x << (32 - n))
  SDLoc DL(Op);
  SDValue Val = Op.getOperand(0);
  SDValue Amt = Op.getOperand(1);
  EVT VT = Op.getValueType();

  // Handle constant rotation amounts
  if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Amt)) {
    unsigned RotAmt = C->getZExtValue() & 31;  // Mask to 5 bits
    if (RotAmt == 0)
      return Val;

    SDValue ShrAmt = DAG.getConstant(RotAmt, DL, VT);
    SDValue ShlAmt = DAG.getConstant(32 - RotAmt, DL, VT);

    SDValue Shr = DAG.getNode(ISD::SRL, DL, VT, Val, ShrAmt);
    SDValue Shl = DAG.getNode(ISD::SHL, DL, VT, Val, ShlAmt);
    return DAG.getNode(ISD::OR, DL, VT, Shr, Shl);
  }

  // For variable rotation amounts
  SDValue ThirtyTwo = DAG.getConstant(32, DL, VT);
  SDValue NegAmt = DAG.getNode(ISD::SUB, DL, VT, ThirtyTwo, Amt);

  SDValue Shr = DAG.getNode(ISD::SRL, DL, VT, Val, Amt);
  SDValue Shl = DAG.getNode(ISD::SHL, DL, VT, Val, NegAmt);
  return DAG.getNode(ISD::OR, DL, VT, Shr, Shl);
}

void SLOW32TargetLowering::ReplaceNodeResults(SDNode *N,
                                              SmallVectorImpl<SDValue> &Results,
                                              SelectionDAG &DAG) const {
  SDLoc DL(N);

  switch (N->getOpcode()) {
    case ISD::SHL:
    case ISD::SRA:
    case ISD::SRL:
      // Handle i64 shifts
      if (N->getValueType(0) == MVT::i64) {
        SDValue Result = LowerI64Shift(SDValue(N, 0), DAG);
        Results.push_back(Result);
        return;
      }
      break;
    default:
      break;
  }
}

MachineBasicBlock *
SLOW32TargetLowering::EmitInstrWithCustomInserter(MachineInstr &MI,
                                                  MachineBasicBlock *BB) const {
  const TargetInstrInfo &TII = *BB->getParent()->getSubtarget().getInstrInfo();
  DebugLoc DL = MI.getDebugLoc();

  switch (MI.getOpcode()) {
  case SLOW32::SELECT_PSEUDO: {
    // SELECT_PSEUDO expands to:
    //   beq $cond, r0, FalseBB
    // TrueBB:
    //   add $dst, $t, r0
    //   jal r0, DoneBB
    // FalseBB:
    //   add $dst, $f, r0
    // DoneBB:

    MachineFunction *MF = BB->getParent();
    const BasicBlock *LLVM_BB = BB->getBasicBlock();
    MachineBasicBlock *TrueBB = MF->CreateMachineBasicBlock(LLVM_BB);
    MachineBasicBlock *FalseBB = MF->CreateMachineBasicBlock(LLVM_BB);
    MachineBasicBlock *DoneBB = MF->CreateMachineBasicBlock(LLVM_BB);

    MachineFunction::iterator It = ++BB->getIterator();
    MF->insert(It, TrueBB);
    MF->insert(It, FalseBB);
    MF->insert(It, DoneBB);

    // Transfer successors
    DoneBB->splice(DoneBB->begin(), BB,
                   std::next(MachineBasicBlock::iterator(MI)), BB->end());
    DoneBB->transferSuccessorsAndUpdatePHIs(BB);

    // Set up branches
    BB->addSuccessor(TrueBB);
    BB->addSuccessor(FalseBB);
    TrueBB->addSuccessor(DoneBB);
    FalseBB->addSuccessor(DoneBB);

    // BB: beq $cond, r0, FalseBB
    BuildMI(BB, DL, TII.get(SLOW32::BEQ))
        .addReg(MI.getOperand(1).getReg())  // $cond
        .addReg(SLOW32::R0)
        .addMBB(FalseBB);

    // TrueBB: add $dst, $t, r0
    BuildMI(TrueBB, DL, TII.get(SLOW32::ADD), MI.getOperand(0).getReg())
        .addReg(MI.getOperand(2).getReg())  // $t
        .addReg(SLOW32::R0);
    BuildMI(TrueBB, DL, TII.get(SLOW32::JAL))
        .addReg(SLOW32::R0)
        .addMBB(DoneBB);

    // FalseBB: add $dst, $f, r0
    BuildMI(FalseBB, DL, TII.get(SLOW32::ADD), MI.getOperand(0).getReg())
        .addReg(MI.getOperand(3).getReg())  // $f
        .addReg(SLOW32::R0);

    MI.eraseFromParent();
    return DoneBB;
  }
  default:
    llvm_unreachable("Unexpected custom inserter!");
  }
}

// Tell LLVM about our misaligned memory access support
// CRITICAL: This function has a major impact on memcpy expansion!
bool SLOW32TargetLowering::allowsMisalignedMemoryAccesses(
    EVT VT, unsigned AddrSpace, Align Alignment,
    MachineMemOperand::Flags Flags, unsigned *Fast) const {
  // SLOW32 can do unaligned loads/stores, but they're slower
  // However, allowing them prevents the catastrophic register
  // allocation bug in byte-wise memcpy expansion
  if (Fast)
    *Fast = 0;  // Unaligned is slower but works

  // Allow unaligned access for i32 to prevent byte-wise expansion
  if (VT == MVT::i32)
    return true;

  return false;
}

// Lower ADDC - Add with Carry output
// ADDC produces two results: sum and carry
// Since SLOW32 has no carry flag, compute carry by: carry = (sum < a) for unsigned
SDValue SLOW32TargetLowering::LowerADDC(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  EVT VT = Op.getValueType();

  // Compute the sum
  SDValue Sum = DAG.getNode(ISD::ADD, DL, VT, LHS, RHS);

  // Compute carry: carry = (sum < lhs) - unsigned overflow occurred if sum < either input
  SDValue Carry = DAG.getSetCC(DL, MVT::i1, Sum, LHS, ISD::SETULT);

  // Return both sum and carry (carry as i32 since that's what ADDC expects)
  SDValue CarryOut = DAG.getZExtOrTrunc(Carry, DL, VT);
  return DAG.getMergeValues({Sum, CarryOut}, DL);
}

// Lower ADDE - Add with Carry input
// ADDE adds two values plus a carry input and produces sum and carry output
SDValue SLOW32TargetLowering::LowerADDE(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  SDValue CarryIn = Op.getOperand(2);
  EVT VT = Op.getValueType();

  // First add LHS + RHS
  SDValue Sum1 = DAG.getNode(ISD::ADD, DL, VT, LHS, RHS);
  // Compute first carry
  SDValue Carry1 = DAG.getSetCC(DL, MVT::i1, Sum1, LHS, ISD::SETULT);

  // Add the carry input (ensure it's the right type)
  // CarryIn is usually i32 from previous ADDC/SUBC
  SDValue CarryInExt;
  if (CarryIn.getValueType().isInteger()) {
    CarryInExt = DAG.getZExtOrTrunc(CarryIn, DL, VT);
  } else {
    // This shouldn't happen - ADDE expects integer carry
    assert(false && "ADDE: CarryIn must be an integer type");
    CarryInExt = DAG.getConstant(0, DL, VT);
  }
  SDValue Sum2 = DAG.getNode(ISD::ADD, DL, VT, Sum1, CarryInExt);
  // Compute second carry
  SDValue Carry2 = DAG.getSetCC(DL, MVT::i1, Sum2, Sum1, ISD::SETULT);

  // Final carry is OR of both carries (can't have both)
  SDValue CarryBit = DAG.getNode(ISD::OR, DL, MVT::i1, Carry1, Carry2);
  SDValue CarryOut = DAG.getZExtOrTrunc(CarryBit, DL, VT);

  // Return both sum and carry
  return DAG.getMergeValues({Sum2, CarryOut}, DL);
}

// Lower SUBC - Subtract with Borrow output
// SUBC produces two results: difference and borrow
// Borrow = (lhs < rhs) for unsigned
SDValue SLOW32TargetLowering::LowerSUBC(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  EVT VT = Op.getValueType();

  // Compute the difference
  SDValue Diff = DAG.getNode(ISD::SUB, DL, VT, LHS, RHS);

  // Compute borrow: borrow = (lhs < rhs)
  SDValue Borrow = DAG.getSetCC(DL, MVT::i1, LHS, RHS, ISD::SETULT);
  SDValue BorrowOut = DAG.getZExtOrTrunc(Borrow, DL, VT);

  // Return both difference and borrow
  return DAG.getMergeValues({Diff, BorrowOut}, DL);
}

// Lower SUBE - Subtract with Borrow input
// SUBE subtracts RHS and borrow from LHS
SDValue SLOW32TargetLowering::LowerSUBE(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  SDValue BorrowIn = Op.getOperand(2);
  EVT VT = Op.getValueType();

  // First subtract LHS - RHS
  SDValue Diff1 = DAG.getNode(ISD::SUB, DL, VT, LHS, RHS);
  // Compute first borrow
  SDValue Borrow1 = DAG.getSetCC(DL, MVT::i1, LHS, RHS, ISD::SETULT);

  // Subtract the borrow input (ensure it's the right type)
  SDValue BorrowInExt;
  if (BorrowIn.getValueType().isInteger()) {
    BorrowInExt = DAG.getZExtOrTrunc(BorrowIn, DL, VT);
  } else {
    // This shouldn't happen - SUBE expects integer borrow
    assert(false && "SUBE: BorrowIn must be an integer type");
    BorrowInExt = DAG.getConstant(0, DL, VT);
  }
  SDValue Diff2 = DAG.getNode(ISD::SUB, DL, VT, Diff1, BorrowInExt);
  // Compute second borrow
  SDValue Borrow2 = DAG.getSetCC(DL, MVT::i1, Diff1, BorrowInExt, ISD::SETULT);

  // Final borrow is OR of both borrows
  SDValue BorrowBit = DAG.getNode(ISD::OR, DL, MVT::i1, Borrow1, Borrow2);
  SDValue BorrowOut = DAG.getZExtOrTrunc(BorrowBit, DL, VT);

  // Return both difference and borrow
  return DAG.getMergeValues({Diff2, BorrowOut}, DL);
}

// x + y ⇒ {sum, cout}
SDValue SLOW32TargetLowering::LowerUADDO(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDNode *N = Op.getNode();
  SDValue X = N->getOperand(0);
  SDValue Y = N->getOperand(1);
  EVT VT0 = N->getValueType(0);  // e.g., i32
  EVT VT1 = N->getValueType(1);  // legal boolean type (often i32)

  SDValue Sum = DAG.getNode(ISD::ADD, DL, VT0, X, Y);
  // carry = (sum < x) - simple unsigned overflow detection
  SDValue Carry = DAG.getSetCC(DL, VT0, Sum, X, ISD::SETULT);
  
  // If VT1 != VT0, convert the carry
  if (VT1 != VT0) {
    Carry = DAG.getBoolExtOrTrunc(Carry, DL, VT1, VT0);
  }
  
  return DAG.getMergeValues({Sum, Carry}, DL);
}

// x + y + cin ⇒ {sum, cout}
SDValue SLOW32TargetLowering::LowerUADDO_CARRY(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDNode *N = Op.getNode();
  SDValue X = N->getOperand(0);
  SDValue Y = N->getOperand(1);
  SDValue CinAny = N->getOperand(2);      // may be i1 or widened boolean
  EVT VT0 = N->getValueType(0);
  EVT VT1 = N->getValueType(1);

  // Normalize Cin to 0/1 in VT0
  SDValue Cin = CinAny;
  if (Cin.getValueType() != VT0) {
    // Convert to 0/1 in VT0
    SDValue Zero = DAG.getConstant(0, DL, Cin.getValueType());
    SDValue CinBool = DAG.getSetCC(DL, VT0, Cin, Zero, ISD::SETNE);
    Cin = CinBool;  // Already in VT0 from SETCC
  }

  SDValue T   = DAG.getNode(ISD::ADD, DL, VT0, X, Y);
  SDValue Sum = DAG.getNode(ISD::ADD, DL, VT0, T, Cin);

  // cout = (t < x) | (sum < cin)
  SDValue C0 = DAG.getSetCC(DL, VT0, T, X, ISD::SETULT);   // carry from x+y
  SDValue C1 = DAG.getSetCC(DL, VT0, Sum, Cin, ISD::SETULT);  // carry from t+cin
  SDValue Cout = DAG.getNode(ISD::OR, DL, VT0, C0, C1);
  
  // Convert if needed
  if (VT1 != VT0) {
    Cout = DAG.getBoolExtOrTrunc(Cout, DL, VT1, VT0);
  }
  
  return DAG.getMergeValues({Sum, Cout}, DL);
}

// x - y ⇒ {diff, bout}
SDValue SLOW32TargetLowering::LowerUSUBO(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDNode *N = Op.getNode();
  SDValue X = N->getOperand(0);
  SDValue Y = N->getOperand(1);
  EVT VT0 = N->getValueType(0);
  EVT VT1 = N->getValueType(1);

  SDValue Diff = DAG.getNode(ISD::SUB, DL, VT0, X, Y);
  // borrow = (x < y) - simple unsigned underflow detection
  SDValue Borrow = DAG.getSetCC(DL, VT0, X, Y, ISD::SETULT);
  
  // If VT1 != VT0, convert the borrow
  if (VT1 != VT0) {
    Borrow = DAG.getBoolExtOrTrunc(Borrow, DL, VT1, VT0);
  }
  
  return DAG.getMergeValues({Diff, Borrow}, DL);
}

// x - y - bin ⇒ {diff, bout}
SDValue SLOW32TargetLowering::LowerUSUBO_CARRY(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDNode *N = Op.getNode();
  SDValue X = N->getOperand(0);
  SDValue Y = N->getOperand(1);
  SDValue BinAny = N->getOperand(2);      // may be i1 or widened boolean
  EVT VT0 = N->getValueType(0);
  EVT VT1 = N->getValueType(1);

  // Normalize Bin to 0/1 in VT0
  SDValue Bin = BinAny;
  if (Bin.getValueType() != VT0) {
    // Convert to 0/1 in VT0
    SDValue Zero = DAG.getConstant(0, DL, Bin.getValueType());
    SDValue BinBool = DAG.getSetCC(DL, VT0, Bin, Zero, ISD::SETNE);
    Bin = BinBool;  // Already in VT0 from SETCC
  }

  SDValue T    = DAG.getNode(ISD::SUB, DL, VT0, X, Y);
  SDValue Diff = DAG.getNode(ISD::SUB, DL, VT0, T, Bin);

  // bout = (x < y) | (t < bin)
  SDValue B0 = DAG.getSetCC(DL, VT0, X, Y, ISD::SETULT);   // borrow from x-y
  SDValue B1 = DAG.getSetCC(DL, VT0, T, Bin, ISD::SETULT); // borrow from t-bin
  SDValue Bout = DAG.getNode(ISD::OR, DL, VT0, B0, B1);
  
  // Convert if needed
  if (VT1 != VT0) {
    Bout = DAG.getBoolExtOrTrunc(Bout, DL, VT1, VT0);
  }
  
  return DAG.getMergeValues({Diff, Bout}, DL);
}

// Lower unsigned 32x32->64 multiply (returns both low and high parts)
// Since SLOW32 doesn't have a multiply-high instruction, we decompose
// into 16-bit multiplies following ChatGPT 5's approach B
SDValue SLOW32TargetLowering::LowerUMUL_LOHI(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue A = Op.getOperand(0);
  SDValue B = Op.getOperand(1);
  EVT VT = MVT::i32;
  
  // Split each 32-bit value into 16-bit halves
  SDValue Mask16 = DAG.getConstant(0xFFFF, DL, VT);
  SDValue Sixteen = DAG.getConstant(16, DL, VT);
  
  // A = (AH << 16) | AL
  SDValue AL = DAG.getNode(ISD::AND, DL, VT, A, Mask16);
  SDValue AH = DAG.getNode(ISD::SRL, DL, VT, A, Sixteen);
  
  // B = (BH << 16) | BL
  SDValue BL = DAG.getNode(ISD::AND, DL, VT, B, Mask16);
  SDValue BH = DAG.getNode(ISD::SRL, DL, VT, B, Sixteen);
  
  // Compute four 16x16->32 products
  SDValue P0 = DAG.getNode(ISD::MUL, DL, VT, AL, BL);  // AL * BL
  SDValue P1 = DAG.getNode(ISD::MUL, DL, VT, AL, BH);  // AL * BH
  SDValue P2 = DAG.getNode(ISD::MUL, DL, VT, AH, BL);  // AH * BL
  SDValue P3 = DAG.getNode(ISD::MUL, DL, VT, AH, BH);  // AH * BH
  
  // Now we need to combine:
  // Result = P0 + (P1 << 16) + (P2 << 16) + (P3 << 32)
  
  // Low 32 bits: P0_low + ((P1 + P2) << 16)_low
  // High 32 bits: P3 + P0_high + ((P1 + P2) << 16)_high + ((P1 + P2) >> 16)
  
  // First, compute P1 + P2
  SDValue Mid = DAG.getNode(ISD::ADD, DL, VT, P1, P2);
  
  // Add the middle part to P0's high 16 bits
  SDValue P0Hi = DAG.getNode(ISD::SRL, DL, VT, P0, Sixteen);
  SDValue MidPlusP0Hi = DAG.getNode(ISD::ADD, DL, VT, Mid, P0Hi);
  
  // Low result: (P0 & 0xFFFF) | ((MidPlusP0Hi & 0xFFFF) << 16)
  SDValue P0Lo = DAG.getNode(ISD::AND, DL, VT, P0, Mask16);
  SDValue MidLo = DAG.getNode(ISD::AND, DL, VT, MidPlusP0Hi, Mask16);
  SDValue MidLoShifted = DAG.getNode(ISD::SHL, DL, VT, MidLo, Sixteen);
  SDValue Lo = DAG.getNode(ISD::OR, DL, VT, P0Lo, MidLoShifted);
  
  // High result: P3 + (MidPlusP0Hi >> 16)
  SDValue MidHi = DAG.getNode(ISD::SRL, DL, VT, MidPlusP0Hi, Sixteen);
  SDValue Hi = DAG.getNode(ISD::ADD, DL, VT, P3, MidHi);
  
  return DAG.getMergeValues({Lo, Hi}, DL);
}

// Lower signed 32x32->64 multiply
SDValue SLOW32TargetLowering::LowerSMUL_LOHI(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue A = Op.getOperand(0);
  SDValue B = Op.getOperand(1);
  EVT VT = MVT::i32;
  
  // Use unsigned multiply and then correct for sign
  SDValue UMulLoHi = DAG.getNode(ISD::UMUL_LOHI, DL, DAG.getVTList(VT, VT), A, B);
  SDValue Lo = UMulLoHi.getValue(0);
  SDValue Hi = UMulLoHi.getValue(1);
  
  // Correction for signed multiply:
  // If A < 0, subtract B from Hi
  // If B < 0, subtract A from Hi
  SDValue Zero = DAG.getConstant(0, DL, VT);
  SDValue ASign = DAG.getSetCC(DL, VT, A, Zero, ISD::SETLT);
  SDValue BSign = DAG.getSetCC(DL, VT, B, Zero, ISD::SETLT);
  
  // Create masked values for subtraction
  SDValue BMask = DAG.getNode(ISD::AND, DL, VT, B, ASign);
  SDValue AMask = DAG.getNode(ISD::AND, DL, VT, A, BSign);
  
  // Adjust Hi
  Hi = DAG.getNode(ISD::SUB, DL, VT, Hi, BMask);
  Hi = DAG.getNode(ISD::SUB, DL, VT, Hi, AMask);
  
  return DAG.getMergeValues({Lo, Hi}, DL);
}

// Lower unsigned multiply-high (just the high part)
SDValue SLOW32TargetLowering::LowerMULHU(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue A = Op.getOperand(0);
  SDValue B = Op.getOperand(1);
  EVT VT = MVT::i32;
  
  SDValue UMulLoHi = DAG.getNode(ISD::UMUL_LOHI, DL, DAG.getVTList(VT, VT), A, B);
  return UMulLoHi.getValue(1);  // Return just the high part
}

// Lower signed multiply-high (just the high part)
SDValue SLOW32TargetLowering::LowerMULHS(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue A = Op.getOperand(0);
  SDValue B = Op.getOperand(1);
  EVT VT = MVT::i32;
  
  SDValue SMulLoHi = DAG.getNode(ISD::SMUL_LOHI, DL, DAG.getVTList(VT, VT), A, B);
  return SMulLoHi.getValue(1);  // Return just the high part
}
