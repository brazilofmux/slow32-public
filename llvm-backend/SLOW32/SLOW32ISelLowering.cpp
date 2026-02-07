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
#include "llvm/Support/Alignment.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

#include <algorithm>

using namespace llvm;

#include "SLOW32GenCallingConv.inc"

static const MCPhysReg SLOW32ArgRegs[] = {
    SLOW32::R3, SLOW32::R4, SLOW32::R5, SLOW32::R6,
    SLOW32::R7, SLOW32::R8, SLOW32::R9, SLOW32::R10};
static constexpr unsigned NumSLOW32ArgRegs =
    sizeof(SLOW32ArgRegs) / sizeof(SLOW32ArgRegs[0]);

// Consecutive-register shadow lookup for varargs calling convention.
// Unlike getShadowFor64Bit (which uses pair-aligned R3/R5/R7/R9),
// this returns the immediately next register for any arg reg.
// Only R3-R7 are used for varargs; R8-R10 are excluded to avoid gaps
// between the register save area and stack arguments.
static Register getNextArgReg(Register Reg) {
  switch (Reg) {
  case SLOW32::R3:  return SLOW32::R4;
  case SLOW32::R4:  return SLOW32::R5;
  case SLOW32::R5:  return SLOW32::R6;
  case SLOW32::R6:  return SLOW32::R7;
  // R7 -> R8 and beyond are omitted; R8-R10 not saved for varargs
  default: return Register();
  }
}

static Register getShadowFor64Bit(Register Reg) {
  switch (Reg) {
  case SLOW32::R1:
    return SLOW32::R2;
  case SLOW32::R3:
    return SLOW32::R4;
  case SLOW32::R5:
    return SLOW32::R6;
  case SLOW32::R7:
    return SLOW32::R8;
  case SLOW32::R9:
    return SLOW32::R10;
  default:
    return Register();
  }
}

static SDValue convertLocToValVT(SelectionDAG &DAG, SDValue Val,
                                 const CCValAssign &VA, const SDLoc &DL) {
  EVT LocVT = VA.getLocVT();
  EVT ValVT = VA.getValVT();

  switch (VA.getLocInfo()) {
  case CCValAssign::SExt:
    Val = DAG.getNode(ISD::AssertSext, DL, LocVT, Val,
                      DAG.getValueType(ValVT));
    Val = DAG.getNode(ISD::TRUNCATE, DL, ValVT, Val);
    break;
  case CCValAssign::ZExt:
    Val = DAG.getNode(ISD::AssertZext, DL, LocVT, Val,
                      DAG.getValueType(ValVT));
    Val = DAG.getNode(ISD::TRUNCATE, DL, ValVT, Val);
    break;
  case CCValAssign::AExt:
    Val = DAG.getNode(ISD::TRUNCATE, DL, ValVT, Val);
    break;
  case CCValAssign::Trunc:
    Val = DAG.getNode(ISD::TRUNCATE, DL, ValVT, Val);
    break;
  case CCValAssign::BCvt:
    Val = DAG.getNode(ISD::BITCAST, DL, ValVT, Val);
    break;
  case CCValAssign::FPExt:
  case CCValAssign::Indirect:
  case CCValAssign::Full:
    if (ValVT != Val.getValueType())
      Val = DAG.getNode(ISD::BITCAST, DL, ValVT, Val);
    break;
  case CCValAssign::SExtUpper:
  case CCValAssign::ZExtUpper:
  case CCValAssign::AExtUpper:
  case CCValAssign::VExt:
    llvm_unreachable("Unsupported CCValAssign loc info for SLOW32");
  }

  return Val;
}

static SDValue convertValToLocVT(SelectionDAG &DAG, SDValue Val,
                                 const CCValAssign &VA, const SDLoc &DL) {
  EVT LocVT = VA.getLocVT();
  switch (VA.getLocInfo()) {
  case CCValAssign::Full:
    if (Val.getValueType() == LocVT)
      return Val;
    return DAG.getNode(ISD::BITCAST, DL, LocVT, Val);
  case CCValAssign::SExt:
    return DAG.getNode(ISD::SIGN_EXTEND, DL, LocVT, Val);
  case CCValAssign::ZExt:
    return DAG.getNode(ISD::ZERO_EXTEND, DL, LocVT, Val);
  case CCValAssign::AExt:
    return DAG.getNode(ISD::ANY_EXTEND, DL, LocVT, Val);
  case CCValAssign::Trunc:
    return DAG.getNode(ISD::TRUNCATE, DL, LocVT, Val);
  case CCValAssign::BCvt:
    return DAG.getNode(ISD::BITCAST, DL, LocVT, Val);
  case CCValAssign::Indirect:
    // Caller will materialize address separately.
    return Val;
  case CCValAssign::FPExt:
  case CCValAssign::SExtUpper:
  case CCValAssign::ZExtUpper:
  case CCValAssign::AExtUpper:
  case CCValAssign::VExt:
    llvm_unreachable("Unsupported CCValAssign loc info for SLOW32");
  }

  llvm_unreachable("Unhandled loc info");
}

#ifndef NDEBUG
static bool isSupportedLocInfo(CCValAssign::LocInfo Info) {
  switch (Info) {
  case CCValAssign::Full:
  case CCValAssign::SExt:
  case CCValAssign::ZExt:
  case CCValAssign::AExt:
  case CCValAssign::Trunc:
  case CCValAssign::BCvt:
  case CCValAssign::Indirect:
    return true;
  default:
    return false;
  }
}
#endif

#define DEBUG_TYPE "slow32-lower"

SLOW32TargetLowering::SLOW32TargetLowering(const TargetMachine &TM)
  : TargetLowering(TM, static_cast<const SLOW32TargetMachine&>(TM).getSubtarget()) {
  // Set up the register classes
  addRegisterClass(MVT::i32, &SLOW32::GPRRegClass);
  addRegisterClass(MVT::f32, &SLOW32::GPRRegClass);
  addRegisterClass(MVT::f64, &SLOW32::GPRPairRegClass);

  // SLOW32 uses soft-float in GPRs with native f32/f64 instructions.

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

  // Booleans come back as 0/1 in a GPR
  setBooleanContents(ZeroOrOneBooleanContent);
  setBooleanVectorContents(ZeroOrOneBooleanContent);

  // i1 values live in memory as bytes; promote loads/stores through i8.
  setLoadExtAction({ISD::EXTLOAD, ISD::SEXTLOAD, ISD::ZEXTLOAD}, MVT::i32,
                   MVT::i1, Promote);
  setTruncStoreAction(MVT::i32, MVT::i1, Expand);
  
  // Configure SETCC to produce 0/1 in a GPR (will use default lowering)
  // This is important for SELECT to work correctly
  setOperationAction(ISD::SETCC, MVT::i32, Legal);
  
  // Configure BRCOND to be Custom (we have a lowering for it)
  setOperationAction(ISD::BRCOND, MVT::Other, Custom);

  // Custom lower 32-bit multiply-high operations for i64 multiply expansion
  setOperationAction(ISD::UMUL_LOHI, MVT::i32, Custom);
  setOperationAction(ISD::SMUL_LOHI, MVT::i32, Custom);
  setOperationAction(ISD::MULHS, MVT::i32, Legal);
  setOperationAction(ISD::MULHU, MVT::i32, Custom);
  
  // i32 unsigned division/remainder - SLOW32 only has signed DIV/REM instructions
  // Use custom lowering to generate libcalls for unsigned operations
  setOperationAction(ISD::UDIV, MVT::i32, Custom);  // Will call __udivsi3
  setOperationAction(ISD::UREM, MVT::i32, Custom);  // Will call __umodsi3
  setOperationAction(ISD::UDIVREM, MVT::i32, Expand);  // Combined operation also needs expansion
  setOperationAction(ISD::SDIVREM, MVT::i32, Expand);  // Also expand signed divrem
  
  // SLOW32 is not recognized in LLVM's RuntimeLibcalls infrastructure,
  // so we need to explicitly set which libcall implementations to use.
  // These must be set in BOTH places due to LLVM's transitional libcall API:
  //   - Here via setLibcallImpl() for makeLibCall(RTLIB::Libcall) calls
  //   - In SLOW32Subtarget::initLibcallLoweringInfo() for DAG.getLibcalls() calls

  // i64 division/remainder
  setLibcallImpl(RTLIB::SDIV_I64, RTLIB::impl___divdi3);
  setLibcallImpl(RTLIB::UDIV_I64, RTLIB::impl___udivdi3);
  setLibcallImpl(RTLIB::SREM_I64, RTLIB::impl___moddi3);
  setLibcallImpl(RTLIB::UREM_I64, RTLIB::impl___umoddi3);

  // i32 unsigned division/remainder (SLOW32 only has signed DIV/REM instructions)
  setLibcallImpl(RTLIB::UDIV_I32, RTLIB::impl___udivsi3);
  setLibcallImpl(RTLIB::UREM_I32, RTLIB::impl___umodsi3);
  setLibcallImpl(RTLIB::REM_F32, RTLIB::impl_fmodf);
  setLibcallImpl(RTLIB::REM_F64, RTLIB::impl_fmod);
  setLibcallImpl(RTLIB::RINT_F32, RTLIB::impl_rintf);
  setLibcallImpl(RTLIB::RINT_F64, RTLIB::impl_rint);
  setLibcallImpl(RTLIB::NEARBYINT_F32, RTLIB::impl_nearbyintf);
  setLibcallImpl(RTLIB::NEARBYINT_F64, RTLIB::impl_nearbyint);
  setLibcallImpl(RTLIB::FLOOR_F32, RTLIB::impl_floorf);
  setLibcallImpl(RTLIB::FLOOR_F64, RTLIB::impl_floor);
  setLibcallImpl(RTLIB::CEIL_F32, RTLIB::impl_ceilf);
  setLibcallImpl(RTLIB::CEIL_F64, RTLIB::impl_ceil);
  setLibcallImpl(RTLIB::TRUNC_F32, RTLIB::impl_truncf);
  setLibcallImpl(RTLIB::TRUNC_F64, RTLIB::impl_trunc);
  setLibcallImpl(RTLIB::ROUND_F32, RTLIB::impl_roundf);
  setLibcallImpl(RTLIB::ROUND_F64, RTLIB::impl_round);
  setLibcallImpl(RTLIB::FMA_F32, RTLIB::impl_fmaf);
  setLibcallImpl(RTLIB::FMA_F64, RTLIB::impl_fma);

  // The runtime ships native C implementations of the basic memory helpers.
  setLibcallImpl(RTLIB::MEMCPY, RTLIB::impl_memcpy);
  setLibcallImpl(RTLIB::MEMMOVE, RTLIB::impl_memmove);
  setLibcallImpl(RTLIB::MEMSET, RTLIB::impl_memset);

  // SELECT is our canonical operation — SELECT_CC always expands to SELECT + SETCC
  setOperationAction(ISD::SELECT_CC, MVT::i32, Expand);
  setOperationAction(ISD::SELECT, MVT::i32, Custom);
  // Note: We DO support SETCC via SLT/SLTU/SEQ/SNE instructions

  // Support for 64-bit operations - use custom lowering for shifts
  // SLOW32 doesn't have carry flags, so we need to use shift-parts approach like RISC-V
  setOperationAction(ISD::SHL_PARTS, MVT::i32, Custom);
  setOperationAction(ISD::SRA_PARTS, MVT::i32, Custom);
  setOperationAction(ISD::SRL_PARTS, MVT::i32, Custom);

  // Set stack pointer - R29 in SLOW32 is the stack pointer
  setStackPointerRegisterToSaveRestore(SLOW32::R29);

  // Floating point operations are supported in GPRs with dedicated opcodes.

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
  setOperationAction(ISD::ConstantPool, MVT::i32, Custom);

  // Varargs support - follow RISC-V pattern
  setOperationAction(ISD::VASTART, MVT::Other, Custom);
  // Let LLVM expand VAARG, VACOPY, VAEND
  setOperationAction(ISD::VAARG, MVT::Other, Custom);
  setOperationAction(ISD::VACOPY, MVT::Other, Expand);
  setOperationAction(ISD::VAEND, MVT::Other, Expand);

  // Remove custom lowering for loads/stores - handle in patterns instead
  // setOperationAction(ISD::LOAD, MVT::i32, Custom);
  // setOperationAction(ISD::STORE, MVT::i32, Custom);

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
  
  // Register custom DAG combines
  setTargetDAGCombine({ISD::ADD, ISD::MUL, ISD::AND, ISD::OR, ISD::SHL});

  // f32/f64 operations are legal; fmod remains a libcall.
  setOperationAction(ISD::FADD, MVT::f32, Legal);
  setOperationAction(ISD::FSUB, MVT::f32, Legal);
  setOperationAction(ISD::FMUL, MVT::f32, Legal);
  setOperationAction(ISD::FDIV, MVT::f32, Legal);
  setOperationAction(ISD::FSQRT, MVT::f32, Legal);
  setOperationAction(ISD::FNEG, MVT::f32, Legal);
  setOperationAction(ISD::FABS, MVT::f32, Legal);
  setOperationAction(ISD::FCOPYSIGN, MVT::f32, Custom);
  setOperationAction(ISD::FREM, MVT::f32, Expand);
  setOperationAction(ISD::FMA, MVT::f32, Expand);
  setOperationAction(ISD::FRINT, MVT::f32, Expand);
  setOperationAction(ISD::FNEARBYINT, MVT::f32, Expand);
  setOperationAction(ISD::FTRUNC, MVT::f32, Expand);
  setOperationAction(ISD::FCEIL, MVT::f32, Expand);
  setOperationAction(ISD::FFLOOR, MVT::f32, Expand);
  setOperationAction(ISD::SETCC, MVT::f32, Legal);
  setOperationAction(ISD::BR_CC, MVT::f32, Expand);
  setOperationAction(ISD::SELECT, MVT::f32, Custom);
  setOperationAction(ISD::SELECT_CC, MVT::f32, Expand);
  setOperationAction(ISD::FP_ROUND, MVT::f32, Legal);
  // f32 and i32 share GPR, so bitcast is a no-op.
  setOperationAction(ISD::BITCAST, MVT::f32, Legal);

  setOperationAction(ISD::FADD, MVT::f64, Legal);
  setOperationAction(ISD::FSUB, MVT::f64, Legal);
  setOperationAction(ISD::FMUL, MVT::f64, Legal);
  setOperationAction(ISD::FDIV, MVT::f64, Legal);
  setOperationAction(ISD::FSQRT, MVT::f64, Legal);
  setOperationAction(ISD::FNEG, MVT::f64, Legal);
  setOperationAction(ISD::FABS, MVT::f64, Legal);
  setOperationAction(ISD::FCOPYSIGN, MVT::f64, Custom);
  setOperationAction(ISD::FREM, MVT::f64, Expand);
  setOperationAction(ISD::FMA, MVT::f64, Expand);
  setOperationAction(ISD::FRINT, MVT::f64, Expand);
  setOperationAction(ISD::FNEARBYINT, MVT::f64, Expand);
  setOperationAction(ISD::FTRUNC, MVT::f64, Expand);
  setOperationAction(ISD::FCEIL, MVT::f64, Expand);
  setOperationAction(ISD::FFLOOR, MVT::f64, Expand);
  setOperationAction(ISD::SETCC, MVT::f64, Legal);
  setOperationAction(ISD::BR_CC, MVT::f64, Expand);
  setOperationAction(ISD::SELECT, MVT::f64, Custom);
  setOperationAction(ISD::SELECT_CC, MVT::f64, Expand);
  setOperationAction(ISD::FP_EXTEND, MVT::f64, Legal);

  setOperationAction(ISD::FP_TO_SINT, MVT::i32, Legal);
  setOperationAction(ISD::FP_TO_UINT, MVT::i32, Legal);
  setOperationAction(ISD::SINT_TO_FP, MVT::f32, Legal);
  setOperationAction(ISD::UINT_TO_FP, MVT::f32, Legal);
  setOperationAction(ISD::SINT_TO_FP, MVT::f64, Legal);
  setOperationAction(ISD::UINT_TO_FP, MVT::f64, Legal);

  // i64 ↔ FP conversions: use hardware FCVT_L/FCVT_S_L instructions
  // instead of slow libcalls (__fixdfdi, __floatdidf, etc.)
  setOperationAction(ISD::FP_TO_SINT, MVT::i64, Custom);
  setOperationAction(ISD::FP_TO_UINT, MVT::i64, Custom);
  setOperationAction(ISD::SINT_TO_FP, MVT::i64, Custom);
  setOperationAction(ISD::UINT_TO_FP, MVT::i64, Custom);

  // No extending FP load — decompose into f32 load + fcvt.d.s.
  setLoadExtAction(ISD::EXTLOAD, MVT::f64, MVT::f32, Expand);
  // No truncating FP store — decompose into fcvt.s.d + f32 store.
  setTruncStoreAction(MVT::f64, MVT::f32, Expand);

  // f32 loads/stores are handled by TableGen patterns (LDW/STW).
  // f64 loads/stores need custom splitting into two i32 loads/stores.
  setOperationAction(ISD::LOAD, MVT::f64, Custom);
  setOperationAction(ISD::STORE, MVT::f64, Custom);
}

const char *SLOW32TargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch ((SLOW32ISD::NodeType)Opcode) {
  case SLOW32ISD::FIRST_NUMBER: break;
  case SLOW32ISD::RET_FLAG: return "SLOW32ISD::RET_FLAG";
  case SLOW32ISD::BR_CC: return "SLOW32ISD::BR_CC";
  case SLOW32ISD::BR_NE: return "SLOW32ISD::BR_NE";
  case SLOW32ISD::BR_LT: return "SLOW32ISD::BR_LT";
  case SLOW32ISD::BR_GE: return "SLOW32ISD::BR_GE";
  case SLOW32ISD::BR_GT: return "SLOW32ISD::BR_GT";
  case SLOW32ISD::BR_LE: return "SLOW32ISD::BR_LE";
  case SLOW32ISD::BR_LTU: return "SLOW32ISD::BR_LTU";
  case SLOW32ISD::BR_GEU: return "SLOW32ISD::BR_GEU";
  case SLOW32ISD::BR_GTU: return "SLOW32ISD::BR_GTU";
  case SLOW32ISD::BR_LEU: return "SLOW32ISD::BR_LEU";
  case SLOW32ISD::CALL: return "SLOW32ISD::CALL";
  case SLOW32ISD::HI: return "SLOW32ISD::HI";
  case SLOW32ISD::LO: return "SLOW32ISD::LO";
  case SLOW32ISD::BuildPairF64: return "SLOW32ISD::BuildPairF64";
  case SLOW32ISD::SplitF64: return "SLOW32ISD::SplitF64";
  case SLOW32ISD::FCVT_L: return "SLOW32ISD::FCVT_L";
  case SLOW32ISD::FCVT_LU: return "SLOW32ISD::FCVT_LU";
  case SLOW32ISD::FCVT_FROM_L: return "SLOW32ISD::FCVT_FROM_L";
  case SLOW32ISD::FCVT_FROM_LU: return "SLOW32ISD::FCVT_FROM_LU";
  }
  return nullptr;
}

SDValue SLOW32TargetLowering::LowerOperation(SDValue Op, SelectionDAG &DAG) const {
  switch (Op.getOpcode()) {
    case ISD::GlobalAddress:  return LowerGlobalAddress(Op, DAG);
    case ISD::ExternalSymbol: return LowerExternalSymbol(Op, DAG);
    case ISD::JumpTable:      return LowerJumpTable(Op, DAG);
    case ISD::ConstantPool:   return LowerConstantPool(Op, DAG);
    case ISD::LOAD:           return LowerLOAD(Op, DAG);
    case ISD::STORE:          return LowerSTORE(Op, DAG);
    case ISD::VASTART:        return LowerVASTART(Op, DAG);
    case ISD::VAARG:          return LowerVAARG(Op, DAG);
    case ISD::BRCOND:         return LowerBRCOND(Op, DAG);
    case ISD::BR_CC:          return LowerBR_CC(Op, DAG);
    case ISD::FCOPYSIGN:      return LowerFCOPYSIGN(Op, DAG);
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
    case ISD::UDIV:           return LowerUDIV(Op, DAG);
    case ISD::UREM:           return LowerUREM(Op, DAG);
    case ISD::SINT_TO_FP:
    case ISD::UINT_TO_FP: {
      // i64 → fp: split i64 into halves, combine into GPRPair, hardware convert
      EVT DstVT = Op.getValueType();
      SDValue Src = Op.getOperand(0);
      if (Src.getValueType() != MVT::i64)
        return SDValue();
      SDLoc DL(Op);
      SDValue Lo = DAG.getNode(ISD::EXTRACT_ELEMENT, DL, MVT::i32, Src,
                               DAG.getConstant(0, DL, MVT::i32));
      SDValue Hi = DAG.getNode(ISD::EXTRACT_ELEMENT, DL, MVT::i32, Src,
                               DAG.getConstant(1, DL, MVT::i32));
      SDValue Pair = DAG.getNode(SLOW32ISD::BuildPairF64, DL, MVT::f64, Lo, Hi);
      unsigned Opc = (Op.getOpcode() == ISD::SINT_TO_FP) ? SLOW32ISD::FCVT_FROM_L
                                                          : SLOW32ISD::FCVT_FROM_LU;
      return DAG.getNode(Opc, DL, DstVT, Pair);
    }
    default: return SDValue();
  }
}
SDValue SLOW32TargetLowering::LowerGlobalAddress(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  GlobalAddressSDNode *GA = cast<GlobalAddressSDNode>(Op);
  EVT VT = Op.getValueType();
  const GlobalValue *GV = GA->getGlobal();
  
  // Debug: Check if we have an offset
  int64_t Offset = GA->getOffset();
  LLVM_DEBUG(dbgs() << "LowerGlobalAddress: " << GV->getName() 
                    << " offset=" << Offset << "\n");

  // Use %hi/%lo addressing for all global addresses (both functions and data)
  // Create target global address nodes with appropriate flags
  // Materialise the address through the LOAD_ADDR pseudo so we expand to
  // canonical LUI/ADDI sequences post-RA.
  SDValue TargetGA = DAG.getTargetGlobalAddress(GV, DL, VT, Offset,
                                                GA->getTargetFlags());
  SDNode *Mov = DAG.getMachineNode(SLOW32::LOAD_ADDR, DL, VT, TargetGA);
  return SDValue(Mov, 0);
}

SDValue SLOW32TargetLowering::LowerLOAD(SDValue Op, SelectionDAG &DAG) const {
  LoadSDNode *Load = cast<LoadSDNode>(Op);
  SDValue Addr = Load->getBasePtr();
  SDLoc DL(Op);
  EVT VT = Load->getValueType(0);

  if (VT == MVT::f64) {
    EVT PtrVT = getPointerTy(DAG.getDataLayout());
    SDValue Chain = Load->getChain();
    MachinePointerInfo MPI = Load->getPointerInfo();

    Align BaseAlign = Load->getAlign();
    SDValue Lo = DAG.getLoad(MVT::i32, DL, Chain, Addr, MPI, BaseAlign);
    Chain = Lo.getValue(1);
    SDValue AddrHi = DAG.getNode(ISD::ADD, DL, PtrVT, Addr,
                                 DAG.getConstant(4, DL, PtrVT));
    SDValue Hi = DAG.getLoad(MVT::i32, DL, Chain, AddrHi,
                             MPI.getWithOffset(4),
                             commonAlignment(BaseAlign, 4));
    Chain = Hi.getValue(1);

    SDValue Val = DAG.getNode(SLOW32ISD::BuildPairF64, DL, MVT::f64, Lo, Hi);
    return DAG.getMergeValues({Val, Chain}, DL);
  }

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
  EVT VT = Store->getValue().getValueType();

  if (VT == MVT::f64) {
    EVT PtrVT = getPointerTy(DAG.getDataLayout());
    SDValue Chain = Store->getChain();
    MachinePointerInfo MPI = Store->getPointerInfo();

    SDValue Split = DAG.getNode(SLOW32ISD::SplitF64, DL,
                                DAG.getVTList(MVT::i32, MVT::i32),
                                Store->getValue());
    SDValue Lo = Split.getValue(0);
    SDValue Hi = Split.getValue(1);

    Align BaseAlign = Store->getAlign();
    SDValue StoreLo = DAG.getStore(Chain, DL, Lo, Addr, MPI, BaseAlign);
    SDValue AddrHi = DAG.getNode(ISD::ADD, DL, PtrVT, Addr,
                                 DAG.getConstant(4, DL, PtrVT));
    SDValue StoreHi = DAG.getStore(StoreLo, DL, Hi, AddrHi,
                                   MPI.getWithOffset(4),
                                   commonAlignment(BaseAlign, 4));
    return StoreHi;
  }

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

  // Materialise external addresses through LOAD_ADDR like regular globals so
  // we benefit from the same folding and post-RA expansion.
  SDValue TargetES = DAG.getTargetExternalSymbol(Symbol, VT, ES->getTargetFlags());
  SDNode *Mov = DAG.getMachineNode(SLOW32::LOAD_ADDR, DL, VT, TargetES);
  return SDValue(Mov, 0);
}

SDValue SLOW32TargetLowering::LowerJumpTable(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  JumpTableSDNode *JT = cast<JumpTableSDNode>(Op);
  EVT VT = Op.getValueType();

  // Similar to global addresses, use %hi/%lo addressing with RISC-V style
  SDValue TargetJT =
      DAG.getTargetJumpTable(JT->getIndex(), VT, JT->getTargetFlags());
  SDNode *Mov = DAG.getMachineNode(SLOW32::LOAD_ADDR, DL, VT, TargetJT);
  return SDValue(Mov, 0);
}

SDValue SLOW32TargetLowering::LowerConstantPool(SDValue Op,
                                                 SelectionDAG &DAG) const {
  SDLoc DL(Op);
  ConstantPoolSDNode *CP = cast<ConstantPoolSDNode>(Op);
  EVT VT = Op.getValueType();

  SDValue TargetCP;
  if (CP->isMachineConstantPoolEntry())
    TargetCP = DAG.getTargetConstantPool(CP->getMachineCPVal(), VT,
                                         CP->getAlign(), CP->getOffset(),
                                         CP->getTargetFlags());
  else
    TargetCP = DAG.getTargetConstantPool(CP->getConstVal(), VT,
                                         CP->getAlign(), CP->getOffset(),
                                         CP->getTargetFlags());

  SDNode *Mov = DAG.getMachineNode(SLOW32::LOAD_ADDR, DL, VT, TargetCP);
  return SDValue(Mov, 0);
}

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

  // Handle f64 by splitting into two i32 halves, selecting each,
  // then recombining via BuildPairF64 (i64 is not legal).
  if (VT == MVT::f64) {
    SDValue TrueSplit = DAG.getNode(SLOW32ISD::SplitF64, DL,
                                    DAG.getVTList(MVT::i32, MVT::i32), TrueV);
    SDValue FalseSplit = DAG.getNode(SLOW32ISD::SplitF64, DL,
                                     DAG.getVTList(MVT::i32, MVT::i32), FalseV);
    SDValue ResLo = DAG.getNode(ISD::SELECT, DL, MVT::i32, Cond,
                                TrueSplit.getValue(0), FalseSplit.getValue(0));
    SDValue ResHi = DAG.getNode(ISD::SELECT, DL, MVT::i32, Cond,
                                TrueSplit.getValue(1), FalseSplit.getValue(1));
    return DAG.getNode(SLOW32ISD::BuildPairF64, DL, MVT::f64, ResLo, ResHi);
  }

  // Ensure condition is normalized to i32 (0 or 1)
  if (Cond.getValueType() != MVT::i32) {
    Cond = DAG.getNode(ISD::ZERO_EXTEND, DL, MVT::i32, Cond);
  }

  // For f32, bitcast to i32, select, bitcast back. BITCAST f32↔i32 is Legal.
  if (VT == MVT::f32) {
    SDValue TrueI = DAG.getNode(ISD::BITCAST, DL, MVT::i32, TrueV);
    SDValue FalseI = DAG.getNode(ISD::BITCAST, DL, MVT::i32, FalseV);
    SDValue ResI = DAG.getNode(ISD::SELECT, DL, MVT::i32, Cond, TrueI, FalseI);
    return DAG.getNode(ISD::BITCAST, DL, MVT::f32, ResI);
  }

  // Only handle integer selects below.
  if (!VT.isInteger()) {
    return SDValue();
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

SDValue SLOW32TargetLowering::LowerFCOPYSIGN(SDValue Op,
                                             SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue Mag = Op.getOperand(0);
  SDValue Sign = Op.getOperand(1);
  EVT VT = Op.getValueType();

  SDValue MagMask = DAG.getConstant(0x7fffffff, DL, MVT::i32);
  SDValue SignMask = DAG.getConstant(0x80000000u, DL, MVT::i32);

  if (VT == MVT::f32) {
    // f32: bitcast to i32 and use integer bit ops.
    SDValue MagBits = DAG.getNode(ISD::BITCAST, DL, MVT::i32, Mag);
    SDValue SignBits;
    if (Sign.getValueType() == MVT::f64) {
      // Mixed copysign(f32, f64): extract sign from f64 high word.
      SDValue Split = DAG.getNode(SLOW32ISD::SplitF64, DL,
                                  DAG.getVTList(MVT::i32, MVT::i32), Sign);
      SignBits = Split.getValue(1);
    } else {
      SignBits = DAG.getNode(ISD::BITCAST, DL, MVT::i32, Sign);
    }
    SDValue MagMasked = DAG.getNode(ISD::AND, DL, MVT::i32, MagBits, MagMask);
    SDValue SignMasked = DAG.getNode(ISD::AND, DL, MVT::i32, SignBits, SignMask);
    SDValue ResBits = DAG.getNode(ISD::OR, DL, MVT::i32, MagMasked, SignMasked);
    return DAG.getNode(ISD::BITCAST, DL, MVT::f32, ResBits);
  }

  // f64: split into register pair, operate on the high word (sign bit is
  // bit 31 of the high i32 in IEEE 754 double).
  SDValue MagSplit = DAG.getNode(SLOW32ISD::SplitF64, DL,
                                 DAG.getVTList(MVT::i32, MVT::i32), Mag);
  SDValue MagLo = MagSplit.getValue(0);
  SDValue MagHi = MagSplit.getValue(1);

  SDValue SignBits;
  if (Sign.getValueType() == MVT::f64) {
    SDValue SignSplit = DAG.getNode(SLOW32ISD::SplitF64, DL,
                                   DAG.getVTList(MVT::i32, MVT::i32), Sign);
    SignBits = SignSplit.getValue(1);
  } else {
    // Mixed copysign(f64, f32): extract sign from f32 bits directly.
    SignBits = DAG.getNode(ISD::BITCAST, DL, MVT::i32, Sign);
  }

  SDValue HiMasked = DAG.getNode(ISD::AND, DL, MVT::i32, MagHi, MagMask);
  SDValue SignMasked = DAG.getNode(ISD::AND, DL, MVT::i32, SignBits, SignMask);
  SDValue NewHi = DAG.getNode(ISD::OR, DL, MVT::i32, HiMasked, SignMasked);

  return DAG.getNode(SLOW32ISD::BuildPairF64, DL, MVT::f64, MagLo, NewHi);
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

static SDValue emitBranchForCond(SelectionDAG &DAG, SDLoc DL,
                                SDValue Chain, ISD::CondCode CC,
                                SDValue LHS, SDValue RHS, SDValue Dest) {
  auto Emit = [&](SLOW32ISD::NodeType Opcode) {
    return DAG.getNode(Opcode, DL, MVT::Other, Chain, LHS, RHS, Dest);
  };

  switch (CC) {
  case ISD::SETEQ:
    return Emit(SLOW32ISD::BR_CC);
  case ISD::SETNE:
    return Emit(SLOW32ISD::BR_NE);
  case ISD::SETLT:
    return Emit(SLOW32ISD::BR_LT);
  case ISD::SETGE:
    return Emit(SLOW32ISD::BR_GE);
  case ISD::SETGT:
    return Emit(SLOW32ISD::BR_GT);
  case ISD::SETLE:
    return Emit(SLOW32ISD::BR_LE);
  case ISD::SETULT:
    return Emit(SLOW32ISD::BR_LTU);
  case ISD::SETUGE:
    return Emit(SLOW32ISD::BR_GEU);
  case ISD::SETUGT:
    return Emit(SLOW32ISD::BR_GTU);
  case ISD::SETULE:
    return Emit(SLOW32ISD::BR_LEU);
  default:
    break;
  }

  SDValue Cmp = DAG.getNode(ISD::SETCC, DL, MVT::i32, LHS, RHS,
                            DAG.getCondCode(CC));
  SDValue Zero = DAG.getConstant(0, DL, MVT::i32);
  return DAG.getNode(SLOW32ISD::BR_NE, DL, MVT::Other,
                     Chain, Cmp, Zero, Dest);
}

SDValue SLOW32TargetLowering::LowerBRCOND(SDValue Op, SelectionDAG &DAG) const {
  SDValue Chain = Op.getOperand(0);
  SDValue Cond = Op.getOperand(1);
  SDValue Dest = Op.getOperand(2);
  SDLoc DL(Op);

  LLVM_DEBUG(dbgs() << "LowerBRCOND opcode: " << Cond.getOpcode() << "\n");

  if (Cond.getOpcode() == ISD::SETCC) {
    SDValue LHS = Cond.getOperand(0);
    SDValue RHS = Cond.getOperand(1);
    // Only decompose integer SETCC into direct branch nodes (BR_LT, BR_GEU,
    // etc.).  Float SETCC must go through the SETCC instruction (FEQ_S,
    // FLT_S, ...) and then branch on the i32 result, because the BR_*
    // nodes emit integer compare-and-branch instructions.
    if (LHS.getValueType().isInteger()) {
      ISD::CondCode CC = cast<CondCodeSDNode>(Cond.getOperand(2))->get();
      return emitBranchForCond(DAG, DL, Chain, CC, LHS, RHS, Dest);
    }
  }

  SDValue Zero = DAG.getConstant(0, DL, MVT::i32);
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

  return emitBranchForCond(DAG, DL, Chain, CC, LHS, RHS, Dest);
}

SDValue SLOW32TargetLowering::LowerFormalArguments(
    SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &dl,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  MachineRegisterInfo &RegInfo = MF.getRegInfo();
  SLOW32MachineFunctionInfo *FuncInfo = MF.getInfo<SLOW32MachineFunctionInfo>();

  // Seed canonical live-ins so prologue code can freely touch them.
  MachineBasicBlock &EntryMBB = MF.front();
  for (MCRegister R : {SLOW32::R29, SLOW32::R30, SLOW32::R31}) {
    if (!RegInfo.isLiveIn(R))
      RegInfo.addLiveIn(R);
    if (!EntryMBB.isLiveIn(R))
      EntryMBB.addLiveIn(R);
  }

  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, isVarArg, MF, ArgLocs, *DAG.getContext());
  CCInfo.AnalyzeFormalArguments(Ins,
                               isVarArg ? CC_SLOW32_VarArg : CC_SLOW32);

  SmallVector<SDValue, 8> Chains;
  SmallVector<SDValue, 8> ArgValues(ArgLocs.size());
  assert(ArgLocs.size() == Ins.size() &&
         "Unexpected argument assignment mismatch");
  EVT PtrVT = getPointerTy(DAG.getDataLayout());

  for (unsigned I = 0, E = ArgLocs.size(); I != E;) {
    const CCValAssign &VA = ArgLocs[I];
    assert(isSupportedLocInfo(VA.getLocInfo()) &&
           "Unsupported calling convention assignment");

    auto HandleSplitI64 = [&](unsigned Index) -> bool {
      if (Ins[Index].ArgVT != MVT::i64 || Ins[Index].VT != MVT::i32)
        return false;
      if (Index + 1 >= ArgLocs.size())
        return false;
      if (Ins[Index + 1].ArgVT != MVT::i64 || Ins[Index + 1].VT != MVT::i32)
        return false;
      if (Ins[Index].OrigArgIndex != Ins[Index + 1].OrigArgIndex)
        return false;

      const CCValAssign &LoVA = ArgLocs[Index];
      const CCValAssign &HiVA = ArgLocs[Index + 1];
      if (!LoVA.isRegLoc())
        return false;

      Register LoPhys = LoVA.getLocReg();
      Register HiPhys = isVarArg ? getNextArgReg(LoPhys)
                                 : getShadowFor64Bit(LoPhys);
      if (!HiPhys)
        return false;

      if (!EntryMBB.isLiveIn(LoPhys))
        EntryMBB.addLiveIn(LoPhys);
      if (!EntryMBB.isLiveIn(HiPhys))
        EntryMBB.addLiveIn(HiPhys);

      Register LoVReg = RegInfo.createVirtualRegister(&SLOW32::GPRRegClass);
      Register HiVReg = RegInfo.createVirtualRegister(&SLOW32::GPRRegClass);
      RegInfo.addLiveIn(LoPhys, LoVReg);
      RegInfo.addLiveIn(HiPhys, HiVReg);

      SDValue LoCopy = DAG.getCopyFromReg(Chain, dl, LoVReg, MVT::i32);
      Chains.push_back(LoCopy.getValue(1));
      SDValue HiCopy = DAG.getCopyFromReg(Chain, dl, HiVReg, MVT::i32);
      Chains.push_back(HiCopy.getValue(1));

      ArgValues[Index] = convertLocToValVT(DAG, LoCopy, LoVA, dl);
      CCValAssign HiAsReg = CCValAssign::getReg(HiVA.getValNo(), HiVA.getValVT(),
                                               HiPhys, MVT::i32,
                                               HiVA.getLocInfo());
      ArgValues[Index + 1] = convertLocToValVT(DAG, HiCopy, HiAsReg, dl);
      return true;
    };

    if (HandleSplitI64(I)) {
      I += 2;
      continue;
    }

    SDValue ArgValue;
    if (VA.isRegLoc()) {
      Register PhysReg = VA.getLocReg();
      if (!EntryMBB.isLiveIn(PhysReg))
        EntryMBB.addLiveIn(PhysReg);
      if (VA.getLocVT() == MVT::f64) {
        Register HiPhys = isVarArg ? getNextArgReg(PhysReg)
                                   : getShadowFor64Bit(PhysReg);
        if (!HiPhys)
          report_fatal_error("Missing shadow register for f64 argument");
        if (!EntryMBB.isLiveIn(HiPhys))
          EntryMBB.addLiveIn(HiPhys);

        Register LoVReg = RegInfo.createVirtualRegister(&SLOW32::GPRRegClass);
        Register HiVReg = RegInfo.createVirtualRegister(&SLOW32::GPRRegClass);
        RegInfo.addLiveIn(PhysReg, LoVReg);
        RegInfo.addLiveIn(HiPhys, HiVReg);

        SDValue LoCopy = DAG.getCopyFromReg(Chain, dl, LoVReg, MVT::i32);
        Chains.push_back(LoCopy.getValue(1));
        SDValue HiCopy = DAG.getCopyFromReg(Chain, dl, HiVReg, MVT::i32);
        Chains.push_back(HiCopy.getValue(1));

        ArgValue = DAG.getNode(SLOW32ISD::BuildPairF64, dl, MVT::f64,
                               LoCopy, HiCopy);
      } else {
        const TargetRegisterClass *RC = getRegClassFor(VA.getLocVT());
        Register VReg = RegInfo.createVirtualRegister(RC);
        RegInfo.addLiveIn(PhysReg, VReg);
        SDValue ValIn = DAG.getCopyFromReg(Chain, dl, VReg, VA.getLocVT());
        Chains.push_back(ValIn.getValue(1));
        ArgValue = ValIn;
      }
    } else {
      assert(VA.isMemLoc() && "Expected register or memory location");
      unsigned Bytes = VA.getLocVT().getStoreSize();
      int FI = MFI.CreateFixedObject(Bytes, VA.getLocMemOffset(), true);
      SDValue FIN = DAG.getFrameIndex(FI, PtrVT);
      SDValue Load = DAG.getLoad(VA.getLocVT(), dl, Chain, FIN,
                                 MachinePointerInfo::getFixedStack(MF, FI));
      Chains.push_back(Load.getValue(1));
      ArgValue = Load;
    }

    ArgValues[I] = convertLocToValVT(DAG, ArgValue, VA, dl);
    ++I;
  }

  if (!Chains.empty())
    Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, Chains);

  for (unsigned I = 0, E = Ins.size(); I != E; ++I)
    InVals.push_back(ArgValues[I]);

  if (isVarArg) {
    unsigned FirstVAReg = CCInfo.getFirstUnallocated(SLOW32ArgRegs);
    // For varargs, save R3-R7 (indices 0-4). R8-R10 are excluded:
    // - Caller CC uses R3-R7 for i32/f32, and R4:R5, R6:R7 for i64/f64 pairs
    // - Callee saves exactly the regs caller can use, avoiding garbage gaps
    // - This limits varargs to 4 slots after R3 (format string), or 2 double pairs
    const unsigned NumVarArgRegs = 5;
    unsigned NumRegsLeft = FirstVAReg < NumVarArgRegs ? NumVarArgRegs - FirstVAReg : 0;

    int VarArgsFrameIndex = 0;
    int VarArgsSaveSize = static_cast<int>(NumRegsLeft * 4);
    SmallVector<SDValue, 8> VarArgChains;

    if (NumRegsLeft) {
      VarArgsFrameIndex =
          MFI.CreateFixedObject(VarArgsSaveSize, -VarArgsSaveSize, true);
      SDValue FIN = DAG.getFrameIndex(VarArgsFrameIndex, PtrVT);

      SmallVector<unsigned, 8> PendingRegs;
      for (unsigned RegIdx = FirstVAReg; RegIdx < NumVarArgRegs; ++RegIdx)
        PendingRegs.push_back(RegIdx);

      for (unsigned Slot = 0; Slot < PendingRegs.size(); ++Slot) {
        unsigned RegIdx = PendingRegs[PendingRegs.size() - 1 - Slot];
        Register PhysReg = SLOW32ArgRegs[RegIdx];
        Register VReg = RegInfo.createVirtualRegister(&SLOW32::GPRRegClass);
        RegInfo.addLiveIn(PhysReg, VReg);

        SDValue Copy = DAG.getCopyFromReg(Chain, dl, VReg, MVT::i32);
        SDValue CopyChain = Copy.getValue(1);

        unsigned Offset = (RegIdx - FirstVAReg) * 4;
        SDValue Addr = FIN;
        if (Offset)
          Addr = DAG.getNode(ISD::ADD, dl, PtrVT, FIN,
                             DAG.getConstant(Offset, dl, PtrVT));

        SDValue Store = DAG.getStore(
            CopyChain, dl, Copy, Addr,
            MachinePointerInfo::getFixedStack(MF, VarArgsFrameIndex, Offset));
        VarArgChains.push_back(Store);
      }
    } else {
      // No registers left; first vararg lives at the first stack slot
      // allocated for arguments beyond the fixed prototype.
      VarArgsFrameIndex =
          MFI.CreateFixedObject(4, CCInfo.getStackSize(), true);
    }

    if (!VarArgChains.empty()) {
      VarArgChains.push_back(Chain);
      Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, VarArgChains);
    }

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
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();

  SmallVector<CCValAssign, 16> RVLocs;
  CCState CCInfo(CC, IsVarArg, MF, RVLocs, *DAG.getContext());
  CCInfo.AnalyzeReturn(Outs, RetCC_SLOW32);

  SDValue Glue;
  SmallVector<SDValue, 4> MemChains;
  EVT PtrVT = getPointerTy(DAG.getDataLayout());

  assert(RVLocs.size() == OutVals.size() &&
         "Return value assignment mismatch");

  for (unsigned I = 0, E = RVLocs.size(); I != E;) {
    const CCValAssign &VA = RVLocs[I];
    assert(isSupportedLocInfo(VA.getLocInfo()) &&
           "Unsupported return assignment");

    auto HandleSplitI64 = [&](unsigned Index) -> bool {
      if (Outs[Index].ArgVT != MVT::i64 || Outs[Index].VT != MVT::i32)
        return false;
      if (Index + 1 >= RVLocs.size())
        return false;
      if (Outs[Index + 1].ArgVT != MVT::i64 || Outs[Index + 1].VT != MVT::i32)
        return false;
      if (Outs[Index].OrigArgIndex != Outs[Index + 1].OrigArgIndex)
        return false;

      const CCValAssign &LoVA = RVLocs[Index];
      const CCValAssign &HiVA = RVLocs[Index + 1];
      SDValue LoVal = convertValToLocVT(DAG, OutVals[Index], LoVA, DL);
      SDValue HiVal = convertValToLocVT(DAG, OutVals[Index + 1], HiVA, DL);

      if (LoVA.isRegLoc()) {
        Register LoReg = LoVA.getLocReg();
        Register HiReg = getShadowFor64Bit(LoReg);
        if (HiReg) {
          Chain = DAG.getCopyToReg(Chain, DL, LoReg, LoVal, Glue);
          Glue = Chain.getValue(1);
          Chain = DAG.getCopyToReg(Chain, DL, HiReg, HiVal, Glue);
          Glue = Chain.getValue(1);
          return true;
        }
      }

      if (LoVA.isMemLoc()) {
        // Both halves spill to memory; store them sequentially.
        unsigned Bytes = LoVA.getLocVT().getStoreSize();
        int LoFI = MFI.CreateFixedObject(Bytes, LoVA.getLocMemOffset(), false);
        SDValue LoFIN = DAG.getFrameIndex(LoFI, PtrVT);
        SDValue LoStore = DAG.getStore(
            Chain, DL, LoVal, LoFIN,
            MachinePointerInfo::getFixedStack(DAG.getMachineFunction(), LoFI));
        MemChains.push_back(LoStore);

        unsigned HiBytes = HiVA.getLocVT().getStoreSize();
        int HiFI = MFI.CreateFixedObject(HiBytes, HiVA.getLocMemOffset(), false);
        SDValue HiFIN = DAG.getFrameIndex(HiFI, PtrVT);
        SDValue HiStore = DAG.getStore(
            Chain, DL, HiVal, HiFIN,
            MachinePointerInfo::getFixedStack(DAG.getMachineFunction(), HiFI));
        MemChains.push_back(HiStore);
        return true;
      }

      return false;
    };

    if (HandleSplitI64(I)) {
      I += 2;
      continue;
    }

    SDValue Val = convertValToLocVT(DAG, OutVals[I], VA, DL);

    if (VA.isRegLoc() && VA.getLocVT() == MVT::f64) {
      Register Reg = VA.getLocReg();
      Register Shadow = getShadowFor64Bit(Reg);
      assert(Shadow && "Unexpected register assignment for f64 return");

      SDValue Split = DAG.getNode(SLOW32ISD::SplitF64, DL,
                                  DAG.getVTList(MVT::i32, MVT::i32), Val);
      SDValue Lo = Split.getValue(0);
      SDValue Hi = Split.getValue(1);

      Chain = DAG.getCopyToReg(Chain, DL, Reg, Lo, Glue);
      Glue = Chain.getValue(1);
      Chain = DAG.getCopyToReg(Chain, DL, Shadow, Hi, Glue);
      Glue = Chain.getValue(1);
      ++I;
      continue;
    }

    if (VA.isRegLoc()) {
      Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), Val, Glue);
      Glue = Chain.getValue(1);
    } else {
      unsigned Bytes = VA.getLocVT().getStoreSize();
      int FI = MFI.CreateFixedObject(Bytes, VA.getLocMemOffset(), false);
      SDValue FIN = DAG.getFrameIndex(FI, PtrVT);
      SDValue Store = DAG.getStore(
          Chain, DL, Val, FIN,
          MachinePointerInfo::getFixedStack(DAG.getMachineFunction(), FI));
      MemChains.push_back(Store);
    }

    ++I;
  }

  if (!MemChains.empty()) {
    MemChains.push_back(Chain);
    Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other, MemChains);
  }

  if (RVLocs.empty()) {
    // RET uses r1 implicitly; keep it defined even for void functions.
    Chain = DAG.getCopyToReg(Chain, DL, SLOW32::R1, DAG.getUNDEF(MVT::i32), Glue);
    Glue = Chain.getValue(1);
  }

  return DAG.getNode(SLOW32ISD::RET_FLAG, DL, MVT::Other, Chain, Glue);
}

SDValue SLOW32TargetLowering::LowerCall(TargetLowering::CallLoweringInfo &CLI,
                                        SmallVectorImpl<SDValue> &InVals) const {
  SelectionDAG &DAG = CLI.DAG;
  const SDLoc &DL = CLI.DL;
  SDValue Chain = CLI.Chain;
  SDValue Callee = CLI.Callee;
  CallingConv::ID CallConv = CLI.CallConv;
  bool IsVarArg = CLI.IsVarArg;

  if (CLI.IsTailCall)
    CLI.IsTailCall = false;

  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  EVT PtrVT = getPointerTy(DAG.getDataLayout());

  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, IsVarArg, MF, ArgLocs, *DAG.getContext());
  CCInfo.AnalyzeCallOperands(CLI.Outs,
                             IsVarArg ? CC_SLOW32_VarArg : CC_SLOW32);

  unsigned StackSize = CCInfo.getStackSize();

  SmallVector<SDValue, 8> MemOps;
  SmallVector<SDValue, 8> ByValArgs(CLI.Outs.size(), SDValue());

  for (unsigned I = 0, E = CLI.Outs.size(); I != E; ++I) {
    const ISD::OutputArg &Out = CLI.Outs[I];
    if (!Out.Flags.isByVal() || Out.Flags.getByValSize() == 0)
      continue;

    unsigned Size = Out.Flags.getByValSize();
    Align Alignment = std::max(Align(4), Out.Flags.getNonZeroByValAlign());
    int FI = MFI.CreateStackObject(Size, Alignment, false);
    SDValue FIN = DAG.getFrameIndex(FI, PtrVT);
    SDValue SizeVal = DAG.getConstant(Size, DL, PtrVT);

    SDValue Copy = DAG.getMemcpy(Chain, DL, FIN, CLI.OutVals[I], SizeVal,
                                 Alignment, /*IsVolatile=*/false,
                                 /*AlwaysInline=*/false, nullptr, std::nullopt,
                                 MachinePointerInfo(), MachinePointerInfo());
    MemOps.push_back(Copy);
    ByValArgs[I] = FIN;
  }

  if (!MemOps.empty()) {
    MemOps.push_back(Chain);
    Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other, MemOps);
  }

  Chain = DAG.getCALLSEQ_START(Chain, StackSize, 0, DL);

  SmallVector<std::pair<unsigned, SDValue>, 8> RegsToPass;
  SmallVector<SDValue, 8> StackStores;
  SDValue StackPtr;

  for (unsigned I = 0, E = ArgLocs.size(); I != E;) {
    const CCValAssign &VA = ArgLocs[I];
    const ISD::OutputArg &Out = CLI.Outs[VA.getValNo()];

    auto HandleSplitI64 = [&](unsigned Index) -> bool {
      const CCValAssign &LoVA = ArgLocs[Index];
      if (!LoVA.isRegLoc())
        return false;
      if (Index + 1 >= ArgLocs.size())
        return false;
      const CCValAssign &HiVA = ArgLocs[Index + 1];
      const ISD::OutputArg &LoOut = CLI.Outs[LoVA.getValNo()];
      const ISD::OutputArg &HiOut = CLI.Outs[HiVA.getValNo()];
      if (LoOut.ArgVT != MVT::i64 || LoOut.VT != MVT::i32)
        return false;
      if (HiOut.ArgVT != MVT::i64 || HiOut.VT != MVT::i32)
        return false;
      if (LoOut.OrigArgIndex != HiOut.OrigArgIndex)
        return false;
      Register LoReg = LoVA.getLocReg();
      Register HiReg = IsVarArg ? getNextArgReg(LoReg)
                                : getShadowFor64Bit(LoReg);
      if (!HiReg)
        return false;

      SDValue LoArg = CLI.OutVals[LoVA.getValNo()];
      if (LoOut.Flags.isByVal() && LoOut.Flags.getByValSize())
        LoArg = ByValArgs[LoVA.getValNo()];
      SDValue HiArg = CLI.OutVals[HiVA.getValNo()];
      if (HiOut.Flags.isByVal() && HiOut.Flags.getByValSize())
        HiArg = ByValArgs[HiVA.getValNo()];

      SDValue LoVal = convertValToLocVT(DAG, LoArg, LoVA, DL);
      SDValue HiVal = convertValToLocVT(DAG, HiArg, HiVA, DL);

      RegsToPass.emplace_back(LoReg, LoVal);
      RegsToPass.emplace_back(HiReg, HiVal);
      return true;
    };

    if (HandleSplitI64(I)) {
      I += 2;
      continue;
    }

    SDValue Arg = CLI.OutVals[VA.getValNo()];
    if (Out.Flags.isByVal() && Out.Flags.getByValSize())
      Arg = ByValArgs[VA.getValNo()];

    Arg = convertValToLocVT(DAG, Arg, VA, DL);

    if (VA.isRegLoc()) {
      if (VA.getLocVT() == MVT::f64) {
        Register LoReg = VA.getLocReg();
        Register HiReg = IsVarArg ? getNextArgReg(LoReg)
                                  : getShadowFor64Bit(LoReg);
        if (!HiReg)
          report_fatal_error("Missing shadow register for f64 argument");

        SDValue Split = DAG.getNode(SLOW32ISD::SplitF64, DL,
                                    DAG.getVTList(MVT::i32, MVT::i32), Arg);
        RegsToPass.emplace_back(LoReg, Split.getValue(0));
        RegsToPass.emplace_back(HiReg, Split.getValue(1));
        ++I;
        continue;
      }
      RegsToPass.emplace_back(VA.getLocReg(), Arg);
    } else {
      if (!StackPtr.getNode()) {
        SDValue Copy = DAG.getCopyFromReg(Chain, DL, SLOW32::R29, PtrVT);
        StackPtr = Copy;
        Chain = Copy.getValue(1);
      }
      SDValue Offset = DAG.getConstant(VA.getLocMemOffset(), DL, PtrVT);
      SDValue Ptr = DAG.getNode(ISD::ADD, DL, PtrVT, StackPtr, Offset);
      SDValue Store = DAG.getStore(Chain, DL, Arg, Ptr, MachinePointerInfo());
      StackStores.push_back(Store);
    }

    ++I;
  }

  if (!StackStores.empty()) {
    StackStores.push_back(Chain);
    Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other, StackStores);
  }

  SDValue InGlue;
  for (auto &[PhysReg, Val] : RegsToPass) {
    Chain = DAG.getCopyToReg(Chain, DL, PhysReg, Val, InGlue);
    InGlue = Chain.getValue(1);
  }

  if (auto *G = dyn_cast<GlobalAddressSDNode>(Callee)) {
    Callee = DAG.getTargetGlobalAddress(G->getGlobal(), DL, PtrVT,
                                        G->getOffset(), G->getTargetFlags());
  } else if (auto *ES = dyn_cast<ExternalSymbolSDNode>(Callee)) {
    Callee = DAG.getTargetExternalSymbol(ES->getSymbol(), PtrVT,
                                         ES->getTargetFlags());
  }

  SmallVector<SDValue, 8> Ops;
  Ops.push_back(Chain);
  Ops.push_back(Callee);

  const TargetRegisterInfo *TRI = DAG.getSubtarget().getRegisterInfo();
  const uint32_t *Mask = TRI->getCallPreservedMask(MF, CallConv);
  Ops.push_back(DAG.getRegisterMask(Mask));

  if (InGlue.getNode())
    Ops.push_back(InGlue);

  SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
  Chain = DAG.getNode(SLOW32ISD::CALL, DL, NodeTys, Ops);
  SDValue Glue = Chain.getValue(1);

  Chain = DAG.getCALLSEQ_END(Chain, StackSize, 0, Glue, DL);
  Glue = Chain.getValue(1);

  SmallVector<CCValAssign, 16> RVLocs;
  CCState RetCC(CallConv, IsVarArg, MF, RVLocs, *DAG.getContext());
  RetCC.AnalyzeCallResult(CLI.Ins, RetCC_SLOW32);

  SDValue StackReload;
  for (unsigned I = 0, E = RVLocs.size(); I != E;) {
    const CCValAssign &VA = RVLocs[I];
    assert(isSupportedLocInfo(VA.getLocInfo()) &&
           "Unsupported returned value assignment");

    auto HandleSplitI64 = [&](unsigned Index) -> bool {
      const CCValAssign &LoVA = RVLocs[Index];
      if (!LoVA.isRegLoc())
        return false;
      if (Index + 1 >= RVLocs.size())
        return false;
      const CCValAssign &HiVA = RVLocs[Index + 1];
      const ISD::InputArg &LoIn = CLI.Ins[LoVA.getValNo()];
      const ISD::InputArg &HiIn = CLI.Ins[HiVA.getValNo()];
      if (LoIn.ArgVT != MVT::i64 || LoIn.VT != MVT::i32)
        return false;
      if (HiIn.ArgVT != MVT::i64 || HiIn.VT != MVT::i32)
        return false;
      if (LoIn.OrigArgIndex != HiIn.OrigArgIndex)
        return false;

      Register LoReg = LoVA.getLocReg();
      Register HiReg = getShadowFor64Bit(LoReg);
      if (!HiReg)
        return false;

      SDValue Lo = DAG.getCopyFromReg(Chain, DL, LoReg, MVT::i32, Glue);
      Chain = Lo.getValue(1);
      Glue = Lo.getValue(2);
      SDValue Hi = DAG.getCopyFromReg(Chain, DL, HiReg, MVT::i32, Glue);
      Chain = Hi.getValue(1);
      Glue = Hi.getValue(2);

      InVals.push_back(convertLocToValVT(DAG, Lo, LoVA, DL));
      CCValAssign HiAsReg = CCValAssign::getReg(HiVA.getValNo(), HiVA.getValVT(),
                                               HiReg, MVT::i32,
                                               HiVA.getLocInfo());
      InVals.push_back(convertLocToValVT(DAG, Hi, HiAsReg, DL));
      return true;
    };

    if (HandleSplitI64(I)) {
      I += 2;
      continue;
    }

    if (VA.isRegLoc()) {
      if (VA.getLocVT() == MVT::f64) {
        Register LoReg = VA.getLocReg();
        Register HiReg = getShadowFor64Bit(LoReg);
        if (!HiReg)
          report_fatal_error("Missing shadow register for f64 return");

        SDValue Lo = DAG.getCopyFromReg(Chain, DL, LoReg, MVT::i32, Glue);
        Chain = Lo.getValue(1);
        Glue = Lo.getValue(2);
        SDValue Hi = DAG.getCopyFromReg(Chain, DL, HiReg, MVT::i32, Glue);
        Chain = Hi.getValue(1);
        Glue = Hi.getValue(2);

        InVals.push_back(DAG.getNode(SLOW32ISD::BuildPairF64, DL, MVT::f64,
                                     Lo, Hi));
        ++I;
        continue;
      }
      SDValue Val = DAG.getCopyFromReg(Chain, DL, VA.getLocReg(),
                                       VA.getLocVT(), Glue);
      Chain = Val.getValue(1);
      Glue = Val.getValue(2);
      InVals.push_back(convertLocToValVT(DAG, Val, VA, DL));
    } else {
      if (!StackReload.getNode()) {
        SDValue Copy = DAG.getCopyFromReg(Chain, DL, SLOW32::R29, PtrVT);
        StackReload = Copy;
        Chain = Copy.getValue(1);
      }
      SDValue Offset = DAG.getConstant(VA.getLocMemOffset(), DL, PtrVT);
      SDValue Ptr = DAG.getNode(ISD::ADD, DL, PtrVT, StackReload, Offset);
      SDValue Load = DAG.getLoad(VA.getLocVT(), DL, Chain, Ptr,
                                 MachinePointerInfo());
      Chain = Load.getValue(1);
      InVals.push_back(convertLocToValVT(DAG, Load, VA, DL));
    }

    ++I;
  }

  return Chain;
}

EVT SLOW32TargetLowering::getOptimalMemOpType(
    LLVMContext &Context, const MemOp &Op,
    const AttributeList &FuncAttributes) const {
  // At -O0, keep it simple and correct with byte operations
  if (getTargetMachine().getOptLevel() == CodeGenOptLevel::None)
    return MVT::i8;
  
  // Keep the lowering conservative when the destination alignment is not
  // fixed. SelectionDAG sets DstAlignCanChange for stack slots and similar
  // cases, and calling getDstAlign() in that scenario trips an assertion.
  unsigned DstAlign = Op.isFixedDstAlign() ? Op.getDstAlign().value() : 1;
  unsigned MinAlign = DstAlign;

  // For memcpy, also consider the source alignment.
  if (Op.isMemcpy()) {
    unsigned SrcAlign = Op.getSrcAlign().value();
    MinAlign = std::min(MinAlign, SrcAlign);
  }

  if (MinAlign < 4 || Op.size() < 4)
    return MVT::i8;
  
  // For aligned cases at O1/O2, use efficient word operations
  return MVT::i32;
}

bool SLOW32TargetLowering::isLegalAddressingMode(const DataLayout &DL,
                                                 const AddrMode &AM,
                                                 Type *Ty, unsigned AS,
                                                 Instruction *I) const {
  // SLOW32 supports [base + simm12] addressing mode
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
  return Op;
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
    case ISD::FP_TO_SINT:
    case ISD::FP_TO_UINT: {
      // fp → i64: hardware convert to GPRPair, then split into i32 halves
      SDValue Src = N->getOperand(0);
      unsigned Opc = (N->getOpcode() == ISD::FP_TO_SINT) ? SLOW32ISD::FCVT_L
                                                          : SLOW32ISD::FCVT_LU;
      SDValue Pair = DAG.getNode(Opc, DL, MVT::f64, Src);
      SDValue Split = DAG.getNode(SLOW32ISD::SplitF64, DL,
                                  DAG.getVTList(MVT::i32, MVT::i32), Pair);
      SDValue Lo = SDValue(Split.getNode(), 0);
      SDValue Hi = SDValue(Split.getNode(), 1);
      SDValue Result = DAG.getNode(ISD::BUILD_PAIR, DL, MVT::i64, Lo, Hi);
      Results.push_back(Result);
      return;
    }
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
    // SELECT_PSEUDO expands to a triangle pattern with a PHI:
    //
    //   HeadMBB:
    //     bne $cond, r0, TailMBB   // if true, branch to TailMBB
    //   IfFalseMBB:                 // fallthrough (condition is false)
    //   TailMBB:
    //     $dst = PHI [$t, HeadMBB], [$f, IfFalseMBB]

    MachineFunction *MF = BB->getParent();
    const BasicBlock *LLVM_BB = BB->getBasicBlock();

    MachineBasicBlock *HeadMBB = BB;
    MachineBasicBlock *IfFalseMBB = MF->CreateMachineBasicBlock(LLVM_BB);
    MachineBasicBlock *TailMBB = MF->CreateMachineBasicBlock(LLVM_BB);

    MachineFunction::iterator It = ++BB->getIterator();
    MF->insert(It, IfFalseMBB);
    MF->insert(It, TailMBB);

    // Move everything after SELECT_PSEUDO to TailMBB.
    TailMBB->splice(TailMBB->begin(), HeadMBB,
                    std::next(MachineBasicBlock::iterator(MI)),
                    HeadMBB->end());
    TailMBB->transferSuccessorsAndUpdatePHIs(HeadMBB);

    // Set up CFG edges.
    HeadMBB->addSuccessor(IfFalseMBB);
    HeadMBB->addSuccessor(TailMBB);
    IfFalseMBB->addSuccessor(TailMBB);

    // HeadMBB: bne $cond, r0, TailMBB  (if true, skip to TailMBB)
    BuildMI(HeadMBB, DL, TII.get(SLOW32::BNE))
        .addReg(MI.getOperand(1).getReg())  // $cond
        .addReg(SLOW32::R0)
        .addMBB(TailMBB);

    // IfFalseMBB is empty — just falls through to TailMBB.

    // Propagate live-ins from HeadMBB to the new blocks.  Physical
    // registers like the stack pointer (R29) must be listed as live-in
    // in every block that reaches a use.
    for (const auto &LI : HeadMBB->liveins()) {
      IfFalseMBB->addLiveIn(LI);
      TailMBB->addLiveIn(LI);
    }

    // TailMBB: PHI to merge true/false values.
    BuildMI(*TailMBB, TailMBB->begin(), DL, TII.get(SLOW32::PHI),
            MI.getOperand(0).getReg())
        .addReg(MI.getOperand(2).getReg())  // $t (from HeadMBB)
        .addMBB(HeadMBB)
        .addReg(MI.getOperand(3).getReg())  // $f (from IfFalseMBB)
        .addMBB(IfFalseMBB);

    MF->getProperties().resetNoPHIs();

    MI.eraseFromParent();
    return TailMBB;
  }
  case SLOW32::BuildPairF64Pseudo: {
    // Combine two i32 halves into a GPRPair (f64) via REG_SEQUENCE.
    Register LoReg = MI.getOperand(1).getReg();
    Register HiReg = MI.getOperand(2).getReg();
    Register DstReg = MI.getOperand(0).getReg();

    BuildMI(*BB, MI, DL, TII.get(TargetOpcode::REG_SEQUENCE), DstReg)
        .addReg(LoReg)
        .addImm(gsub_0)
        .addReg(HiReg)
        .addImm(gsub_1);
    MI.eraseFromParent();
    return BB;
  }
  case SLOW32::SplitF64Pseudo: {
    // Extract two i32 halves from a GPRPair (f64) via COPY with subreg.
    Register SrcReg = MI.getOperand(2).getReg();
    Register LoReg = MI.getOperand(0).getReg();
    Register HiReg = MI.getOperand(1).getReg();

    BuildMI(*BB, MI, DL, TII.get(TargetOpcode::COPY), LoReg)
        .addReg(SrcReg, RegState(), gsub_0);
    BuildMI(*BB, MI, DL, TII.get(TargetOpcode::COPY), HiReg)
        .addReg(SrcReg, RegState(), gsub_1);
    MI.eraseFromParent();
    return BB;
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
  // For i32, allow misaligned access but mark it as slow
  // This lets LLVM use it when necessary but prefer byte ops
  // when guided by getOptimalMemOpType
  if (VT == MVT::i32 && Alignment < Align(4)) {
    if (Fast) 
      *Fast = 0;  // Legal but slow -> prefer byte ops
    return true;    // Emulator/hw can handle if it must
  }
  
  // Conservative for other types
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
SDValue SLOW32TargetLowering::LowerUMUL_LOHI(SDValue Op,
                                             SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue A = Op.getOperand(0);
  SDValue B = Op.getOperand(1);
  EVT VT = MVT::i32;

  SDValue Lo = DAG.getNode(ISD::MUL, DL, VT, A, B);
  SDValue Hi = LowerMULHU(Op, DAG);

  return DAG.getMergeValues({Lo, Hi}, DL);
}

// Lower signed 32x32->64 multiply
SDValue SLOW32TargetLowering::LowerSMUL_LOHI(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue A = Op.getOperand(0);
  SDValue B = Op.getOperand(1);
  EVT VT = MVT::i32;
  
  SDValue Lo = DAG.getNode(ISD::MUL, DL, VT, A, B);
  SDValue Hi = DAG.getNode(ISD::MULHS, DL, VT, A, B);
  
  return DAG.getMergeValues({Lo, Hi}, DL);
}

// Lower unsigned multiply-high (just the high part)
SDValue SLOW32TargetLowering::LowerMULHU(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue A = Op.getOperand(0);
  SDValue B = Op.getOperand(1);
  EVT VT = MVT::i32;

  // MULHU(A, B) = MULHS(A, B) + (A < 0 ? B : 0) + (B < 0 ? A : 0)
  SDValue MulHS = DAG.getNode(ISD::MULHS, DL, VT, A, B);

  SDValue Shift31 = DAG.getConstant(31, DL, VT);

  // Correction for A < 0: (A >> 31) & B
  SDValue MaskA = DAG.getNode(ISD::SRA, DL, VT, A, Shift31);
  SDValue CorrA = DAG.getNode(ISD::AND, DL, VT, MaskA, B);

  // Correction for B < 0: (B >> 31) & A
  SDValue MaskB = DAG.getNode(ISD::SRA, DL, VT, B, Shift31);
  SDValue CorrB = DAG.getNode(ISD::AND, DL, VT, MaskB, A);

  SDValue Hi = DAG.getNode(ISD::ADD, DL, VT, MulHS, CorrA);
  Hi = DAG.getNode(ISD::ADD, DL, VT, Hi, CorrB);

  return Hi;
}

SDValue SLOW32TargetLowering::LowerUDIV(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue L = Op.getOperand(0);
  SDValue R = Op.getOperand(1);

  // Use makeLibCall for consistent attributes and better scheduling
  TargetLowering::MakeLibCallOptions CallOptions;
  CallOptions.setIsSigned(false);  // Unsigned division
  SDValue Chain = DAG.getEntryNode();
  return makeLibCall(DAG, RTLIB::UDIV_I32, MVT::i32, {L, R}, CallOptions, DL, Chain).first;
}

SDValue SLOW32TargetLowering::LowerUREM(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue L = Op.getOperand(0);
  SDValue R = Op.getOperand(1);

  // Use makeLibCall for consistent attributes and better scheduling
  TargetLowering::MakeLibCallOptions CallOptions;
  CallOptions.setIsSigned(false);  // Unsigned remainder
  SDValue Chain = DAG.getEntryNode();
  return makeLibCall(DAG, RTLIB::UREM_I32, MVT::i32, {L, R}, CallOptions, DL, Chain).first;
}
