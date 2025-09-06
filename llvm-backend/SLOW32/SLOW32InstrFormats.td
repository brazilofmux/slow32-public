
// No include needed - Target.td is already included in SLOW32.td
class S32Inst<dag outs, dag ins, string asm, list<dag> pattern = [],
              InstrItinClass itin = IIC_ALU>
  : Instruction {
  let Namespace = "SLOW32";
  let OutOperandList = outs;
  let InOperandList = ins;
  let AsmString = asm;
  let Pattern = pattern;
  let Size = 4;
  let Itinerary = itin;
}

// Pseudo instruction class
class Pseudo<dag outs, dag ins, string asm, list<dag> pattern = []>
  : S32Inst<outs, ins, asm, pattern> {
  let isPseudo = 1;
  let isCodeGenOnly = 1;
}
// Type profiles for custom nodes
def SDT_SLOW32Call : SDTypeProfile<0, 1, [SDTCisPtrTy<0>]>;
def SDT_SLOW32HiLo : SDTypeProfile<1, 1, [SDTCisInt<0>, SDTCisPtrTy<1>]>;
// Branch node: takes chain, lhs, rhs, and dest (basic block)
def SDT_SLOW32BrCC : SDTypeProfile<0, 3, [SDTCisVT<0, i32>,
                                          SDTCisVT<1, i32>,
                                          SDTCisVT<2, OtherVT>]>;
