// SLOW32 Register definitions
// No include needed here - Target.td is already included in SLOW32.td

class GPR<string n, bits<5> num, list<string> alt = []> : Register<n> { 
  let Namespace = "SLOW32";
  let AltNames = alt;
  let HWEncoding{4-0} = num;
  let HWEncoding{15-5} = 0;
}

// Define all 32 registers (ABI aliases in AltNames)
def R0  : GPR<"r0",  0, ["zero"]>;
def R1  : GPR<"r1",  1, ["rv"]>;
def R2  : GPR<"r2",  2, ["t0"]>;
def R3  : GPR<"r3",  3, ["a0"]>;
def R4  : GPR<"r4",  4, ["a1"]>;
def R5  : GPR<"r5",  5, ["a2"]>;
def R6  : GPR<"r6",  6, ["a3"]>;
def R7  : GPR<"r7",  7, ["a4"]>;
def R8  : GPR<"r8",  8, ["a5"]>;
def R9  : GPR<"r9",  9, ["a6"]>;
def R10 : GPR<"r10", 10, ["a7"]>;
def R11 : GPR<"r11", 11, ["s0"]>;
def R12 : GPR<"r12", 12, ["s1"]>;
def R13 : GPR<"r13", 13, ["s2"]>;
def R14 : GPR<"r14", 14, ["s3"]>;
def R15 : GPR<"r15", 15, ["s4"]>;
def R16 : GPR<"r16", 16, ["s5"]>;
def R17 : GPR<"r17", 17, ["s6"]>;
def R18 : GPR<"r18", 18, ["s7"]>;
def R19 : GPR<"r19", 19, ["s8"]>;
def R20 : GPR<"r20", 20, ["s9"]>;
def R21 : GPR<"r21", 21, ["s10"]>;
def R22 : GPR<"r22", 22, ["s11"]>;
def R23 : GPR<"r23", 23, ["s12"]>;
def R24 : GPR<"r24", 24, ["s13"]>;
def R25 : GPR<"r25", 25, ["s14"]>;
def R26 : GPR<"r26", 26, ["s15"]>;
def R27 : GPR<"r27", 27, ["s16"]>;
def R28 : GPR<"r28", 28, ["s17"]>;
def R29 : GPR<"sp", 29>;
def R30 : GPR<"fp", 30>;
def R31 : GPR<"lr", 31>;

// Define register class
def GPR : RegisterClass<"SLOW32", [i32], 32, (add 
  R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10,
  R11, R12, R13, R14, R15, R16, R17, R18, R19, R20, 
  R21, R22, R23, R24, R25, R26, R27, R28, R29, R30, R31)>;

