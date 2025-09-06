// SLOW32 Register definitions
// No include needed here - Target.td is already included in SLOW32.td

class GPR<string n, bits<5> num> : Register<n> { 
  let Namespace = "SLOW32";
  let HWEncoding{4-0} = num;
  let HWEncoding{15-5} = 0;
}

// Define all 32 registers
def R0  : GPR<"r0",  0>;  def R1  : GPR<"r1",  1>;  def R2  : GPR<"r2",  2>;
def R3  : GPR<"r3",  3>;  def R4  : GPR<"r4",  4>;  def R5  : GPR<"r5",  5>;
def R6  : GPR<"r6",  6>;  def R7  : GPR<"r7",  7>;  def R8  : GPR<"r8",  8>;
def R9  : GPR<"r9",  9>;  def R10 : GPR<"r10", 10>; def R11 : GPR<"r11", 11>;
def R12 : GPR<"r12", 12>; def R13 : GPR<"r13", 13>; def R14 : GPR<"r14", 14>;
def R15 : GPR<"r15", 15>; def R16 : GPR<"r16", 16>; def R17 : GPR<"r17", 17>;
def R18 : GPR<"r18", 18>; def R19 : GPR<"r19", 19>; def R20 : GPR<"r20", 20>;
def R21 : GPR<"r21", 21>; def R22 : GPR<"r22", 22>; def R23 : GPR<"r23", 23>;
def R24 : GPR<"r24", 24>; def R25 : GPR<"r25", 25>; def R26 : GPR<"r26", 26>;
def R27 : GPR<"r27", 27>; def R28 : GPR<"r28", 28>; def R29 : GPR<"sp", 29>;
def R30 : GPR<"fp", 30>;  def R31 : GPR<"lr", 31>;

// Register aliases matching SLOW-32 conventions
def ZERO : GPR<"zero", 0>;
def RV   : GPR<"rv", 1>;
def T0   : GPR<"t0", 2>;
def A0   : GPR<"a0", 3>;
def A1   : GPR<"a1", 4>;
def A2   : GPR<"a2", 5>;
def A3   : GPR<"a3", 6>;
def A4   : GPR<"a4", 7>;
def A5   : GPR<"a5", 8>;
def A6   : GPR<"a6", 9>;
def A7   : GPR<"a7", 10>;
def S0   : GPR<"s0", 11>;
def S1   : GPR<"s1", 12>;
def S2   : GPR<"s2", 13>;
def S3   : GPR<"s3", 14>;
def S4   : GPR<"s4", 15>;
def S5   : GPR<"s5", 16>;
def S6   : GPR<"s6", 17>;
def S7   : GPR<"s7", 18>;
def S8   : GPR<"s8", 19>;
def S9   : GPR<"s9", 20>;
def S10  : GPR<"s10", 21>;
def S11  : GPR<"s11", 22>;
def S12  : GPR<"s12", 23>;
def S13  : GPR<"s13", 24>;
def S14  : GPR<"s14", 25>;
def S15  : GPR<"s15", 26>;
def S16  : GPR<"s16", 27>;
def S17  : GPR<"s17", 28>;
def SP   : GPR<"sp", 29>;
def FP   : GPR<"fp", 30>;
def LR   : GPR<"lr", 31>;

// Define register class
def GPR : RegisterClass<"SLOW32", [i32], 32, (add 
  R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10,
  R11, R12, R13, R14, R15, R16, R17, R18, R19, R20, 
  R21, R22, R23, R24, R25, R26, R27, R28, R29, R30, R31)>;