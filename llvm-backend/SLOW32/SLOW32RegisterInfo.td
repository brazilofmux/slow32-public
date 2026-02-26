// SLOW32 Register definitions
// No include needed here - Target.td is already included in SLOW32.td

class GPR<string n, bits<5> num, list<string> alt = []> : Register<n> { 
  let Namespace = "SLOW32";
  let AltNames = alt;
  let HWEncoding{4-0} = num;
  let HWEncoding{15-5} = 0;
}

// Sub-register indices for register pairs.
def gsub_0 : SubRegIndex<32>;
def gsub_1 : SubRegIndex<32, 32>;

// Define all 32 registers (ABI aliases in AltNames)
def R0  : GPR<"r0",  0, ["zero"]>, DwarfRegNum<[0]>;
def R1  : GPR<"r1",  1, ["rv"]>,  DwarfRegNum<[1]>;
def R2  : GPR<"r2",  2, ["t0"]>,  DwarfRegNum<[2]>;
def R3  : GPR<"r3",  3, ["a0"]>,  DwarfRegNum<[3]>;
def R4  : GPR<"r4",  4, ["a1"]>,  DwarfRegNum<[4]>;
def R5  : GPR<"r5",  5, ["a2"]>,  DwarfRegNum<[5]>;
def R6  : GPR<"r6",  6, ["a3"]>,  DwarfRegNum<[6]>;
def R7  : GPR<"r7",  7, ["a4"]>,  DwarfRegNum<[7]>;
def R8  : GPR<"r8",  8, ["a5"]>,  DwarfRegNum<[8]>;
def R9  : GPR<"r9",  9, ["a6"]>,  DwarfRegNum<[9]>;
def R10 : GPR<"r10", 10, ["a7"]>, DwarfRegNum<[10]>;
def R11 : GPR<"r11", 11, ["s0"]>, DwarfRegNum<[11]>;
def R12 : GPR<"r12", 12, ["s1"]>, DwarfRegNum<[12]>;
def R13 : GPR<"r13", 13, ["s2"]>, DwarfRegNum<[13]>;
def R14 : GPR<"r14", 14, ["s3"]>, DwarfRegNum<[14]>;
def R15 : GPR<"r15", 15, ["s4"]>, DwarfRegNum<[15]>;
def R16 : GPR<"r16", 16, ["s5"]>, DwarfRegNum<[16]>;
def R17 : GPR<"r17", 17, ["s6"]>, DwarfRegNum<[17]>;
def R18 : GPR<"r18", 18, ["s7"]>, DwarfRegNum<[18]>;
def R19 : GPR<"r19", 19, ["s8"]>, DwarfRegNum<[19]>;
def R20 : GPR<"r20", 20, ["s9"]>, DwarfRegNum<[20]>;
def R21 : GPR<"r21", 21, ["s10"]>, DwarfRegNum<[21]>;
def R22 : GPR<"r22", 22, ["s11"]>, DwarfRegNum<[22]>;
def R23 : GPR<"r23", 23, ["s12"]>, DwarfRegNum<[23]>;
def R24 : GPR<"r24", 24, ["s13"]>, DwarfRegNum<[24]>;
def R25 : GPR<"r25", 25, ["s14"]>, DwarfRegNum<[25]>;
def R26 : GPR<"r26", 26, ["s15"]>, DwarfRegNum<[26]>;
def R27 : GPR<"r27", 27, ["s16"]>, DwarfRegNum<[27]>;
def R28 : GPR<"r28", 28, ["s17"]>, DwarfRegNum<[28]>;
def R29 : GPR<"sp", 29>,           DwarfRegNum<[29]>;
def R30 : GPR<"fp", 30>,           DwarfRegNum<[30]>;
def R31 : GPR<"lr", 31>,           DwarfRegNum<[31]>;

// Define register class
def GPR : RegisterClass<"SLOW32", [i32, f32], 32, (add 
  R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10,
  R11, R12, R13, R14, R15, R16, R17, R18, R19, R20, 
  R21, R22, R23, R24, R25, R26, R27, R28, R29, R30, R31)>;

// Even/odd register pairs for 64-bit values (f64/i64).
def Tuples2GPR : RegisterTuples<[gsub_0, gsub_1],
                                [(add R0, R2, R4, R6, R8, R10, R12, R14,
                                      R16, R18, R20, R22, R24, R26, R28, R30),
                                 (add R1, R3, R5, R7, R9, R11, R13, R15,
                                      R17, R19, R21, R23, R25, R27, R29, R31)]>;

def GPRPair : RegisterClass<"SLOW32", [f64], 64, (add Tuples2GPR)>;
