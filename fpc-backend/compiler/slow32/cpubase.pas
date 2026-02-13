{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Contains the base types for SLOW-32

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
{ This Unit contains the base types for SLOW-32 }
unit cpubase;

{$i fpcdefs.inc}

interface

uses
  strings,globtype,
  cutils,cclasses,aasmbase,cpuinfo,cgbase;

{*****************************************************************************
                                Assembler Opcodes
*****************************************************************************}

    type
      TAsmOp=(A_None,
        { Pseudo instructions }
        A_NOP,A_CALL,A_LA,A_LI,A_MV,A_NOT,A_NEG,A_J,A_RET,A_TAIL,

        { R-type ALU }
        A_ADD,A_SUB,A_AND,A_OR,A_XOR,A_SLL,A_SRL,A_SRA,

        { I-type ALU }
        A_ADDI,A_ANDI,A_ORI,A_XORI,A_SLLI,A_SRLI,A_SRAI,

        { Multiply / Divide }
        A_MUL,A_MULH,A_MULHU,A_DIV,A_REM,

        { Load upper immediate }
        A_LUI,

        { Compare (set result in register) }
        A_SLT,A_SLTU,A_SEQ,A_SNE,A_SGT,A_SGTU,A_SGE,A_SGEU,A_SLE,A_SLEU,

        { Load }
        A_LDB,A_LDBU,A_LDH,A_LDHU,A_LDW,

        { Store }
        A_STB,A_STH,A_STW,

        { Branch }
        A_BEQ,A_BNE,A_BLT,A_BGE,A_BGT,A_BLE,A_BLTU,A_BGEU,A_BGTU,A_BLEU,

        { Jump }
        A_JAL,A_JALR
      );

      { This should define the array of instructions as string }
      op2strtable=array[tasmop] of string[8];

    Const
      { First value of opcode enumeration }
      firstop = low(tasmop);
      { Last value of opcode enumeration  }
      lastop  = high(tasmop);

{*****************************************************************************
                                  Registers
*****************************************************************************}

    type
      { Number of registers used for indexing in tables }
      tregisterindex=0..{$i rs32nor.inc}-1;

    const
      maxvarregs = 32-6; { 32 int registers - r0 - stackpointer - r2 - 3 scratch registers }
      maxfpuvarregs = 0; { no separate FPU register file }

      { Available Superregisters }
      {$i rs32sup.inc}

      { No Subregisters }
      R_SUBWHOLE=R_SUBNONE;

      { Available Registers }
      {$i rs32con.inc}

      { Integer Super registers first and last }
      first_int_imreg = $20;

      { Float Super register first and last }
      first_fpu_imreg     = $20;

      { MM Super register first and last }
      first_mm_imreg     = $20;

{ TODO: Calculate bsstart}
      regnumber_count_bsstart = 32;

      regnumber_table : array[tregisterindex] of tregister = (
        {$i rs32num.inc}
      );

      regstabs_table : array[tregisterindex] of shortint = (
        {$i rs32sta.inc}
      );

      regdwarf_table : array[tregisterindex] of shortint = (
        {$i rs32dwa.inc}
      );

{*****************************************************************************
                                Conditions
*****************************************************************************}

    type
      TAsmCond = (C_None { unconditional jumps },
        C_EQ,C_NE,C_LT,C_GE,C_GT,C_LE,C_LTU,C_GEU,C_GTU,C_LEU);

      TAsmConds = set of TAsmCond;

    const
      cond2str: Array[TAsmCond] of string[4] = ({cf_none}'',
        'eq','ne','lt','ge','gt','le','ltu','geu','gtu','leu');

      uppercond2str: Array[TAsmCond] of string[4] = ({cf_none}'',
        'EQ','NE','LT','GE','GT','LE','LTU','GEU','GTU','LEU');

{*****************************************************************************
                                   Flags
*****************************************************************************}

    type
      TResFlagsEnum = (F_EQ,F_NE,F_LT,F_GE,F_GT,F_LE,F_LTU,F_GEU,F_GTU,F_LEU);

{*****************************************************************************
                                Operands
*****************************************************************************}

    { SLOW-32 has no fence flags, no rounding modes, no memory ordering.
      We keep TRoundingMode as a placeholder for potential soft-float needs. }
    type
      TRoundingMode = (RM_Default,
                       RM_RNE,
                       RM_RTZ,
                       RM_RDN,
                       RM_RUP,
                       RM_RMM);

    const
      roundingmode2str : array[TRoundingMode] of string[3] = ('',
        'rne','rtz','rdn','rup','rmm');

{*****************************************************************************
                                 Constants
*****************************************************************************}

    const
      max_operands = 4;

{*****************************************************************************
                            Default generic sizes
*****************************************************************************}

      {# Defines the default address size for a processor }
      OS_ADDR = OS_32;
      {# the natural int size for a processor,
         has to match osuinttype/ossinttype as initialized in psystem }
      OS_INT = OS_32;
      OS_SINT = OS_S32;
      {# the maximum float size for a processor }
      OS_FLOAT = OS_F64;
      {# the size of a vector register for a processor }
      OS_VECTOR = OS_M128;

{*****************************************************************************
                               GDB Information
*****************************************************************************}

      stab_regindex : array[tregisterindex] of shortint = (
        {$i rs32sta.inc}
      );

{*****************************************************************************
                          Generic Register names
*****************************************************************************}

      {# Stack pointer register }
      NR_STACK_POINTER_REG = NR_R29;
      RS_STACK_POINTER_REG = RS_R29;
      {# Frame pointer register }
      NR_FRAME_POINTER_REG = NR_R30;
      RS_FRAME_POINTER_REG = RS_R30;

      NR_PIC_OFFSET_REG = NR_R2;
      { Return address of a function }
      NR_RETURN_ADDRESS_REG = NR_R31;
      RS_RETURN_ADDRESS_REG = RS_R31;
      { Results are returned in this register (32-bit values) }
      NR_FUNCTION_RETURN_REG = NR_R1;
      RS_FUNCTION_RETURN_REG = RS_R1;
      { Low part of 64bit return value }
      NR_FUNCTION_RETURN64_LOW_REG = NR_R1;
      RS_FUNCTION_RETURN64_LOW_REG = RS_R1;
      { High part of 64bit return value }
      NR_FUNCTION_RETURN64_HIGH_REG = NR_R2;
      RS_FUNCTION_RETURN64_HIGH_REG = RS_R2;
      { The value returned from a function is available in this register }
      NR_FUNCTION_RESULT_REG = NR_FUNCTION_RETURN_REG;
      RS_FUNCTION_RESULT_REG = RS_FUNCTION_RETURN_REG;
      { The low part of 64bit value returned from a function }
      NR_FUNCTION_RESULT64_LOW_REG = NR_FUNCTION_RETURN64_LOW_REG;
      RS_FUNCTION_RESULT64_LOW_REG = RS_FUNCTION_RETURN64_LOW_REG;
      { The high part of 64bit value returned from a function }
      NR_FUNCTION_RESULT64_HIGH_REG = NR_FUNCTION_RETURN64_HIGH_REG;
      RS_FUNCTION_RESULT64_HIGH_REG = RS_FUNCTION_RETURN64_HIGH_REG;

      NR_FPU_RESULT_REG = NR_NO;
      NR_MM_RESULT_REG = NR_NO;

      NR_DEFAULTFLAGS = NR_NO;
      RS_DEFAULTFLAGS = RS_NO;

{*****************************************************************************
                       GCC /ABI linking information
*****************************************************************************}

      {# Registers which must be saved when calling a routine declared as
         cppdecl, cdecl, stdcall, safecall, palmossyscall. The registers
         saved should be the ones as defined in the target ABI and / or GCC.

         SLOW-32 callee-saved: r29(sp), r30(fp), r31(lr), r11-r28(s0-s17)
      }
      saved_standard_registers : array[0..20] of tsuperregister = (
        RS_R29,
        RS_R30,RS_R31,
        RS_R11,RS_R12,RS_R13,RS_R14,RS_R15,RS_R16,RS_R17,RS_R18,
        RS_R19,RS_R20,RS_R21,RS_R22,RS_R23,RS_R24,RS_R25,RS_R26,
        RS_R27,RS_R28
      );

      { this is only for the generic code which is not used for this architecture }
      saved_address_registers : array[0..0] of tsuperregister = (RS_INVALID);
      saved_mm_registers : array[0..0] of tsuperregister = (RS_INVALID);

      {# Required parameter alignment when calling a routine declared as
         stdcall and cdecl. The alignment value should be the one defined
         by GCC or the target ABI.

         The value of this constant is equal to the constant
         PARM_BOUNDARY / BITS_PER_UNIT in the GCC source.
      }
      std_param_align = 4;

{*****************************************************************************
                            CPU Dependent Constants
*****************************************************************************}

      maxfpuregs = 0;

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function is_imm12(value: tcgint): boolean;

    function is_calljmp(o:tasmop):boolean;

    function cgsize2subreg(regtype: tregistertype; s:Tcgsize):Tsubregister;
    { Returns the tcgsize corresponding with the size of reg.}
    function reg_cgsize(const reg: tregister) : tcgsize;

    function findreg_by_number(r:Tregister):tregisterindex;
    function std_regnum_search(const s:string):Tregister;
    function std_regname(r:Tregister):string;

    function inverse_cond(const c: TAsmCond): Tasmcond; {$ifdef USEINLINE}inline;{$endif USEINLINE}
    function dwarf_reg(r:tregister):shortint;
    function dwarf_reg_no_error(r:tregister):shortint;
    function eh_return_data_regno(nr: longint): longint;

    function conditions_equal(const c1,c2: TAsmCond): boolean;

    { Checks if Subset is a subset of c (e.g. "less than" is a subset of "less than or equal" }
    function condition_in(const Subset, c: TAsmCond): Boolean;

    function is_extra_reg(const s : string) : tregister;

implementation

    uses
      rgbase,verbose;

    const
      std_regname_table : TRegNameTable = (
        {$i rs32std.inc}
      );

      regnumber_index : array[tregisterindex] of tregisterindex = (
        {$i rs32rni.inc}
      );

      std_regname_index : array[tregisterindex] of tregisterindex = (
        {$i rs32sri.inc}
      );

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function is_imm12(value: tcgint): boolean;
      begin
        result:=(value >= -2048) and (value <= 2047);
      end;


    function is_calljmp(o:tasmop):boolean;
      begin
       is_calljmp:=false;
        case o of
          A_BEQ,A_BNE,A_BLT,A_BGE,A_BGT,A_BLE,
          A_BLTU,A_BGEU,A_BGTU,A_BLEU,
          A_J,A_JAL,A_JALR,A_CALL:
            is_calljmp:=true;
          else
            ;
        end;
      end;


    function inverse_cond(const c: TAsmCond): Tasmcond; {$ifdef USEINLINE}inline;{$endif USEINLINE}
      const
        inv_condflags:array[TAsmCond] of TAsmCond=(C_None,
          C_NE,C_EQ,C_GE,C_LT,C_LE,C_GT,C_GEU,C_LTU,C_LEU,C_GTU);
      begin
        result := inv_condflags[c];
      end;


    function reg_cgsize(const reg: tregister): tcgsize;
      begin
        case getregtype(reg) of
          R_INTREGISTER :
            result:=OS_INT;
          R_MMREGISTER:
            result:=OS_M128;
          R_FPUREGISTER:
            result:=OS_F64;
          else
            internalerror(200303181);
        end;
      end;


    function cgsize2subreg(regtype: tregistertype; s:Tcgsize):Tsubregister;
      begin
        cgsize2subreg:=R_SUBWHOLE;
      end;


    function findreg_by_number(r:Tregister):tregisterindex;
      begin
        result:=rgBase.findreg_by_number_table(r,regnumber_index);
      end;


    function std_regnum_search(const s:string):Tregister;
      begin
        result:=regnumber_table[findreg_by_name_table(s,std_regname_table,std_regname_index)];
      end;


    function std_regname(r:Tregister):string;
      var
        p : tregisterindex;
      begin
        p:=findreg_by_number_table(r,regnumber_index);
        if p<>0 then
          result:=std_regname_table[p]
        else
          result:=generic_regname(r);
      end;


    function dwarf_reg(r:tregister):shortint;
      begin
        result:=regdwarf_table[findreg_by_number(r)];
        if result=-1 then
          internalerror(200603251);
      end;

    function dwarf_reg_no_error(r:tregister):shortint;
      begin
        result:=regdwarf_table[findreg_by_number(r)];
      end;

    function eh_return_data_regno(nr: longint): longint;
      begin
        if (nr>=0) and (nr<4) then
          result:=nr+3  { a0..a3 = r3..r6 }
        else
          result:=-1;
      end;

    function conditions_equal(const c1, c2: TAsmCond): boolean;
      begin
        result:=c1=c2;
      end;


    { Checks if Subset is a subset of c (e.g. "less than" is a subset of "less than or equal" }
    function condition_in(const Subset, c: TAsmCond): Boolean;
      begin
        Result := (c = C_None) or conditions_equal(Subset, c);

        if not Result then
          case Subset of
            C_EQ:
              Result := (c in [C_GE, C_GEU, C_LE, C_LEU]);
            C_LT:
              Result := (c in [C_LE]);
            C_GT:
              Result := (c in [C_GE]);
            C_LTU:
              Result := (c in [C_LEU]);
            C_GTU:
              Result := (c in [C_GEU]);
            else
              Result := False;
          end;
      end;


    function is_extra_reg(const s: string): tregister;
      type
        treg2str = record
          name : string[4];
          reg : tregister;
        end;

      const
        extraregs : array[0..33] of treg2str = (
          (name: 'ZERO'; reg : NR_R0),
          (name: 'RV';   reg : NR_R1),
          (name: 'T0';   reg : NR_R2),
          (name: 'A0';   reg : NR_R3),
          (name: 'A1';   reg : NR_R4),
          (name: 'A2';   reg : NR_R5),
          (name: 'A3';   reg : NR_R6),
          (name: 'A4';   reg : NR_R7),
          (name: 'A5';   reg : NR_R8),
          (name: 'A6';   reg : NR_R9),
          (name: 'A7';   reg : NR_R10),
          (name: 'S0';   reg : NR_R11),
          (name: 'S1';   reg : NR_R12),
          (name: 'S2';   reg : NR_R13),
          (name: 'S3';   reg : NR_R14),
          (name: 'S4';   reg : NR_R15),
          (name: 'S5';   reg : NR_R16),
          (name: 'S6';   reg : NR_R17),
          (name: 'S7';   reg : NR_R18),
          (name: 'S8';   reg : NR_R19),
          (name: 'S9';   reg : NR_R20),
          (name: 'S10';  reg : NR_R21),
          (name: 'S11';  reg : NR_R22),
          (name: 'S12';  reg : NR_R23),
          (name: 'S13';  reg : NR_R24),
          (name: 'S14';  reg : NR_R25),
          (name: 'S15';  reg : NR_R26),
          (name: 'S16';  reg : NR_R27),
          (name: 'S17';  reg : NR_R28),
          (name: 'SP';   reg : NR_R29),
          (name: 'FP';   reg : NR_R30),
          (name: 'LR';   reg : NR_R31),
          (name: 'RA';   reg : NR_R31),
          (name: 'LINK'; reg : NR_R31)
          );

      var
        i : longint;
    begin
      result:=NR_NO;
      { reg found?
        possible aliases are always 2 to 4 chars
      }
      if not (length(s) in [2..4]) then
        exit;
      for i:=low(extraregs) to high(extraregs) do
        begin
          if s=extraregs[i].name then
            begin
              result:=extraregs[i].reg;
              exit;
            end;
        end;
    end;

end.
