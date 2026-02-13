{
    Copyright (c) 1998-2002 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit implements the SLOW-32 optimizer object

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

Unit aoptcpu;

Interface

{$i fpcdefs.inc}

uses
  cpubase,
  globals, globtype,
  cgbase,
  aoptobj, aoptcpub, aopt,
  aasmtai, aasmcpu;

Type
  TCpuAsmOptimizer = class(TAsmOptimizer)
    function InstructionLoadsFromReg(const reg: TRegister; const hp: tai): boolean; override;
    function RegLoadedWithNewValue(reg: tregister; hp: tai): boolean; override;
    function RegModifiedByInstruction(Reg: TRegister; p1: tai): boolean; override;

    { outputs a debug message into the assembler file }
    procedure DebugMsg(const s: string; p: tai);

    function PeepHoleOptPass1Cpu(var p: tai): boolean; override;
  End;

Implementation

uses
  verbose,
  cutils,
  aasmbase;


procedure TCpuAsmOptimizer.DebugMsg(const s: string; p: tai);
  begin
    if cs_asm_source in current_settings.globalswitches then
      asml.insertbefore(tai_comment.Create(strpnew(s)),p);
  end;


function TCpuAsmOptimizer.InstructionLoadsFromReg(const reg: TRegister; const hp: tai): boolean;
  var
    p: taicpu;
    i: longint;
  begin
    result:=false;
    if not (assigned(hp) and (hp.typ=ait_instruction)) then
      exit;
    p:=taicpu(hp);

    { lots of instructions load from registers; conservatively check all operands }
    for i:=0 to p.ops-1 do
      begin
        case p.oper[i]^.typ of
          top_reg:
            { for most instructions, operand 0 is the destination and is written,
              not read. But branches, stores, etc. read all operands. }
            begin
              if (i=0) and not(p.opcode in [A_STB,A_STH,A_STW,
                  A_BEQ,A_BNE,A_BLT,A_BGE,A_BGT,A_BLE,A_BLTU,A_BGEU,A_BGTU,A_BLEU,
                  A_JALR]) then
                continue;
              if (p.oper[i]^.reg=reg) then
                begin
                  result:=true;
                  exit;
                end;
            end;
          top_ref:
            begin
              if (p.oper[i]^.ref^.base=reg) or
                 (p.oper[i]^.ref^.index=reg) then
                begin
                  result:=true;
                  exit;
                end;
            end;
          else
            ;
        end;
      end;
  end;


function TCpuAsmOptimizer.RegLoadedWithNewValue(reg: tregister; hp: tai): boolean;
  var
    p: taicpu;
  begin
    result:=false;
    if not (assigned(hp) and (hp.typ=ait_instruction)) then
      exit;
    p:=taicpu(hp);

    { Instructions that write to operand 0 as destination }
    case p.opcode of
      A_ADD,A_SUB,A_AND,A_OR,A_XOR,A_SLL,A_SRL,A_SRA,
      A_ADDI,A_ANDI,A_ORI,A_XORI,A_SLLI,A_SRLI,A_SRAI,
      A_MUL,A_MULH,A_MULHU,A_DIV,A_REM,
      A_LUI,
      A_SLT,A_SLTU,A_SEQ,A_SNE,A_SGT,A_SGTU,A_SGE,A_SGEU,A_SLE,A_SLEU,
      A_LDB,A_LDBU,A_LDH,A_LDHU,A_LDW,
      A_LI,A_MV,A_NOT,A_NEG,A_LA:
        begin
          if (p.ops>0) and (p.oper[0]^.typ=top_reg) and
             (p.oper[0]^.reg=reg) then
            result:=true;
        end;
      A_JAL:
        begin
          { JAL writes to the link register (operand 0 if two-operand form) }
          if (p.ops>=1) and (p.oper[0]^.typ=top_reg) and
             (p.oper[0]^.reg=reg) then
            result:=true;
        end;
      else
        ;
    end;
  end;


function TCpuAsmOptimizer.RegModifiedByInstruction(Reg: TRegister; p1: tai): boolean;
  begin
    result:=RegLoadedWithNewValue(reg,p1);
  end;


function TCpuAsmOptimizer.PeepHoleOptPass1Cpu(var p: tai): boolean;
  begin
    { Minimal implementation - peephole optimizations can be added later }
    result:=false;
  end;


begin
  casmoptimizer:=TCpuAsmOptimizer;
End.
