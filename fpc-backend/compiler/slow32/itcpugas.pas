{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit contains the SLOW-32 GAS instruction tables

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
unit itcpugas;

{$I fpcdefs.inc}

  interface

    uses
      cpubase, cgbase;

    const
      gas_op2str: array[tasmop] of string[14] = ('<none>',
        { Pseudo instructions }
        'nop','call','la','li','mv','not','neg','j','ret','tail',
        { R-type ALU }
        'add','sub','and','or','xor','sll','srl','sra',
        { I-type ALU }
        'addi','andi','ori','xori','slli','srli','srai',
        { Multiply/Divide }
        'mul','mulh','mulhu','div','rem',
        { Load upper immediate }
        'lui',
        { Compare }
        'slt','sltu','seq','sne','sgt','sgtu','sge','sgeu','sle','sleu',
        { Load }
        'ldb','ldbu','ldh','ldhu','ldw',
        { Store }
        'stb','sth','stw',
        { Branch }
        'beq','bne','blt','bge','bgt','ble','bltu','bgeu','bgtu','bleu',
        { Jump }
        'jal','jalr'
        );

    function gas_regnum_search(const s: string): Tregister;
    function gas_regname(r: Tregister): string;
    function is_extra_reg(const s: string): tregister;

  implementation

    uses
      globtype,globals,aasmbase,
      cutils,verbose,systems,
      rgbase;

    const
      gas_regname_table : TRegNameTable = (
        {$i rs32std.inc}
      );

      gas_regname_index : array[tregisterindex] of tregisterindex = (
        {$i rs32sri.inc}
      );

    function findreg_by_gasname(const s:string):tregisterindex;
      var
        i,p : tregisterindex;
      begin
        {Binary search.}
        p:=0;
        i:=regnumber_count_bsstart;
        repeat
          if (p+i<=high(tregisterindex)) and (gas_regname_table[gas_regname_index[p+i]]<=s) then
            p:=p+i;
          i:=i shr 1;
        until i=0;
        if gas_regname_table[gas_regname_index[p]]=s then
          findreg_by_gasname:=gas_regname_index[p]
        else
          findreg_by_gasname:=0;
      end;


    function gas_regnum_search(const s:string):Tregister;
      begin
        result:=regnumber_table[findreg_by_gasname(s)];
      end;


    function gas_regname(r:Tregister):string;
      var
        p : tregisterindex;
      begin
        p:=findreg_by_number(r);
        if p<>0 then
          result:=gas_regname_table[p]
        else
          result:=generic_regname(r);
      end;


    function is_extra_reg(const s: string): tregister;
      type
        treg2str = record
          name : string[5];
          reg : tregister;
        end;

      const
        { ABI aliases for SLOW-32 registers:
            r0=zero, r1=rv, r2=t0, r3-r10=a0-a7,
            r11-r28=s0-s17, r29=sp, r30=fp, r31=lr }
        extraregs : array[0..29] of treg2str = (
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
          (name: 'SP';   reg : NR_R29)
          { NR_R30=fp and NR_R31=lr are already named r30/r31;
            fp is handled as FP alias below }
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
