{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit contains the GAS asm writer for SLOW-32

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

{****************************************************************************}
{                  Helper routines for Instruction Writer                    }
{****************************************************************************}

unit ags32gas;

{$i fpcdefs.inc}

  interface

    uses
       systems,aasmbase,
       aasmtai,aasmdata,
       assemble,aggas,
       cpubase,cgutils,
       globtype;

  type
    TS32InstrWriter=class(TCPUInstrWriter)
       procedure WriteInstruction(hp : tai);override;
    end;

    TS32GNUAssembler=class(TGNUassembler)
      constructor CreateWithWriter(info: pasminfo; wr: TExternalAssemblerOutputFile; freewriter, smart: boolean); override;
      function MakeCmdLine: TCmdStr; override;
    end;

  implementation

    uses
       cutils,globals,verbose,
       cgbase,
       itcpugas,cpuinfo,
       aasmcpu;


    { Generate memory reference string in SLOW-32 base+offset format.
      Examples: r29+4, r29+0, r29-4 }
    function getreferencestring(asminfo: pasminfo; var ref : treference) : string;
    var
      s : string;
    begin
       with ref do
        begin
          if ((offset < -2048) or (offset > 2047)) and
             (refaddr = addr_no) then
            internalerror(2006052501);
          case refaddr of
            addr_no:
              s := '';
            addr_pic_no_got:
              internalerror(2016060501);
            else
              begin
                s :='';
                if assigned(symbol) then
                  begin
                    if asminfo^.dollarsign<>'$' then
                      begin
                        s:=s+ApplyAsmSymbolRestrictions(symbol.name);
                        if assigned(relsymbol) then
                          s:=s+'-'+ApplyAsmSymbolRestrictions(relsymbol.name)
                      end
                    else
                      begin
                        s:=s+symbol.name;
                        if assigned(relsymbol) then
                          s:=s+'-'+relsymbol.name;
                      end;
                  end;
              end;
          end;
          if offset<0 then
           s:=s+tostr(offset)
          else
           if (offset>0) then
            begin
              if assigned(symbol) then
                s:=s+'+'+tostr(offset)
              else
                s:=s+tostr(offset);
            end;

           if (refaddr = addr_full) then
             begin
               { For addr_full, just return the symbol expression }
               getreferencestring:=s;
               exit;
             end;

           { SLOW-32 memory reference format: base+offset }
           if (index=NR_NO) then
             begin
                if (base<>NR_NO) then
                  begin
                    if (s='') then
                      s:=gas_regname(base)+'+0'
                    else
                      s:=gas_regname(base)+'+'+s;
                  end
                else if not assigned(symbol) then
                  begin
                    if (s='') then
                      s:='0+0'
                    else
                      s:='0+'+s;
                  end;
             end
           else
             Internalerror(2021030602);
        end;
      getreferencestring:=s;
    end;


    function getopstr(asminfo: pasminfo; const o:toper) : string;
    var
      hs : string;
    begin
      case o.typ of
        top_reg:
          getopstr:=gas_regname(o.reg);
        top_const:
          getopstr:=tostr(longint(o.val));
        top_ref:
          if o.ref^.refaddr=addr_full then
            begin
              hs:=o.ref^.symbol.name;
              if asminfo^.dollarsign<>'$' then
                hs:=ApplyAsmSymbolRestrictions(hs);
              if o.ref^.offset>0 then
               hs:=hs+'+'+tostr(o.ref^.offset)
              else
               if o.ref^.offset<0 then
                hs:=hs+tostr(o.ref^.offset);
              getopstr:=hs;
            end
          else
            getopstr:=getreferencestring(asminfo,o.ref^);
        else
          internalerror(2002070604);
      end;
    end;


    Procedure TS32InstrWriter.WriteInstruction(hp : tai);
    var s: string;
        i: byte;
        sep: string[3];
    begin
      s:=#9+gas_op2str[taicpu(hp).opcode];

      if taicpu(hp).ops<>0 then
        begin
          sep:=#9;
          for i:=0 to taicpu(hp).ops-1 do
            begin
               s:=s+sep+getopstr(owner.asminfo,taicpu(hp).oper[i]^);
               sep:=',';
            end;
        end;

      owner.writer.AsmWriteLn(s);
    end;


{****************************************************************************}
{                        GNU SLOW-32 Assembler writer                        }
{****************************************************************************}

    constructor TS32GNUAssembler.CreateWithWriter(info: pasminfo; wr: TExternalAssemblerOutputFile; freewriter, smart: boolean);
      begin
        inherited;
        InstrWriter := TS32InstrWriter.create(self);
      end;


    function TS32GNUAssembler.MakeCmdLine: TCmdStr;
      begin
        result := inherited MakeCmdLine;
      end;


  const
    as_slow32_gas_info : tasminfo =
       (
         id     : as_gas;

         idtxt  : 'AS';
         asmbin : 'as';
         asmcmd : '-o $OBJ $EXTRAOPT $ASM';
         supported_targets : [system_slow32_embedded];
         flags : [af_needar,af_smartlink_sections,af_supports_dwarf];
         labelprefix : '.L';
         labelmaxlen : -1;
         comment : '# ';
         dollarsign: '$';
       );

begin
  RegisterAssembler(as_slow32_gas_info,TS32GNUAssembler);
end.
