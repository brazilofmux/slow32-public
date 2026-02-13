{
    Copyright (c) 2006 by Florian Klaempfl
    Copyright (c) 2025-2026 by SLOW-32 contributors

    This unit implements the code generator for the SLOW-32

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
unit cgcpu;

{$i fpcdefs.inc}

  interface

    uses
       globtype,symtype,symdef,
       cgbase,cgobj,
       aasmbase,aasmcpu,aasmtai,aasmdata,
       cpubase,cpuinfo,cgutils,cg64f32,rgcpu,
       parabase;

    type

      { tcgs32 - main code generator for SLOW-32 }

      tcgs32 = class(tcg)
        procedure init_register_allocators; override;
        procedure done_register_allocators; override;

        procedure a_loadaddr_ref_cgpara(list : TAsmList;const r : treference;const paraloc : tcgpara); override;

        procedure a_bit_scan_reg_reg(list: TAsmList; reverse,not_zero: boolean; srcsize, dstsize: tcgsize; src, dst: TRegister); override;

        procedure a_call_reg(list : TAsmList;reg: tregister); override;
        procedure a_call_name(list : TAsmList;const s : string; weak: boolean); override;

        { move instructions }
        procedure a_load_reg_reg(list : TAsmList; fromsize, tosize : tcgsize;reg1,reg2 : tregister); override;

        procedure a_load_const_ref(list: TAsmList; size: tcgsize; a: tcgint; const ref: treference); override;
        procedure a_load_reg_ref(list: TAsmList; fromsize, tosize: TCGSize; reg: tregister; const ref: treference); override;
        procedure a_load_ref_reg(list: TAsmList; fromsize, tosize: tcgsize; const ref: treference; reg: tregister); override;
        procedure a_load_const_reg(list: TAsmList; size: tcgsize; a: tcgint; register: tregister); override;

        procedure a_op_const_reg(list : TAsmList; Op: TOpCG; size: TCGSize; a: tcgint; reg: TRegister); override;
        procedure a_op_reg_reg(list : TAsmList; Op: TOpCG; size: TCGSize; src, dst: TRegister); override;

        procedure a_op_const_reg_reg(list: TAsmList; op: TOpCg; size: tcgsize; a: tcgint; src, dst: tregister); override;
        procedure a_op_reg_reg_reg(list: TAsmList; op: TOpCg; size: tcgsize; src1, src2, dst: tregister); override;
        procedure a_op_const_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tcgsize; a: tcgint; src, dst: tregister;setflags : boolean;var ovloc : tlocation); override;
        procedure a_op_reg_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tcgsize; src1, src2, dst: tregister; setflags: boolean; var ovloc: tlocation); override;

        procedure a_loadaddr_ref_reg(list : TAsmList;const ref : treference;r : tregister);override;

        procedure a_cmp_const_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;a : tcgint;reg : tregister; l : tasmlabel); override;
        procedure a_cmp_reg_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel); override;

        procedure a_jmp_name(list : TAsmList;const s : string); override;
        procedure a_jmp_always(list : TAsmList;l: tasmlabel); override;

        { 32x32 to 64 bit multiplication }
        procedure a_mul_reg_reg_pair(list: TAsmList;size: tcgsize; src1,src2,dstlo,dsthi: tregister); override;

        procedure g_concatcopy(list : TAsmList;const source,dest : treference;len : tcgint); override;

        procedure g_proc_entry(list : TAsmList;localsize : longint;nostackframe:boolean);override;
        procedure g_proc_exit(list : TAsmList;parasize : longint;nostackframe:boolean); override;

        procedure g_save_registers(list: TAsmList); override;
        procedure g_restore_registers(list: TAsmList); override;

        procedure g_profilecode(list: TAsmList); override;

        procedure g_overflowcheck_loc(list: TAsmList; const Loc: tlocation; def: tdef; ovloc: tlocation); override;
        procedure g_overflowcheck(list: TAsmList; const Loc: tlocation; def: tdef); override;

        { fpu stubs - SLOW-32 has no hardware FPU, floats handled via soft-float }
        procedure a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tcgsize; reg1, reg2: tregister); override;
        procedure a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tcgsize; const ref: treference; reg: tregister); override;
        procedure a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tcgsize; reg: tregister; const ref: treference); override;

        procedure g_check_for_fpu_exception(list: TAsmList;force,clear : boolean); override;

        function fixref(list: TAsmList; var ref: treference): boolean;
      protected
        procedure maybeadjustresult(list: TAsmList; op: topcg; size: tcgsize; dst: tregister);
      end;

      { tcg64fs32 - 64-bit code generator for SLOW-32 (32-bit ALU) }

      tcg64fs32 = class(tcg64f32)
        procedure a_op64_reg_reg(list : TAsmList;op:TOpCG;size : tcgsize;regsrc,regdst : tregister64);override;
        procedure a_op64_const_reg(list : TAsmList;op:TOpCG;size : tcgsize;value : int64;reg : tregister64);override;
        procedure a_op64_const_reg_reg(list: TAsmList;op:TOpCG;size : tcgsize;value : int64;regsrc,regdst : tregister64);override;
        procedure a_op64_reg_reg_reg(list: TAsmList;op:TOpCG;size : tcgsize;regsrc1,regsrc2,regdst : tregister64);override;
        procedure a_load64_ref_cgpara(list: TAsmList; const r: treference; const paraloc: tcgpara);override;
        procedure a_load64_ref_reg(list: TAsmList; const ref: treference; reg: tregister64);override;
        procedure a_load64_reg_ref(list: TAsmList; reg: tregister64; const ref: treference);override;
      end;


  const
    TOpCG2AsmConstOp: Array[topcg] of TAsmOp = (A_NONE,
          A_NONE,A_ADDI,A_ANDI,A_NONE,A_NONE,A_NONE,A_NONE,
          A_None,A_None,A_ORI,A_SRAI,A_SLLI,A_SRLI,A_NONE,A_XORI,A_None,A_None);
    TOpCG2AsmOp: Array[topcg] of TAsmOp = (A_NONE,
          A_NONE,A_ADD,A_AND,A_NONE,A_DIV,A_MUL,A_MUL,
          A_None,A_None,A_OR,A_SRA,A_SLL,A_SRL,A_SUB,A_XOR,A_None,A_None);

{$ifdef extdebug}
     function ref2string(const ref : treference) : string;
     function cgop2string(const op : TOpCg) : String;
{$endif extdebug}

  procedure create_codegen;

  implementation

    uses
       {$ifdef extdebug}sysutils,{$endif}
       symtable,
       globals,verbose,systems,cutils,
       symconst,symsym,fmodule,
       rgobj,tgobj,cpupi,procinfo,paramgr;

{$undef AVOID_OVERFLOW}
{$ifopt Q+}
  {$define AVOID_OVERFLOW}
  const
     max_12_bit = 1 shl 12;
{$endif}

{ Range check must be disabled explicitly as conversions between signed and unsigned
  32-bit values are done without explicit typecasts }
{$R-}

{$ifdef extdebug}
     function ref2string(const ref : treference) : string;
       begin
         result := 'base : ' + inttostr(ord(ref.base)) + ' index : ' + inttostr(ord(ref.index)) + ' refaddr : ' + inttostr(ord(ref.refaddr)) + ' offset : ' + inttostr(ref.offset) + ' symbol : ';
         if (assigned(ref.symbol)) then
           result := result + ref.symbol.name;
       end;

     function cgop2string(const op : TOpCg) : String;
       const
         opcg_strings : array[TOpCg] of string[6] = (
           'None', 'Move', 'Add', 'And', 'Div', 'IDiv', 'IMul', 'Mul',
           'Neg', 'Not', 'Or', 'Sar', 'Shl', 'Shr', 'Sub', 'Xor', 'Rol', 'Ror'
         );
       begin
         result := opcg_strings[op];
       end;
{$endif extdebug}


    { Helper: check if a value fits in the upper 20 bits with low 12 bits zero }
    function is_lui_imm(value: tcgint): boolean;
      begin
        result:=SarInt64((value and $FFFFF000) shl 32, 32) = value;
      end;


{*****************************************************************************
                            Register allocators
*****************************************************************************}

    procedure tcgs32.init_register_allocators;
      begin
        inherited init_register_allocators;

        { SLOW-32 integer register allocation order:
          - First allocate caller-saved (argument/temp) registers: r3-r10 (a0-a7), r2 (t0), r1 (rv)
          - Then allocate callee-saved: r11-r28 (s0-s17)
          - Never allocate: r0 (zero), r29 (sp), r30 (fp), r31 (lr) }
        rg[R_INTREGISTER]:=trgintcpu.create(R_INTREGISTER,R_SUBWHOLE,
          [RS_R3,RS_R4,RS_R5,RS_R6,RS_R7,RS_R8,RS_R9,RS_R10,
           RS_R2,RS_R1,
           RS_R11,RS_R12,RS_R13,RS_R14,RS_R15,RS_R16,RS_R17,RS_R18,
           RS_R19,RS_R20,RS_R21,RS_R22,RS_R23,RS_R24,RS_R25,RS_R26,
           RS_R27,RS_R28],first_int_imreg,[]);

        { No FPU register allocator - SLOW-32 has no FPU register file }
      end;


    procedure tcgs32.done_register_allocators;
      begin
        rg[R_INTREGISTER].free;
        inherited done_register_allocators;
      end;


{*****************************************************************************
                               Calls
*****************************************************************************}

    procedure tcgs32.a_call_name(list : TAsmList;const s : string; weak: boolean);
      var
        href: treference;
      begin
        if not(weak) then
          reference_reset_symbol(href,current_asmdata.RefAsmSymbol(s,AT_FUNCTION),0,0,[])
        else
          reference_reset_symbol(href,current_asmdata.WeakRefAsmSymbol(s,AT_FUNCTION),0,0,[]);

        { SLOW-32: Use CALL pseudo-instruction which the assembler will expand }
        list.concat(taicpu.op_ref(A_CALL,href));

        { not assigned while generating external wrappers }
        if assigned(current_procinfo) then
          include(current_procinfo.flags,pi_do_call);
      end;


    procedure tcgs32.a_call_reg(list : TAsmList;reg: tregister);
      begin
        { JALR lr, reg, 0 -- call via register, saving return address in lr (r31) }
        list.concat(taicpu.op_reg_reg(A_JALR,NR_RETURN_ADDRESS_REG,reg));
        include(current_procinfo.flags,pi_do_call);
      end;


{*****************************************************************************
                            Load/Store operations
*****************************************************************************}

    procedure tcgs32.a_load_const_ref(list: TAsmList; size: tcgsize; a: tcgint; const ref: treference);
      begin
        if a=0 then
          a_load_reg_ref(list,size,size,NR_R0,ref)
        else
          inherited a_load_const_ref(list, size, a, ref);
      end;


    procedure tcgs32.a_load_const_reg(list: TAsmList; size: tcgsize; a: tcgint; register: tregister);
      begin
        if a=0 then
          { ADDI dst, r0, 0 }
          a_load_reg_reg(list,size,size,NR_R0,register)
        else
          begin
            if is_imm12(a) then
              { Small constant: ADDI dst, r0, imm }
              list.concat(taicpu.op_reg_reg_const(A_ADDI,register,NR_R0,a))
            else if is_lui_imm(a) then
              { Upper-only constant: LUI dst, upper20 }
              list.concat(taicpu.op_reg_const(A_LUI,register,(a shr 12) and $FFFFF))
            else
              begin
                { General case: LUI dst, upper20 ; ORI dst, dst, lower12
                  SLOW-32 uses ORI instead of ADDI for the lower bits to avoid
                  sign-extension issues with the RISC-V ADDI approach. }
                list.concat(taicpu.op_reg_const(A_LUI,register,(a shr 12) and $FFFFF));
                list.concat(taicpu.op_reg_reg_const(A_ORI,register,register,a and $FFF));
              end;
          end;
      end;


    procedure tcgs32.a_load_reg_reg(list : TAsmList;fromsize, tosize : tcgsize;reg1,reg2 : tregister);
      var
        ai: taicpu;
      begin
{$ifdef EXTDEBUG}
        list.concat(tai_comment.Create(strpnew('Move '+tcgsize2str(fromsize)+'->'+tcgsize2str(tosize))));
{$endif EXTDEBUG}
        if (tosize=OS_S32) and (fromsize=OS_32) then
          begin
            ai:=taicpu.op_reg_reg_const(A_ADDI,reg2,reg1,0);
            list.concat(ai);
            rg[R_INTREGISTER].add_move_instruction(ai);
          end
        else if (tcgsize2unsigned[tosize]=OS_32) and (fromsize=OS_8) then
          list.Concat(taicpu.op_reg_reg_const(A_ANDI,reg2,reg1,$FF))
        else if (tosize=OS_8) and (fromsize<>OS_8) then
          list.Concat(taicpu.op_reg_reg_const(A_ANDI,reg2,reg1,$FF))
        else if (tcgsize2size[fromsize] > tcgsize2size[tosize]) or
          ((tcgsize2size[fromsize] = tcgsize2size[tosize]) and (fromsize <> tosize)) or
          { do we need to mask out the sign when loading from smaller signed to larger unsigned type? }
          ((tcgsize2unsigned[fromsize]<>fromsize) and ((tcgsize2unsigned[tosize]=tosize)) and
            (tcgsize2size[fromsize] < tcgsize2size[tosize]) and (tcgsize2size[tosize] <> sizeof(pint)) ) then
          begin
            { Sign/zero extension using SLLI+SRAI/SRLI patterns }
            if tcgsize2size[fromsize]<tcgsize2size[tosize] then
              begin
                list.Concat(taicpu.op_reg_reg_const(A_SLLI,reg2,reg1,8*(4-tcgsize2size[fromsize])));

                if tcgsize2unsigned[fromsize]<>fromsize then
                  list.Concat(taicpu.op_reg_reg_const(A_SRAI,reg2,reg2,8*(tcgsize2size[tosize]-tcgsize2size[fromsize])))
                else
                  list.Concat(taicpu.op_reg_reg_const(A_SRLI,reg2,reg2,8*(tcgsize2size[tosize]-tcgsize2size[fromsize])));
              end
            else
              list.Concat(taicpu.op_reg_reg_const(A_SLLI,reg2,reg1,8*(4-tcgsize2size[tosize])));

            if tcgsize2unsigned[tosize]=tosize then
              list.Concat(taicpu.op_reg_reg_const(A_SRLI,reg2,reg2,8*(4-tcgsize2size[tosize])))
            else
              list.Concat(taicpu.op_reg_reg_const(A_SRAI,reg2,reg2,8*(4-tcgsize2size[tosize])));
          end
        else
          begin
            ai:=taicpu.op_reg_reg_const(A_ADDI,reg2,reg1,0);
            list.concat(ai);
            rg[R_INTREGISTER].add_move_instruction(ai);
          end;
      end;


    procedure tcgs32.a_load_reg_ref(list: TAsmList; fromsize, tosize: TCGSize;
        reg: tregister; const ref: treference);

      const
        StoreInstr: array[OS_8..OS_INT] of TAsmOp =
        (A_STB,A_STH,A_STW);
      var
        ref2: TReference;
        op: TAsmOp;
      begin
        if not (fromsize in [OS_8..OS_INT,OS_S8..OS_SINT]) then
          internalerror(2002090904);
        if not (tosize in [OS_8..OS_INT,OS_S8..OS_SINT]) then
          internalerror(2002090905);

        tosize:=tcgsize2unsigned[tosize];

        ref2 := ref;
        fixref(list, ref2);

        op := storeinstr[tcgsize2unsigned[tosize]];
        list.concat(taicpu.op_reg_ref(op, reg,ref2));
      end;


    procedure tcgs32.a_load_ref_reg(list: TAsmList; fromsize, tosize: tcgsize; const ref: treference; reg: tregister);
      var
        href: treference;
        op: TAsmOp;
        tmpreg: TRegister;
      begin
        href:=ref;
        fixref(list,href);

        if href.refaddr=addr_full then
          begin
            tmpreg:=getintregister(list,OS_ADDR);
            a_loadaddr_ref_reg(list,href,tmpreg);
            reference_reset_base(href,tmpreg,0,ctempposinvalid,0,ref.volatility);
          end;

        case fromsize of
          OS_8: op:=A_LDBU;
          OS_16: op:=A_LDHU;
          OS_S8: op:=A_LDB;
          OS_S16: op:=A_LDH;
          OS_64,OS_S64, { This only happens if tosize is smaller than fromsize }
          { We can therefore only consider the low 32-bit of the 64bit value }
          OS_32,
          OS_S32: op:=A_LDW;
        else
          internalerror(2016060502);
        end;

        list.concat(taicpu.op_reg_ref(op,reg,href));
        if (fromsize<>tosize) and (not (tosize in [OS_SINT,OS_INT])) then
          a_load_reg_reg(list,fromsize,tosize,reg,reg);
      end;


    procedure tcgs32.a_loadaddr_ref_cgpara(list : TAsmList;const r : treference;const paraloc : tcgpara);
      var
        ref: treference;
        tmpreg: tregister;

      begin
        paraloc.check_simple_location;
        paramanager.allocparaloc(list,paraloc.location);
        case paraloc.location^.loc of
           LOC_REGISTER,LOC_CREGISTER:
             a_loadaddr_ref_reg(list,r,paraloc.location^.register);
           LOC_REFERENCE:
             begin
               reference_reset(ref,paraloc.alignment,[]);
               ref.base := paraloc.location^.reference.index;
               ref.offset := paraloc.location^.reference.offset;
               tmpreg := rg[R_INTREGISTER].getregister(list,R_SUBWHOLE);
               a_loadaddr_ref_reg(list,r,tmpreg);
               a_load_reg_ref(list,OS_ADDR,OS_ADDR,tmpreg,ref);
             end;
           else
             internalerror(2002080701);
        end;
      end;


    procedure tcgs32.a_bit_scan_reg_reg(list: TAsmList; reverse,not_zero: boolean; srcsize, dstsize: tcgsize; src, dst: TRegister);
      begin
        internalerror(2016060401);
      end;


{*****************************************************************************
                            ALU operations
*****************************************************************************}

    procedure tcgs32.a_op_const_reg(list : TAsmList; Op: TOpCG; size: TCGSize; a: tcgint; reg: TRegister);
      begin
        a_op_const_reg_reg(list,op,size,a,reg,reg);
      end;


    procedure tcgs32.a_op_reg_reg(list : TAsmList; Op: TOpCG; size: TCGSize; src, dst: TRegister);
      begin
        a_op_reg_reg_reg(list,op,size,src,dst,dst);
      end;


    procedure tcgs32.a_op_const_reg_reg(list: TAsmList; op: TOpCg; size: tcgsize; a: tcgint; src, dst: tregister);
      var
        ovloc: tlocation;
      begin
        a_op_const_reg_reg_checkoverflow(list, op, size, a, src, dst, false, ovloc);
      end;


    procedure tcgs32.a_op_reg_reg_reg(list: TAsmList; op: TOpCg; size: tcgsize; src1, src2, dst: tregister);
      var
        ovloc: tlocation;
      begin
        a_op_reg_reg_reg_checkoverflow(list, op, size, src1, src2, dst, false, ovloc);
      end;


    procedure tcgs32.a_op_const_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tcgsize; a: tcgint; src, dst: tregister; setflags: boolean;
      var ovloc: tlocation);
      var
        tmpreg: TRegister;
      begin
        optimize_op_const(size,op,a);

        if op=OP_NONE then
          begin
            a_load_reg_reg(list,size,size,src,dst);
            exit;
          end;

        if (op=OP_SUB) and not(setflags) then
          begin
            op:=OP_ADD;
            a:=-a;
          end;

        if (TOpCG2AsmConstOp[op]<>A_None) and
           is_imm12(a) and not(setflags) then
          begin
            list.concat(taicpu.op_reg_reg_const(TOpCG2AsmConstOp[op],dst,src,a));
            maybeadjustresult(list,op,size,dst);
          end
        else if setflags then
          begin
            tmpreg:=getintregister(list,size);
            a_load_const_reg(list,size,a,tmpreg);
            a_op_reg_reg_reg_checkoverflow(list,op,size,tmpreg,src,dst,true,ovloc);
          end
        else
          begin
            tmpreg:=getintregister(list,size);
            a_load_const_reg(list,size,a,tmpreg);
            a_op_reg_reg_reg(list,op,size,tmpreg,src,dst);
          end;
      end;


    procedure tcgs32.a_op_reg_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tcgsize; src1, src2, dst: tregister; setflags: boolean; var ovloc: tlocation);
      var
        name: String;
        pd: tprocdef;
        paraloc1, paraloc2: tcgpara;
        tmpreg1, tmpreg2: TRegister;
        signed: Boolean;
      begin
        signed:=tcgsize2unsigned[size]<>size;
        if setflags and
          { do we know overflow checking for this operation? }
          (size in [OS_32,OS_S32]) and
          (op in [OP_ADD,OP_SUB,OP_MUL,OP_IMUL,OP_IDIV,OP_NEG]) then
          begin
            ovloc.loc:=LOC_JUMP;
            current_asmdata.getjumplabel(ovloc.truelabel);
            current_asmdata.getjumplabel(ovloc.falselabel);
          end
        else
          ovloc.loc:=LOC_VOID;
        if op=OP_NOT then
          begin
            list.concat(taicpu.op_reg_reg_const(A_XORI,dst,src1,-1));
            maybeadjustresult(list,op,size,dst);
          end
        else if op=OP_NEG then
          begin
            list.concat(taicpu.op_reg_reg_reg(A_SUB,dst,NR_R0,src1));

            if setflags then
              begin
                { if dst and src are equal, an overflow happened }
                a_cmp_reg_reg_label(list,OS_INT,OC_NE,dst,src1,ovloc.falselabel);
                a_jmp_always(list,ovloc.truelabel);
              end;

            maybeadjustresult(list,op,size,dst);
          end
        else
          case op of
            OP_MOVE:
              a_load_reg_reg(list,size,size,src1,dst);
          else
            if (op in [OP_IMUL,OP_MUL]) and not(CPUS32_HAS_MUL in cpu_capabilities[current_settings.cputype]) then
              begin
                case size of
                  OS_8:
                    name:='fpc_mul_byte';
                  OS_S8:
                    name:='fpc_mul_shortint';
                  OS_16:
                    name:='fpc_mul_word';
                  OS_S16:
                    name:='fpc_mul_integer';
                  OS_32:
                    name:='fpc_mul_dword';
                  OS_S32:
                    name:='fpc_mul_longint';
                  else
                    Internalerror(2021030601);
                end;

                pd:=search_system_proc(name);
                paraloc1.init;
                paraloc2.init;
                paramanager.getcgtempparaloc(list,pd,1,paraloc1);
                paramanager.getcgtempparaloc(list,pd,2,paraloc2);
                a_load_reg_cgpara(list,OS_8,src1,paraloc2);
                a_load_reg_cgpara(list,OS_8,src2,paraloc1);
                paramanager.freecgpara(list,paraloc2);
                paramanager.freecgpara(list,paraloc1);
                alloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
                a_call_name(list,upper(name),false);
                dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
                cg.a_reg_alloc(list,NR_FUNCTION_RESULT_REG);
                cg.a_load_reg_reg(list,size,size,NR_FUNCTION_RESULT_REG,dst);
                cg.a_reg_dealloc(list,NR_FUNCTION_RESULT_REG);
                paraloc2.done;
                paraloc1.done;
              end
            else
              begin
                if setflags and (op=OP_MUL) and (size in [OS_32]) then
                  begin
                    tmpreg1:=getintregister(list,size);
                    list.concat(taicpu.op_reg_reg_reg(A_MULHU,tmpreg1,src2,src1));
                  end
                else
                  tmpreg1:=NR_NO;
                list.concat(taicpu.op_reg_reg_reg(TOpCG2AsmOp[op],dst,src2,src1));
                if setflags and (size in [OS_S32,OS_32]) then
                  begin
                    case op of
                      OP_ADD:
                        begin
                          if size=OS_SINT then
                            begin
                              tmpreg1:=getintregister(list,size);
                              list.concat(taicpu.op_reg_reg_reg(A_SLT,tmpreg1,dst,src2));
                              tmpreg2:=getintregister(list,size);
                              { SLTI: no A_SLTI in SLOW-32, use SLT with zero-loaded reg }
                              { Actually, compare src1 < 0: use SLT src1, r0 }
                              list.concat(taicpu.op_reg_reg_reg(A_SLT,tmpreg2,src1,NR_R0));
                              a_cmp_reg_reg_label(list,OS_INT,OC_EQ,tmpreg1,tmpreg2,ovloc.falselabel)
                            end
                          else if size=OS_INT then
                            begin
                              { unsigned overflow: result < either operand }
                              a_cmp_reg_reg_label(list,OS_INT,OC_AE,src2,dst,ovloc.falselabel);
                            end
                          else
                            Internalerror(2025102003);
                          a_jmp_always(list,ovloc.truelabel);
                        end;
                      OP_SUB:
                        begin
                          if signed then
                            begin
                              tmpreg1:=getintregister(list,size);
                              list.concat(taicpu.op_reg_reg_reg(A_SLT,tmpreg1,src2,dst));
                              tmpreg2:=getintregister(list,size);
                              list.concat(taicpu.op_reg_reg_reg(A_SLT,tmpreg2,src1,NR_R0));
                              a_cmp_reg_reg_label(list,OS_INT,OC_EQ,tmpreg1,tmpreg2,ovloc.falselabel)
                            end
                          else
                            begin
                              { unsigned: no overflow if result <= src2 }
                              a_cmp_reg_reg_label(list,OS_INT,OC_AE,src2,dst,ovloc.falselabel);
                            end;
                          a_jmp_always(list,ovloc.truelabel);
                        end;
                      OP_MUL:
                        begin
                          if size=OS_INT then
                            a_cmp_reg_reg_label(list,OS_INT,OC_EQ,tmpreg1,NR_R0,ovloc.falselabel)
                          else
                            Internalerror(2025102002);
                          a_jmp_always(list,ovloc.truelabel);
                        end;
                      OP_IMUL:
                        begin
                          if size=OS_SINT then
                            begin
                              tmpreg1:=getintregister(list,size);
                              list.concat(taicpu.op_reg_reg_reg(A_MULH,tmpreg1,src2,src1));
                              tmpreg2:=getintregister(list,size);
                              list.concat(taicpu.op_reg_reg_const(A_SRAI,tmpreg2,dst,sizeof(aint)*8-1));
                              a_cmp_reg_reg_label(list,OS_INT,OC_EQ,tmpreg1,tmpreg2,ovloc.falselabel);
                            end
                          else
                            Internalerror(2025102004);
                          a_jmp_always(list,ovloc.truelabel);
                        end;
                      OP_IDIV:
                        begin
                          { Only overflow if dst is all 1's }
                          tmpreg1:=getintregister(list,OS_INT);
                          list.Concat(taicpu.op_reg_reg_const(A_ADDI,tmpreg1,dst,1));

                          a_cmp_reg_reg_label(list,OS_INT,OC_NE,tmpreg1,NR_R0,ovloc.falselabel);

                          a_jmp_always(list,ovloc.truelabel);
                        end;
                      else
                        ;
                    end
                  end;
                maybeadjustresult(list,op,size,dst);
              end;
          end;
      end;


{*****************************************************************************
                        Address loading
*****************************************************************************}

    procedure tcgs32.a_loadaddr_ref_reg(list : TAsmList;const ref : treference;r : tregister);
      var
        href: treference;
        b, tmpreg: TRegister;
      begin
        href:=ref;
        fixref(list,href);

        if (not assigned(href.symbol)) and
           (href.offset=0) then
          a_load_reg_reg(list,OS_ADDR,OS_ADDR,href.base,r)
        else if (assigned(href.symbol) or
            (not is_imm12(href.offset))) and
           (href.base<>NR_NO) then
          begin
            { Symbol + base: load symbol address via LA, then add base }
            b:= href.base;

            tmpreg:=getintregister(list,OS_ADDR);

            href.base:=NR_NO;
            href.refaddr:=addr_full;
            list.concat(taicpu.op_reg_ref(A_LA,tmpreg,href));

            list.concat(taicpu.op_reg_reg_reg(A_ADD,r,tmpreg,b));
          end
        else if is_imm12(href.offset) and
           (href.base<>NR_NO) then
          begin
            list.concat(taicpu.op_reg_reg_const(A_ADDI,r,href.base,href.offset));
          end
        else if assigned(href.symbol) then
          begin
            { Symbol only, no base }
            tmpreg:=getintregister(list,OS_ADDR);

            b:=href.base;
            href.base:=NR_NO;

            href.refaddr:=addr_full;
            list.concat(taicpu.op_reg_ref(A_LA,r,href));

            if b<>NR_NO then
              list.concat(taicpu.op_reg_reg_reg(A_ADD,r,r,b));
          end
        else
          internalerror(2016060504);
      end;


{*****************************************************************************
                            Compare and Branch
*****************************************************************************}

    procedure tcgs32.a_cmp_const_reg_label(list: TAsmList; size: tcgsize; cmp_op: topcmp; a: tcgint; reg: tregister; l: tasmlabel);
      begin
        if a=0 then
          a_cmp_reg_reg_label(list,size,cmp_op,NR_R0,reg,l)
        else
          inherited;
      end;


    procedure tcgs32.a_cmp_reg_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp; reg1,reg2 : tregister;l : tasmlabel);
      var
        ai: taicpu;
        brop: TAsmOp;
      begin
        { SLOW-32 has direct branch instructions for all comparison types.
          No need to swap operands like RISC-V does.
          reg2 is the left operand, reg1 is the right operand in FPC convention:
            cmp_op is "reg2 <op> reg1" }
        case cmp_op of
          OC_EQ:  brop:=A_BEQ;
          OC_NE:  brop:=A_BNE;
          OC_LT:  brop:=A_BLT;
          OC_GTE: brop:=A_BGE;
          OC_GT:  brop:=A_BGT;
          OC_LTE: brop:=A_BLE;
          OC_B:   brop:=A_BLTU;
          OC_AE:  brop:=A_BGEU;
          OC_A:   brop:=A_BGTU;
          OC_BE:  brop:=A_BLEU;
        else
          internalerror(2016060503);
        end;

        ai:=taicpu.op_reg_reg_sym_ofs(brop,reg2,reg1,l,0);
        ai.is_jmp:=true;
        list.concat(ai);
      end;


{*****************************************************************************
                            Jump operations
*****************************************************************************}

    procedure tcgs32.a_jmp_name(list : TAsmList;const s : string);
      var
        href: treference;
      begin
        { SLOW-32: Use TAIL pseudo-instruction (like tail call, but no return) }
        reference_reset_symbol(href,current_asmdata.RefAsmSymbol(s,AT_FUNCTION),0,0,[]);
        href.refaddr:=addr_full;
        list.concat(taicpu.op_ref(A_TAIL,href));
      end;


    procedure tcgs32.a_jmp_always(list : TAsmList;l: tasmlabel);
      var
        ai: taicpu;
      begin
        { JAL r0, label -- unconditional jump (r0 discards link) }
        ai:=taicpu.op_reg_sym(A_JAL,NR_R0,l);
        ai.is_jmp:=true;
        list.concat(ai);
       end;


{*****************************************************************************
                          Multiply helpers
*****************************************************************************}

    procedure tcgs32.a_mul_reg_reg_pair(list: TAsmList;size: tcgsize; src1,src2,dstlo,dsthi: tregister);
      var
        op: tasmop;
      begin
        case size of
          OS_INT:  op:=A_MULHU;
          OS_SINT: op:=A_MULH;
        else
          InternalError(2014061501);
        end;
        if (dsthi<>NR_NO) then
          list.concat(taicpu.op_reg_reg_reg(op,dsthi,src1,src2));
        { low word is always unsigned }
        if (dstlo<>NR_NO) then
          list.concat(taicpu.op_reg_reg_reg(A_MUL,dstlo,src1,src2));
      end;


{*****************************************************************************
                            Block copy
*****************************************************************************}

    procedure tcgs32.g_concatcopy(list : TAsmList;const source,dest : treference;len : tcgint);
      var
        tmpreg1, hreg, countreg: TRegister;
        src, dst, src2, dst2: TReference;
        lab:      tasmlabel;
        Count, count2: aint;

      function reference_is_reusable(const ref: treference): boolean;
        begin
          result:=(ref.base<>NR_NO) and (ref.index=NR_NO) and
             (ref.symbol=nil) and
             is_imm12(ref.offset);
        end;

      begin
        src2:=source;
        fixref(list,src2);

        dst2:=dest;
        fixref(list,dst2);

        if len > high(longint) then
          internalerror(2002072704);

        { anybody wants to determine a good value here :)? }
        if (len > 100) and
           assigned(current_procinfo) and
           (pi_do_call in current_procinfo.flags) then
          g_concatcopy_move(list, src2, dst2, len)
        else
        begin
          Count := len div 4;
          if (count<=4) and reference_is_reusable(src2) then
            src:=src2
          else
            begin
              reference_reset(src,sizeof(aint),[]);
              { load the address of src2 into src.base }
              src.base := GetAddressRegister(list);
              a_loadaddr_ref_reg(list, src2, src.base);
            end;
          if (count<=4) and reference_is_reusable(dst2) then
            dst:=dst2
          else
            begin
              reference_reset(dst,sizeof(aint),[]);
              { load the address of dst2 into dst.base }
              dst.base := GetAddressRegister(list);
              a_loadaddr_ref_reg(list, dst2, dst.base);
            end;
          { generate a loop }
          if Count > 4 then
          begin
            countreg := GetIntRegister(list, OS_INT);
            tmpreg1  := GetIntRegister(list, OS_INT);
            a_load_const_reg(list, OS_INT, Count, countreg);
            current_asmdata.getjumplabel(lab);
            a_label(list, lab);
            list.concat(taicpu.op_reg_ref(A_LDW, tmpreg1, src));
            list.concat(taicpu.op_reg_ref(A_STW, tmpreg1, dst));
            list.concat(taicpu.op_reg_reg_const(A_ADDI, src.base, src.base, 4));
            list.concat(taicpu.op_reg_reg_const(A_ADDI, dst.base, dst.base, 4));
            list.concat(taicpu.op_reg_reg_const(A_ADDI, countreg, countreg, -1));
            a_cmp_reg_reg_label(list,OS_INT,OC_GT,NR_R0,countreg,lab);
            len := len mod 4;
          end;
          { unrolled loop }
          Count := len div 4;
          if Count > 0 then
          begin
            tmpreg1 := GetIntRegister(list, OS_INT);
            for count2 := 1 to Count do
            begin
              list.concat(taicpu.op_reg_ref(A_LDW, tmpreg1, src));
              list.concat(taicpu.op_reg_ref(A_STW, tmpreg1, dst));
              Inc(src.offset, 4);
              Inc(dst.offset, 4);
            end;
            len := len mod 4;
          end;
          if (len and 4) <> 0 then
          begin
            hreg := GetIntRegister(list, OS_INT);
            a_load_ref_reg(list, OS_32, OS_32, src, hreg);
            a_load_reg_ref(list, OS_32, OS_32, hreg, dst);
            Inc(src.offset, 4);
            Inc(dst.offset, 4);
          end;
          { copy the leftovers }
          if (len and 2) <> 0 then
          begin
            hreg := GetIntRegister(list, OS_INT);
            a_load_ref_reg(list, OS_16, OS_16, src, hreg);
            a_load_reg_ref(list, OS_16, OS_16, hreg, dst);
            Inc(src.offset, 2);
            Inc(dst.offset, 2);
          end;
          if (len and 1) <> 0 then
          begin
            hreg := GetIntRegister(list, OS_INT);
            a_load_ref_reg(list, OS_8, OS_8, src, hreg);
            a_load_reg_ref(list, OS_8, OS_8, hreg, dst);
          end;
        end;
      end;


{*****************************************************************************
                       Procedure entry/exit
*****************************************************************************}

    procedure tcgs32.g_proc_entry(list: TAsmList; localsize: longint; nostackframe: boolean);
      var
        regs: tcpuregisterset;
        r: TSuperRegister;
        href: treference;
        stackcount, stackAdjust: longint;
      begin
        if not(nostackframe) then
          begin
            a_reg_alloc(list,NR_STACK_POINTER_REG);
            if current_procinfo.framepointer<>NR_STACK_POINTER_REG then
              a_reg_alloc(list,NR_FRAME_POINTER_REG);

            { Integer registers to save }
            regs:=rg[R_INTREGISTER].used_in_proc-paramanager.get_volatile_registers_int(pocall_stdcall);

            if current_procinfo.framepointer<>NR_STACK_POINTER_REG then
              regs:=regs+[RS_FRAME_POINTER_REG,RS_RETURN_ADDRESS_REG];

            if (pi_do_call in current_procinfo.flags) then
              regs:=regs+[RS_RETURN_ADDRESS_REG];

            stackcount:=0;
            for r:=RS_R0 to RS_R31 do
              if r in regs then
                inc(stackcount,sizeof(pint));

            { No FPU registers to save in SLOW-32 }

            inc(localsize,stackcount);
            if not is_imm12(-localsize) then
              begin
                if not (RS_RETURN_ADDRESS_REG in regs) then
                  begin
                    include(regs,RS_RETURN_ADDRESS_REG);
                    inc(localsize,sizeof(pint));
                  end;
              end;

            reference_reset_base(href,NR_STACK_POINTER_REG,stackcount,ctempposinvalid,0,[]);

            stackAdjust:=0;
            if stackcount>0 then
              begin
                list.concat(taicpu.op_reg_reg_const(A_ADDI,NR_STACK_POINTER_REG,NR_STACK_POINTER_REG,-stackcount));
                stackAdjust:=stackcount;
                dec(localsize,stackcount);
              end;

            for r:=RS_R0 to RS_R31 do
              if r in regs then
                begin
                  dec(href.offset,sizeof(pint));
                  list.concat(taicpu.op_reg_ref(A_STW,newreg(R_INTREGISTER,r,R_SUBWHOLE),href));
                end;

            if current_procinfo.framepointer<>NR_STACK_POINTER_REG then
              list.concat(taicpu.op_reg_reg_const(A_ADDI,NR_FRAME_POINTER_REG,NR_STACK_POINTER_REG,stackAdjust));

            if localsize>0 then
              begin
                localsize:=align(localsize,sizeof(pint));

                if is_imm12(-localsize) then
                  list.concat(taicpu.op_reg_reg_const(A_ADDI,NR_STACK_POINTER_REG,NR_STACK_POINTER_REG,-localsize))
                else
                  begin
                    a_load_const_reg(list,OS_INT,localsize,NR_RETURN_ADDRESS_REG);
                    list.concat(taicpu.op_reg_reg_reg(A_SUB,NR_STACK_POINTER_REG,NR_STACK_POINTER_REG,NR_RETURN_ADDRESS_REG));
                  end;
              end;
          end;
      end;


    procedure tcgs32.g_proc_exit(list: TAsmList; parasize: longint; nostackframe: boolean);
      var
        r: tsuperregister;
        regs: tcpuregisterset;
        stacksize, localsize, precompensation, postcompensation: longint;
        href: treference;
      begin
        if not(nostackframe) then
          begin
            regs:=rg[R_INTREGISTER].used_in_proc-paramanager.get_volatile_registers_int(pocall_stdcall);

            if current_procinfo.framepointer<>NR_STACK_POINTER_REG then
              regs:=regs+[RS_FRAME_POINTER_REG,RS_RETURN_ADDRESS_REG];

            if (pi_do_call in current_procinfo.flags) then
              regs:=regs+[RS_RETURN_ADDRESS_REG];

            stacksize:=0;
            for r:=RS_R31 downto RS_R0 do
              if r in regs then
                inc(stacksize,sizeof(pint));

            { No FPU registers in SLOW-32 }

            localsize:=current_procinfo.calc_stackframe_size+stacksize;
            if localsize>0 then
              begin
                localsize:=align(localsize,sizeof(pint));

                if not is_imm12(-localsize) then
                  begin
                    if not (RS_RETURN_ADDRESS_REG in regs) then
                      begin
                        include(regs,RS_RETURN_ADDRESS_REG);
                        inc(localsize,sizeof(pint));
                        inc(stacksize,sizeof(pint));
                      end;
                  end;
              end;

            if not is_imm12(localsize) then
              begin
                precompensation:=localsize-2032;
                postcompensation:=localsize-precompensation;
              end
            else
              begin
                precompensation:=0;
                postcompensation:=localsize;
              end;

            reference_reset_base(href,NR_STACK_POINTER_REG,postcompensation-stacksize,ctempposinvalid,0,[]);

            if precompensation>0 then
              begin
                if is_imm12(precompensation) then
                  list.concat(taicpu.op_reg_reg_const(A_ADDI,NR_STACK_POINTER_REG,NR_STACK_POINTER_REG,precompensation))
                else
                  begin
                    { use R5 (a2) as temporary register as it is caller-saved }
                    a_load_const_reg(list,OS_INT,precompensation,NR_R5);
                    list.concat(taicpu.op_reg_reg_reg(A_ADD,NR_STACK_POINTER_REG,NR_STACK_POINTER_REG,NR_R5));
                  end;
              end;

            { No FPU register restore in SLOW-32 }

            for r:=RS_R31 downto RS_R0 do
              if r in regs then
                begin
                  list.concat(taicpu.op_reg_ref(A_LDW,newreg(R_INTREGISTER,r,R_SUBWHOLE),href));
                  inc(href.offset,sizeof(pint));
                end;

            if postcompensation>0 then
              list.concat(taicpu.op_reg_reg_const(A_ADDI,NR_STACK_POINTER_REG,NR_STACK_POINTER_REG,postcompensation));
          end;

        { Return: JALR r0, r31, 0 }
        list.concat(taicpu.op_reg_reg(A_JALR,NR_R0,NR_RETURN_ADDRESS_REG));
      end;


{*****************************************************************************
                         Register save/restore
*****************************************************************************}

    procedure tcgs32.g_save_registers(list: TAsmList);
      begin
        { handled in g_proc_entry }
      end;


    procedure tcgs32.g_restore_registers(list: TAsmList);
      begin
        { handled in g_proc_exit }
      end;


{*****************************************************************************
                              Profiling
*****************************************************************************}

    procedure tcgs32.g_profilecode(list: TAsmList);
      begin
        { No profiling support for SLOW-32 yet }
        internalerror(2018092201);
      end;


{*****************************************************************************
                          Overflow checking
*****************************************************************************}

    procedure tcgs32.g_overflowcheck_loc(list: TAsmList; const Loc: tlocation; def: tdef; ovloc: tlocation);
      begin
        { no overflow checking yet generated }
        if ovloc.loc=LOC_VOID then
          exit;
        if ovloc.loc<>LOC_JUMP then
          Internalerror(2025102001);
        a_label(list,ovloc.truelabel);
        a_call_name(list,'FPC_OVERFLOW',false);
        a_label(list,ovloc.falselabel);
      end;


    procedure tcgs32.g_overflowcheck(list: TAsmList; const Loc: tlocation; def: tdef);
      begin
      end;


{*****************************************************************************
                          FPU stubs (no hardware FPU)
*****************************************************************************}

    procedure tcgs32.a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tcgsize; reg1, reg2: tregister);
      begin
        { SLOW-32 has no FPU register file, floating point is handled by soft-float }
        internalerror(2026021301);
      end;


    procedure tcgs32.a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tcgsize; const ref: treference; reg: tregister);
      begin
        internalerror(2026021302);
      end;


    procedure tcgs32.a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tcgsize; reg: tregister; const ref: treference);
      begin
        internalerror(2026021303);
      end;


    procedure tcgs32.g_check_for_fpu_exception(list: TAsmList;force,clear : boolean);
      begin
        { No FPU exceptions possible on SLOW-32 }
      end;


{*****************************************************************************
                              fixref
*****************************************************************************}

    function tcgs32.fixref(list: TAsmList; var ref: treference): boolean;
      var
        tmpreg: TRegister;
        href: treference;
      begin
        result:=true;

        if assigned(ref.symbol) then
          begin
            { Use LA pseudo-instruction for symbol references }
            reference_reset_symbol(href,ref.symbol,0,0,[]);
            href.refaddr:=addr_full;
            ref.symbol:=nil;

            tmpreg:=getintregister(list,OS_ADDR);

            list.concat(taicpu.op_reg_ref(A_LA,tmpreg,href));

            if (ref.index<>NR_NO) and
               (ref.base<>NR_NO) then
              begin
                a_op_reg_reg(list,OP_ADD,OS_INT,ref.base,tmpreg);
                ref.base:=tmpreg;
              end
            else if (ref.index=NR_NO) and
               (ref.base<>NR_NO) then
              ref.index:=tmpreg
            else
              ref.base:=tmpreg;
          end
        else if (ref.index=NR_NO) and
                (ref.base=NR_NO) then
          begin
            tmpreg:=getintregister(list,OS_INT);

            a_load_const_reg(list, OS_ADDR,ref.offset,tmpreg);

            reference_reset_base(ref,tmpreg,0,ctempposinvalid,ref.alignment,ref.volatility);
          end;

        if (ref.index<>NR_NO) and
           (ref.base=NR_NO) then
          begin
            ref.base:=ref.index;
            ref.index:=NR_NO;
          end;

        if not is_imm12(ref.offset) then
          begin
            tmpreg:=getintregister(list,OS_INT);
            a_load_const_reg(list,OS_INT,ref.offset,tmpreg);

            ref.offset:=0;

            if (ref.index<>NR_NO) and
               (ref.base<>NR_NO) then
              begin
                a_op_reg_reg(list,OP_ADD,OS_INT,ref.index,tmpreg);
                ref.index:=tmpreg;
              end
            else
              ref.index:=tmpreg;
          end;

        if (ref.index<>NR_NO) and
           (ref.base<>NR_NO) then
          begin
            tmpreg:=getaddressregister(list);
            list.concat(taicpu.op_reg_reg_reg(A_ADD,tmpreg,ref.base,ref.index));
            ref.base:=tmpreg;
            ref.index:=NR_NO;
          end;
      end;


    procedure tcgs32.maybeadjustresult(list: TAsmList; op: topcg; size: tcgsize; dst: tregister);
      const
        overflowops = [OP_MUL,OP_IMUL,OP_SHL,OP_ADD,OP_SUB,OP_NOT,OP_NEG];
      begin
        if (op in overflowops) and
           (size in [OS_8,OS_S8,OS_16,OS_S16]) then
          a_load_reg_reg(list,OS_INT,size,dst,dst)
      end;


{*****************************************************************************
                    64-bit code generator (tcg64fs32)
*****************************************************************************}

    procedure tcg64fs32.a_op64_reg_reg(list : TAsmList;op:TOpCG;size : tcgsize;regsrc,regdst : tregister64);
      var
        tmpreg1: TRegister;
      begin
        case op of
          OP_NOT:
            begin
              cg.a_op_reg_reg(list,OP_NOT,OS_32,regsrc.reglo,regdst.reglo);
              cg.a_op_reg_reg(list,OP_NOT,OS_32,regsrc.reghi,regdst.reghi);
            end;
          OP_NEG:
            begin
              tmpreg1 := cg.GetIntRegister(list, OS_INT);
              list.concat(taicpu.op_reg_reg_reg(A_SUB, regdst.reglo, NR_R0, regsrc.reglo));
              list.concat(taicpu.op_reg_reg_reg(A_SLTU, tmpreg1, NR_R0, regdst.reglo));
              list.concat(taicpu.op_reg_reg_reg(A_SUB, regdst.reghi, NR_R0, regsrc.reghi));
              list.concat(taicpu.op_reg_reg_reg(A_SUB, regdst.reghi, regdst.reghi, tmpreg1));
            end;
          else
            a_op64_reg_reg_reg(list,op,size,regsrc,regdst,regdst);
        end;
      end;


    procedure tcg64fs32.a_op64_const_reg(list : TAsmList;op:TOpCG;size : tcgsize;value : int64;reg : tregister64);
      begin
        a_op64_const_reg_reg(list,op,size,value,reg,reg);
      end;


    procedure tcg64fs32.a_op64_reg_reg_reg(list: TAsmList;op:TOpCG;size : tcgsize;regsrc1,regsrc2,regdst : tregister64);
      var
        signed: Boolean;
        tmplo, carry, tmphi, hreg: TRegister;
      begin
        case op of
          OP_AND,OP_OR,OP_XOR:
            begin
              cg.a_op_reg_reg_reg(list,op,OS_32,regsrc1.reglo,regsrc2.reglo,regdst.reglo);
              cg.a_op_reg_reg_reg(list,op,OS_32,regsrc1.reghi,regsrc2.reghi,regdst.reghi);
            end;
          OP_ADD:
            begin
              signed:=(size in [OS_S64]);
              tmplo := cg.GetIntRegister(list,OS_S32);
              carry := cg.GetIntRegister(list,OS_S32);
              // destreg.reglo could be regsrc1.reglo or regsrc2.reglo
              list.concat(taicpu.op_reg_reg_reg(A_ADD, tmplo, regsrc2.reglo, regsrc1.reglo));
              list.concat(taicpu.op_reg_reg_reg(A_SLTU, carry, tmplo, regsrc2.reglo));
              cg.a_load_reg_reg(list,OS_INT,OS_INT,tmplo,regdst.reglo);
              if signed then
                begin
                  list.concat(taicpu.op_reg_reg_reg(A_ADD, regdst.reghi, regsrc2.reghi, regsrc1.reghi));
                  list.concat(taicpu.op_reg_reg_reg(A_ADD, regdst.reghi, regdst.reghi, carry));
                end
              else
                begin
                  tmphi:=cg.GetIntRegister(list,OS_INT);
                  hreg:=cg.GetIntRegister(list,OS_INT);
                  cg.a_load_const_reg(list,OS_INT,$80000000,hreg);
                  // first add carry to one of the addends
                  list.concat(taicpu.op_reg_reg_reg(A_ADD, tmphi, regsrc2.reghi, carry));
                  list.concat(taicpu.op_reg_reg_reg(A_SLTU, carry, tmphi, regsrc2.reghi));
                  list.concat(taicpu.op_reg_reg_reg(A_SUB, carry, hreg, carry));
                  // then add another addend
                  list.concat(taicpu.op_reg_reg_reg(A_ADD, regdst.reghi, tmphi, regsrc1.reghi));
                end;
            end;
          OP_SUB:
            begin
              signed:=(size in [OS_S64]);
              tmplo := cg.GetIntRegister(list,OS_S32);
              carry := cg.GetIntRegister(list,OS_S32);
              // destreg.reglo could be regsrc1.reglo or regsrc2.reglo
              list.concat(taicpu.op_reg_reg_reg(A_SUB, tmplo, regsrc2.reglo, regsrc1.reglo));
              list.concat(taicpu.op_reg_reg_reg(A_SLTU, carry, regsrc2.reglo,tmplo));
              cg.a_load_reg_reg(list,OS_INT,OS_INT,tmplo,regdst.reglo);
              if signed then
                begin
                  list.concat(taicpu.op_reg_reg_reg(A_SUB, regdst.reghi, regsrc2.reghi, regsrc1.reghi));
                  list.concat(taicpu.op_reg_reg_reg(A_SUB, regdst.reghi, regdst.reghi, carry));
                end
              else
                begin
                  tmphi:=cg.GetIntRegister(list,OS_INT);
                  hreg:=cg.GetIntRegister(list,OS_INT);
                  cg.a_load_const_reg(list,OS_INT,$80000000,hreg);
                  // first subtract the carry...
                  list.concat(taicpu.op_reg_reg_reg(A_SUB, tmphi, regsrc2.reghi, carry));
                  list.concat(taicpu.op_reg_reg_reg(A_SLTU, carry, regsrc2.reghi, tmphi));
                  list.concat(taicpu.op_reg_reg_reg(A_SUB, carry, hreg, carry));
                  // ...then the subtrahend
                  list.concat(taicpu.op_reg_reg_reg(A_SUB, regdst.reghi, tmphi, regsrc1.reghi));
                end;
            end;
          else
            internalerror(2002072801);
        end;
      end;


    procedure tcg64fs32.a_op64_const_reg_reg(list: TAsmList;op:TOpCG;size : tcgsize;value : int64;regsrc,regdst : tregister64);
      var
        tmplo,carry: TRegister;
        hisize: tcgsize;
      begin
        carry:=NR_NO;
        if (size in [OS_S64]) then
          hisize:=OS_S32
        else
          hisize:=OS_32;

        case op of
          OP_AND,OP_OR,OP_XOR:
            begin
              cg.a_op_const_reg_reg(list,op,OS_32,aint(lo(value)),regsrc.reglo,regdst.reglo);
              cg.a_op_const_reg_reg(list,op,OS_32,aint(hi(value)),regsrc.reghi,regdst.reghi);
            end;

          OP_ADD:
            begin
              if lo(value)<>0 then
                begin
                  tmplo:=cg.GetIntRegister(list,OS_32);
                  carry:=cg.GetIntRegister(list,OS_32);

                  if is_imm12(aint(lo(value))) then
                    list.concat(taicpu.op_reg_reg_const(A_ADDI,tmplo,regsrc.reglo,aint(lo(value))))
                  else
                    begin
                      cg.a_load_const_reg(list,OS_INT,aint(lo(value)),tmplo);
                      list.concat(taicpu.op_reg_reg_reg(A_ADD,tmplo,tmplo,regsrc.reglo))
                    end;
                  list.concat(taicpu.op_reg_reg_reg(A_SLTU,carry,tmplo,regsrc.reglo));
                  cg.a_load_reg_reg(list,OS_32,OS_32,tmplo,regdst.reglo);
                end
              else
                cg.a_load_reg_reg(list,OS_32,OS_32,regsrc.reglo,regdst.reglo);

              cg.a_op_const_reg_reg(list,OP_ADD,hisize,aint(hi(value)),regsrc.reghi,regdst.reghi);
              if carry<>NR_NO then
                cg.a_op_reg_reg_reg(list,OP_ADD,hisize,carry,regdst.reghi,regdst.reghi);
            end;

          OP_SUB:
            begin
              carry:=NR_NO;
              if lo(value)<>0 then
                begin
                  tmplo:=cg.GetIntRegister(list,OS_32);
                  carry:=cg.GetIntRegister(list,OS_32);

                  if {$ifdef AVOID_OVERFLOW} (abs(value) <= max_12_bit) and {$endif} is_imm12(-aint(lo(value))) then
                    list.concat(taicpu.op_reg_reg_const(A_ADDI,tmplo,regsrc.reglo,-aint(lo(value))))
                  else
                    begin
                      cg.a_load_const_reg(list,OS_INT,aint(lo(value)),tmplo);
                      list.concat(taicpu.op_reg_reg_reg(A_SUB,tmplo,regsrc.reglo,tmplo))
                    end;
                  list.concat(taicpu.op_reg_reg_reg(A_SLTU,carry,regsrc.reglo,tmplo));
                  cg.a_load_reg_reg(list,OS_32,OS_32,tmplo,regdst.reglo);
                end
              else
                cg.a_load_reg_reg(list,OS_32,OS_32,regsrc.reglo,regdst.reglo);

              cg.a_op_const_reg_reg(list,OP_SUB,hisize,aint(hi(value)),regsrc.reghi,regdst.reghi);
              if carry<>NR_NO then
                cg.a_op_reg_reg_reg(list,OP_SUB,hisize,carry,regdst.reghi,regdst.reghi);
            end;
        else
          InternalError(2013050301);
        end;
      end;


    procedure tcg64fs32.a_load64_ref_cgpara(list : TAsmList;const r : treference;const paraloc : tcgpara);
      var
        hreg64 : tregister64;
      begin
        { Override this function to prevent loading the reference twice.
          Use here some extra registers, but those are optimized away by the RA }
        hreg64.reglo:=cg.GetIntRegister(list,OS_32);
        hreg64.reghi:=cg.GetIntRegister(list,OS_32);
        a_load64_ref_reg(list,r,hreg64);
        a_load64_reg_cgpara(list,hreg64,paraloc);
      end;


    procedure tcg64fs32.a_load64_reg_ref(list : TAsmList;reg : tregister64;const ref : treference);
      var
        tmpref: treference;
      begin
        { Override this function to prevent loading the reference twice }
        tmpref:=ref;
        tcgs32(cg).fixref(list,tmpref);
        cg.a_load_reg_ref(list,OS_32,OS_32,reg.reglo,tmpref);
        inc(tmpref.offset,4);
        cg.a_load_reg_ref(list,OS_32,OS_32,reg.reghi,tmpref);
      end;


    procedure tcg64fs32.a_load64_ref_reg(list : TAsmList;const ref : treference;reg : tregister64);
      var
        tmpref: treference;
      begin
        { Override this function to prevent loading the reference twice }
        tmpref:=ref;
        tcgs32(cg).fixref(list,tmpref);
        cg.a_load_ref_reg(list,OS_32,OS_32,tmpref,reg.reglo);
        inc(tmpref.offset,4);
        cg.a_load_ref_reg(list,OS_32,OS_32,tmpref,reg.reghi);
      end;


{*****************************************************************************
                          create_codegen
*****************************************************************************}

    procedure create_codegen;
      begin
        cg := tcgs32.create;
        cg64 :=tcg64fs32.create;
      end;

end.
