{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate SLOW-32 assembler for type converting nodes

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
unit ns32cnv;

{$i fpcdefs.inc}

interface

    uses
      node,ncnv,ncgcnv;

    type
       ts32typeconvnode = class(tcgtypeconvnode)
         protected
         { procedure second_int_to_int;override; }
         { procedure second_string_to_string;override; }
         { procedure second_cstring_to_pchar;override; }
         { procedure second_string_to_chararray;override; }
         { procedure second_array_to_pointer;override; }
          function first_int_to_real: tnode; override;
          function first_real_to_real: tnode; override;
         { procedure second_pointer_to_array;override; }
         { procedure second_chararray_to_string;override; }
         { procedure second_char_to_string;override; }
          procedure second_int_to_real;override;
          procedure second_real_to_real;override;
         { procedure second_cord_to_pointer;override; }
         { procedure second_proc_to_procvar;override; }
         { procedure second_bool_to_int;override; }
          procedure second_int_to_bool;override;
         { procedure second_set_to_set;override;  }
         { procedure second_ansistring_to_pchar;override; }
         { procedure second_pchar_to_string;override; }
         { procedure second_class_to_intf;override; }
         { procedure second_char_to_char;override; }
       end;

implementation

   uses
      verbose,globtype,globals,systems,
      symconst,symdef,symtable,aasmbase,aasmtai,aasmdata,
      defutil,symcpu,
      cgbase,cgutils,pass_1,pass_2,
      ncon,ncal,
      ncgutil,procinfo,
      cpubase,cpuinfo,aasmcpu,
      rgobj,tgobj,cgobj,cgcpu,hlcgobj;


{*****************************************************************************
                             FirstTypeConv
*****************************************************************************}

    function ts32typeconvnode.first_int_to_real: tnode;
      begin
        if is_currency(left.resultdef) then
          begin
            { currency already scaled by typecheckpass; still needs int64 path }
            left.resultdef:=s64inttype;
          end;

        if (FPUSLOW32_SINGLE in fpu_capabilities[current_settings.fputype]) and
          not(cs_fp_emulation in current_settings.moduleswitches) and
          is_single(resultdef) then
          begin
            { int32/int64 -> single via FCVT.S.W / FCVT.S.L }
            if not is_64bitint(left.resultdef) and not is_32bit(left.resultdef) then
              begin
                if is_signed(left.resultdef) then
                  inserttypeconv(left,s32inttype)
                else
                  inserttypeconv(left,u32inttype);
                firstpass(left);
              end;
            result:=nil;
            expectloc:=LOC_FPUREGISTER;
          end
        else if (FPUSLOW32_DOUBLE in fpu_capabilities[current_settings.fputype]) and
          not(cs_fp_emulation in current_settings.moduleswitches) and
          is_double(resultdef) then
          begin
            { int32/int64 -> double via FCVT.D.W / FCVT.D.L }
            if not is_64bitint(left.resultdef) and not is_32bit(left.resultdef) then
              begin
                if is_signed(left.resultdef) then
                  inserttypeconv(left,s32inttype)
                else
                  inserttypeconv(left,u32inttype);
                firstpass(left);
              end;
            result:=nil;
            expectloc:=LOC_REGISTER;
          end
        else
          result:=inherited first_int_to_real;
      end;


    function ts32typeconvnode.first_real_to_real: tnode;
      begin
        { With -CfSLOW32, f32 lives in F-class regs and f64 in int pairs
          (def_cgsize -> OS_64). Use FCVT.D.S / FCVT.S.D instead of softfloat
          helpers; full f64 arith still stays soft until pair F-class lands. }
        if (FPUSLOW32_SINGLE in fpu_capabilities[current_settings.fputype]) and
          not(cs_fp_emulation in current_settings.moduleswitches) then
          begin
            case tfloatdef(left.resultdef).floattype of
              s32real:
                case tfloatdef(resultdef).floattype of
                  s64real:
                    begin
                      result:=nil;
                      expectloc:=LOC_REGISTER;
                    end;
                  s32real:
                    begin
                      result:=left;
                      left:=nil;
                    end;
                  else
                    internalerror(2026071503);
                end;
              s64real:
                case tfloatdef(resultdef).floattype of
                  s32real:
                    begin
                      result:=nil;
                      expectloc:=LOC_FPUREGISTER;
                    end;
                  s64real:
                    begin
                      result:=left;
                      left:=nil;
                    end;
                  else
                    internalerror(2026071502);
                end;
              else
                internalerror(2026071501);
            end;
            exit;
          end
        else
          result := inherited first_real_to_real;
      end;


{*****************************************************************************
                             SecondTypeConv
*****************************************************************************}

    procedure ts32typeconvnode.second_int_to_real;
      var
        op: TAsmOp;
        list: TAsmList;
        cgs: tcgs32;
      begin
        list:=current_asmdata.CurrAsmList;
        secondpass(left);
        if not(left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
          hlcg.location_force_reg(list,left.location,left.resultdef,left.resultdef,true);

        if is_single(resultdef) then
          begin
            location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
            location.register:=cg.getfpuregister(list,location.size);
            if is_64bitint(left.resultdef) then
              begin
                cgs:=tcgs32(cg);
                cgs.a_alloc_evenpair(list,NR_F64PAIR_A);
                cgs.a_load_reg64_evenpair(list,
                  left.location.register64.reglo,left.location.register64.reghi,NR_F64PAIR_A);
                if is_signed(left.resultdef) then
                  op:=A_FCVT_S_L
                else
                  op:=A_FCVT_S_LU;
                list.concat(taicpu.op_reg_reg(op,location.register,NR_F64PAIR_A));
                cgs.a_dealloc_evenpair(list,NR_F64PAIR_A);
              end
            else
              begin
                if is_signed(left.resultdef) then
                  op:=A_FCVT_S_W
                else
                  op:=A_FCVT_S_WU;
                list.concat(taicpu.op_reg_reg(op,location.register,left.location.register));
              end;
          end
        else if is_double(resultdef) then
          begin
            cgs:=tcgs32(cg);
            cgs.a_alloc_evenpair(list,NR_F64PAIR_A);
            if is_64bitint(left.resultdef) then
              begin
                cgs.a_alloc_evenpair(list,NR_F64PAIR_B);
                cgs.a_load_reg64_evenpair(list,
                  left.location.register64.reglo,left.location.register64.reghi,NR_F64PAIR_B);
                if is_signed(left.resultdef) then
                  op:=A_FCVT_D_L
                else
                  op:=A_FCVT_D_LU;
                list.concat(taicpu.op_reg_reg(op,NR_F64PAIR_A,NR_F64PAIR_B));
                cgs.a_dealloc_evenpair(list,NR_F64PAIR_B);
              end
            else
              begin
                if is_signed(left.resultdef) then
                  op:=A_FCVT_D_W
                else
                  op:=A_FCVT_D_WU;
                list.concat(taicpu.op_reg_reg(op,NR_F64PAIR_A,left.location.register));
              end;
            location_reset(location,LOC_REGISTER,OS_64);
            location.register64.reglo:=cg.getintregister(list,OS_32);
            location.register64.reghi:=cg.getintregister(list,OS_32);
            cgs.a_load_evenpair_reg64(list,NR_F64PAIR_A,
              location.register64.reglo,location.register64.reghi);
            cgs.a_dealloc_evenpair(list,NR_F64PAIR_A);
          end
        else
          internalerror(2026071504);
      end;


    procedure ts32typeconvnode.second_real_to_real;
      var
        pairbase, pairhi, tmpreg: tregister;
        list: TAsmList;
      begin
        list:=current_asmdata.CurrAsmList;
        secondpass(left);
        if codegenerror then
          exit;

        { Even-aligned scratch pair for f64 FCVT ops (ISA requires even base).
          r4:r5 are caller-saved and outside the F-class partition (r20-r28). }
        pairbase:=NR_R4;
        pairhi:=NR_R5;

        if is_single(left.resultdef) and is_double(resultdef) then
          begin
            { f32 (F-reg) -> f64 (int pair) via FCVT.D.S }
            hlcg.location_force_fpureg(list,left.location,left.resultdef,true);
            location_reset(location,LOC_REGISTER,OS_64);
            location.register64.reglo:=cg.getintregister(list,OS_32);
            location.register64.reghi:=cg.getintregister(list,OS_32);

            cg.a_reg_alloc(list,pairbase);
            cg.a_reg_alloc(list,pairhi);
            list.concat(taicpu.op_reg_reg(A_FCVT_D_S,pairbase,left.location.register));
            cg.a_load_reg_reg(list,OS_32,OS_32,pairbase,location.register64.reglo);
            cg.a_load_reg_reg(list,OS_32,OS_32,pairhi,location.register64.reghi);
            cg.a_reg_dealloc(list,pairhi);
            cg.a_reg_dealloc(list,pairbase);
          end
        else if is_double(left.resultdef) and is_single(resultdef) then
          begin
            { f64 (int pair) -> f32 (F-reg) via FCVT.S.D }
            hlcg.location_force_reg(list,left.location,left.resultdef,left.resultdef,true);
            location_reset(location,LOC_FPUREGISTER,OS_F32);
            location.register:=cg.getfpuregister(list,OS_F32);

            cg.a_reg_alloc(list,pairbase);
            cg.a_reg_alloc(list,pairhi);
            { Pack lo/hi into the even pair without clobbering if they alias r4/r5 }
            if (left.location.register64.reglo=pairhi) and
               (left.location.register64.reghi=pairbase) then
              begin
                tmpreg:=cg.getintregister(list,OS_32);
                cg.a_load_reg_reg(list,OS_32,OS_32,left.location.register64.reglo,tmpreg);
                cg.a_load_reg_reg(list,OS_32,OS_32,left.location.register64.reghi,pairhi);
                cg.a_load_reg_reg(list,OS_32,OS_32,tmpreg,pairbase);
              end
            else if left.location.register64.reghi=pairbase then
              begin
                cg.a_load_reg_reg(list,OS_32,OS_32,left.location.register64.reghi,pairhi);
                if left.location.register64.reglo<>pairbase then
                  cg.a_load_reg_reg(list,OS_32,OS_32,left.location.register64.reglo,pairbase);
              end
            else
              begin
                if left.location.register64.reglo<>pairbase then
                  cg.a_load_reg_reg(list,OS_32,OS_32,left.location.register64.reglo,pairbase);
                if left.location.register64.reghi<>pairhi then
                  cg.a_load_reg_reg(list,OS_32,OS_32,left.location.register64.reghi,pairhi);
              end;
            list.concat(taicpu.op_reg_reg(A_FCVT_S_D,location.register,pairbase));
            cg.a_reg_dealloc(list,pairhi);
            cg.a_reg_dealloc(list,pairbase);
          end
        else if is_single(left.resultdef) and is_single(resultdef) then
          begin
            location_copy(location,left.location);
          end
        else if is_double(left.resultdef) and is_double(resultdef) then
          begin
            location_copy(location,left.location);
          end
        else
          internalerror(2026080803);
      end;


    procedure ts32typeconvnode.second_int_to_bool;
      var
        hreg1, hreg2: tregister;
        opsize: tcgsize;
        hlabel: tasmlabel;
        newsize  : tcgsize;
        href: treference;
      begin
        secondpass(left);
        if codegenerror then
          exit;

        { Explicit typecasts from any ordinal type to a boolean type }
        { must not change the ordinal value                          }
        if (nf_explicit in flags) and
           not(left.location.loc in [LOC_FLAGS,LOC_JUMP]) then
          begin
             location_copy(location,left.location);
             newsize:=def_cgsize(resultdef);
             { change of size? change sign only if location is LOC_(C)REGISTER? Then we have to sign/zero-extend }
             if (tcgsize2size[newsize]<>tcgsize2size[left.location.size]) or
                ((newsize<>left.location.size) and (location.loc in [LOC_REGISTER,LOC_CREGISTER])) then
               hlcg.location_force_reg(current_asmdata.CurrAsmList,location,left.resultdef,resultdef,true)
             else
               location.size:=newsize;
             exit;
          end;

        location_reset(location, LOC_REGISTER, def_cgsize(resultdef));
        opsize := def_cgsize(left.resultdef);

        if (left.location.loc in [LOC_SUBSETREG,LOC_CSUBSETREG,LOC_SUBSETREF,LOC_CSUBSETREF]) then
          hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);

        case left.location.loc of
          LOC_CREFERENCE, LOC_REFERENCE, LOC_REGISTER, LOC_CREGISTER:
          begin
            if left.location.loc in [LOC_CREFERENCE, LOC_REFERENCE] then
            begin
              hreg2 := cg.getintregister(current_asmdata.CurrAsmList, opsize);
              if left.location.size in [OS_64,OS_S64] then
                begin
                  cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,left.location.reference,hreg2);
                  hreg1:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                  href:=left.location.reference;
                  inc(href.offset,4);
                  cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,href,hreg1);
                  cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_OR,OS_32,hreg1,hreg2,hreg2);
                end
                else
                  cg.a_load_ref_reg(current_asmdata.CurrAsmList, opsize, opsize, left.location.reference, hreg2);
            end
            else
              begin
                hreg2:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                if left.location.size in [OS_64,OS_S64] then
                  begin
                    hreg2:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                    cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_OR,OS_32,left.location.register64.reghi,left.location.register64.reglo,hreg2);
                   end
                 else
                   cg.a_load_reg_reg(current_asmdata.CurrAsmList,opsize,opsize,left.location.register,hreg2);
               end;
             hreg1 := cg.getintregister(current_asmdata.CurrAsmList, opsize);
             { SNE dst, src, r0: sets dst=1 if src!=0 }
             current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SNE, hreg1, hreg2, NR_R0));
          end;
          LOC_JUMP:
          begin
            hreg1 := cg.getintregister(current_asmdata.CurrAsmList, OS_INT);
            current_asmdata.getjumplabel(hlabel);
            cg.a_label(current_asmdata.CurrAsmList, left.location.truelabel);
            cg.a_load_const_reg(current_asmdata.CurrAsmList, OS_INT, 1, hreg1);
            cg.a_jmp_always(current_asmdata.CurrAsmList, hlabel);
            cg.a_label(current_asmdata.CurrAsmList, left.location.falselabel);
            cg.a_load_const_reg(current_asmdata.CurrAsmList, OS_INT, 0, hreg1);
            cg.a_label(current_asmdata.CurrAsmList, hlabel);
          end;
          LOC_FLAGS:
          begin
            Internalerror(2016060403);
          end
          else
            internalerror(10062);
        end;
        { Now hreg1 is either 0 or 1. For C booleans it must be 0 or -1. }
        if is_cbool(resultdef) then
          cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_NEG,OS_SINT,hreg1,hreg1);

        if (location.size in [OS_64,OS_S64]) then
          begin
            location.register64.reglo:=hreg1;
            location.register64.reghi:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
            if (is_cbool(resultdef)) then
             { reglo is either 0 or -1 -> reghi has to become the same }
                cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_32,OS_32,location.register64.reglo,location.register64.reghi)
             else
             { unsigned }
               cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_32,0,location.register64.reghi);
             end
             else
               location.Register := hreg1;
      end;

begin
   ctypeconvnode:=ts32typeconvnode;
end.
