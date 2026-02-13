{
    Copyright (c) 2000-2002 by Florian Klaempfl and Jonas Maebe

    Code generation for add nodes on the SLOW-32

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
unit ns32add;

{$i fpcdefs.inc}

  interface

    uses
      node, nadd, ncgadd, aasmbase, cpubase,
      cgbase;

    type
      ts32addnode = class(tcgaddnode)
        function pass_1: tnode; override;
      private
        procedure cmp64_le(left_reg, right_reg: TRegister64; unsigned: boolean);
        procedure cmp64_lt(left_reg, right_reg: TRegister64; unsigned: boolean);
      protected
        procedure Cmp(signed,is_smallset: boolean);

        function use_mul_helper: boolean; override;
        function use_generic_mul32to64: boolean; override;

        procedure second_cmpsmallset;override;
        procedure second_cmpordinal;override;
        procedure second_cmp64bit; override;

        procedure second_addordinal; override;

        procedure pass_left_and_right;
      end;

  implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,paramgr,procinfo,
      aasmtai,aasmdata,aasmcpu,defutil,
      cgcpu,cgutils,cpuinfo,
      cpupara,
      ncon,nset,
      hlcgobj, ncgutil,cgobj,
      pass_1,pass_2;

{$undef AVOID_OVERFLOW}
{$ifopt Q+}
  {$define AVOID_OVERFLOW}
  const
     low_value = low(longint);
{$endif}

    procedure ts32addnode.Cmp(signed,is_smallset: boolean);
      var
        flabel,tlabel: tasmlabel;
        allow_constant : boolean;
      begin
        pass_left_right;

        allow_constant:=(not is_smallset) or not (nodetype in [lten,gten]);

        force_reg_left_right(true,allow_constant);

        if nf_swapped in flags then
          swapleftright;

        location_reset(location,LOC_REGISTER,OS_INT);
        location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);

        case nodetype of
          equaln:
            begin
              if not (left.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

              if (right.location.loc=LOC_CONSTANT) and
                 {$ifdef AVOID_OVERFLOW} ((right.location.value = low_value) or {$endif}
                 (not is_imm12(-right.location.value)) {$ifdef AVOID_OVERFLOW}){$endif} then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,false);

              if right.location.loc=LOC_CONSTANT then
                begin
                  if right.location.value=0 then
                    { SEQ dst, left, r0 }
                    current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_SEQ,location.register,left.location.register,NR_R0))
                  else
                    begin
                      { SUB tmp, left, const; SEQ dst, tmp, r0 }
                      current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_const(A_ADDI,location.register,left.location.register,-right.location.value));
                      current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_SEQ,location.register,location.register,NR_R0));
                    end;
                end
              else
                current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_SEQ,location.register,left.location.register,right.location.register));
            end;
          unequaln:
            begin
              if not (left.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

              if (right.location.loc=LOC_CONSTANT) and
                 {$ifdef AVOID_OVERFLOW} ((right.location.value = low_value) or {$endif}
                 (not is_imm12(-right.location.value)) {$ifdef AVOID_OVERFLOW}){$endif} then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,false);

              if right.location.loc=LOC_CONSTANT then
                begin
                  if right.location.value=0 then
                    { SNE dst, left, r0 }
                    current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_SNE,location.register,left.location.register,NR_R0))
                  else
                    begin
                      current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_const(A_ADDI,location.register,left.location.register,-right.location.value));
                      current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_SNE,location.register,location.register,NR_R0));
                    end;
                end
              else
                current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_SNE,location.register,left.location.register,right.location.register));
            end;
          ltn:
            begin
              if not (left.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

              if (right.location.loc=LOC_CONSTANT) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,false);

              if signed then
                current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_SLT,location.register,left.location.register,right.location.register))
              else
                current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_SLTU,location.register,left.location.register,right.location.register));
            end;
          gtn:
            begin
              if not (right.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,false);

              if (left.location.loc=LOC_CONSTANT) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

              { gt(a,b) = lt(b,a) }
              if signed then
                current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_SGT,location.register,left.location.register,right.location.register))
              else
                current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_SGTU,location.register,left.location.register,right.location.register));
            end;
          lten:
            begin
              if not (right.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,false);

              if not (left.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

              if is_smallset then
                begin
                  { for sets: left <= right means (left AND right) = left }
                  current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_AND,right.location.register,right.location.register,left.location.register));
                  current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_SEQ,location.register,left.location.register,right.location.register));
                end
              else
                begin
                  { le(a,b) = not lt(b,a) }
                  if signed then
                    current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_SLE,location.register,left.location.register,right.location.register))
                  else
                    current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_SLEU,location.register,left.location.register,right.location.register));
                end;
            end;
          gten:
            begin
              if not (left.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

              if not (right.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,false);

              if is_smallset then
                begin
                  { for sets: left >= right means (left AND right) = right }
                  current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_AND,left.location.register,right.location.register,left.location.register));
                  current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_SEQ,location.register,left.location.register,right.location.register));
                end
              else
                begin
                  { ge(a,b) = not lt(a,b) }
                  if signed then
                    current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_SGE,location.register,left.location.register,right.location.register))
                  else
                    current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_SGEU,location.register,left.location.register,right.location.register));
                end;
            end;
        else
          Internalerror(2016061101);
        end;
      end;


    function ts32addnode.use_mul_helper: boolean;
      begin
        if (nodetype=muln) and not (CPUS32_HAS_MUL in cpu_capabilities[current_settings.cputype]) then
          result:=true
        else
          Result:=inherited use_mul_helper;
      end;


    procedure ts32addnode.second_cmpsmallset;
      begin
        Cmp(false,true);
      end;


    procedure ts32addnode.second_cmpordinal;
      var
        unsigned: Boolean;
      begin
        unsigned:=not(is_signed(left.resultdef)) or
                  not(is_signed(right.resultdef));

        Cmp(not unsigned,false);
      end;


    procedure ts32addnode.second_addordinal;
      var
        unsigned: boolean;
      begin
        { 32x32->64 multiplication }
        if (nodetype=muln) and
           is_32bit(left.resultdef) and
           is_32bit(right.resultdef) and
           is_64bit(resultdef) then
          begin
            unsigned:=not(is_signed(left.resultdef)) or
                      not(is_signed(right.resultdef));
            pass_left_right;
            force_reg_left_right(true,true);
            { force_reg_left_right can leave right as a LOC_CONSTANT }
            if right.location.loc=LOC_CONSTANT then
              hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,true);
            location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
            location.register:=cg.getintregister(current_asmdata.CurrAsmList,def_cgsize(resultdef));
            current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_MUL,location.register,left.location.register,right.location.register));
          end
        else
          inherited second_addordinal;
      end;


    function ts32addnode.pass_1: tnode;
      begin
        if (nodetype=muln) and
           (left.resultdef.typ=orddef) and (left.resultdef.typ=orddef) and
           (CPUS32_HAS_MUL in cpu_capabilities[current_settings.cputype])
           and (not (is_64bit(left.resultdef) or
                     is_64bit(right.resultdef)))
           then
          begin
            result:=nil;

            firstpass(left);
            firstpass(right);

            expectloc:=LOC_REGISTER;
          end
        else if (nodetype=muln) and
           not (CPUS32_HAS_MUL in cpu_capabilities[current_settings.cputype]) and
           (is_64bit(left.resultdef) or
            is_64bit(right.resultdef)) then
          begin
            result:=first_add64bitint;
          end
        else
          Result:=inherited pass_1;

        { if the result is not nil, a new node has been generated and the current node will be discarded }
        if Result=nil then
          begin
            { SLOW-32 has no hardware FPU, no special handling needed }
          end;

        if expectloc=LOC_FLAGS then
          expectloc:=LOC_REGISTER;
        if (expectloc=LOC_JUMP)
           and (not (is_64bit(left.resultdef) or
                     is_64bit(right.resultdef)))
	  and (nodetype in [equaln, unequaln, ltn, lten, gtn, gten]) then
          expectloc:=LOC_REGISTER;
      end;


    procedure ts32addnode.pass_left_and_right;
      begin
        { calculate the operator which is more difficult }
        firstcomplex(self);

        { in case of constant put it to the left }
        if (left.nodetype=ordconstn) then
         swapleftright;

        secondpass(left);
        secondpass(right);
      end;


    const
      cmpops: array[boolean] of TOpCmp = (OC_LT,OC_B);

    procedure ts32addnode.cmp64_lt(left_reg, right_reg: TRegister64;unsigned: boolean);
      begin
        cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,cmpops[unsigned],right_reg.reghi,left_reg.reghi,location.truelabel);
        cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_NE,left_reg.reghi,right_reg.reghi,location.falselabel);
        cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_B,right_reg.reglo,left_reg.reglo,location.truelabel);
        cg.a_jmp_always(current_asmdata.CurrAsmList,location.falselabel);
      end;


    procedure ts32addnode.cmp64_le(left_reg, right_reg: TRegister64;unsigned: boolean);
      begin
        cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,cmpops[unsigned],left_reg.reghi,right_reg.reghi,location.falselabel);
        cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_NE,left_reg.reghi,right_reg.reghi,location.truelabel);
        cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_B,left_reg.reglo,right_reg.reglo,location.falselabel);
        cg.a_jmp_always(current_asmdata.CurrAsmList,location.truelabel);
      end;

    function ts32addnode.use_generic_mul32to64: boolean;
      begin
        result:=true;
      end;

    procedure ts32addnode.second_cmp64bit;
      var
        truelabel,
        falselabel: tasmlabel;
        unsigned: boolean;
        left_reg,right_reg: TRegister64;
      begin
        current_asmdata.getjumplabel(truelabel);
        current_asmdata.getjumplabel(falselabel);
        location_reset_jump(location,truelabel,falselabel);

        pass_left_right;
        force_reg_left_right(true,true);

        unsigned:=not(is_signed(left.resultdef)) or
                  not(is_signed(right.resultdef));

        left_reg:=left.location.register64;
        if (right.location.loc=LOC_CONSTANT) then
          begin
            if lo(right.location.value64)=0 then
              right_reg.reglo:=NR_R0
            else
              begin
                right_reg.reglo:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,lo(right.location.value64),right_reg.reglo);
              end;
            if hi(right.location.value64)=0 then
              right_reg.reghi:=NR_R0
            else
              begin
                right_reg.reghi:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,hi(right.location.value64),right_reg.reghi);
              end;
          end
        else
          right_reg:=right.location.register64;

        case NodeType of
          equaln:
            begin
              cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_NE,left_reg.reghi,right_reg.reghi,location.falselabel);
              cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_NE,left_reg.reglo,right_reg.reglo,location.falselabel);
              cg.a_jmp_always(current_asmdata.CurrAsmList,location.truelabel);
            end;
          unequaln:
            begin
              cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_NE,left_reg.reghi,right_reg.reghi,location.truelabel);
              cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_NE,left_reg.reglo,right_reg.reglo,location.truelabel);
              cg.a_jmp_always(current_asmdata.CurrAsmList,location.falselabel);
            end;
        else
          if nf_swapped in flags then
            case NodeType of
              ltn:
                cmp64_lt(right_reg, left_reg,unsigned);
              lten:
                cmp64_le(right_reg, left_reg,unsigned);
              gtn:
                cmp64_lt(left_reg, right_reg,unsigned);
              gten:
                cmp64_le(left_reg, right_reg,unsigned);
              else
                internalerror(2019051034);
            end
          else
            case NodeType of
              ltn:
                cmp64_lt(left_reg, right_reg,unsigned);
              lten:
                cmp64_le(left_reg, right_reg,unsigned);
              gtn:
                cmp64_lt(right_reg, left_reg,unsigned);
              gten:
                cmp64_le(right_reg, left_reg,unsigned);
              else
                internalerror(2019051033);
            end;
        end;
      end;

begin
   caddnode:=ts32addnode;
end.
