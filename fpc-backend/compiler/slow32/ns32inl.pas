{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate SLOW-32 inline nodes

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
unit ns32inl;

{$i fpcdefs.inc}

interface

    uses
       cpubase,
       node,ninl,ncginl;

    type
      ts32inlinenode = class(tcginlinenode)
        { fpu_slow32 handles f32 abs/sqr/sqrt natively (FABS.S/FMUL.S/FSQRT.S).
          f64 and all other intrinsics still use softfloat helper calls. }
        function first_abs_real: tnode; override;
        function first_sqr_real: tnode; override;
        function first_sqrt_real: tnode; override;
        procedure second_abs_real; override;
        procedure second_sqr_real; override;
        procedure second_sqrt_real; override;
      protected
        function use_native_f32: boolean;
      end;

implementation

    uses
      ncal,
      cutils,globals,verbose,globtype,
      compinnr,
      aasmtai,aasmdata,aasmcpu,
      symconst,symdef,
      defutil,
      procinfo,
      cgbase,pass_2,
      cpuinfo,ncgutil,
      hlcgobj,cgutils,cgobj,rgobj,tgobj;


    function ts32inlinenode.use_native_f32: boolean;
      begin
        if left.nodetype=callparan then
          result:=is_single(tcallparanode(left).left.resultdef)
        else
          result:=is_single(left.resultdef);
        result:=result and
          (FPUSLOW32_SINGLE in fpu_capabilities[current_settings.fputype]) and
          not(cs_fp_emulation in current_settings.moduleswitches);
      end;


    function ts32inlinenode.first_abs_real: tnode;
      begin
        if use_native_f32 then
          begin
            result:=nil;
            expectloc:=LOC_FPUREGISTER;
          end
        else
          result:=inherited first_abs_real;
      end;


    function ts32inlinenode.first_sqr_real: tnode;
      begin
        if use_native_f32 then
          begin
            result:=nil;
            expectloc:=LOC_FPUREGISTER;
          end
        else
          result:=inherited first_sqr_real;
      end;


    function ts32inlinenode.first_sqrt_real: tnode;
      begin
        if use_native_f32 then
          begin
            result:=nil;
            expectloc:=LOC_FPUREGISTER;
          end
        else
          result:=inherited first_sqrt_real;
      end;


    procedure ts32inlinenode.second_abs_real;
      begin
        if not(is_single(resultdef)) then
          internalerror(2026071505);
        secondpass(left);
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
        location_reset(location,LOC_FPUREGISTER,OS_F32);
        location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FABS_S,location.register,left.location.register));
      end;


    procedure ts32inlinenode.second_sqr_real;
      begin
        if not(is_single(resultdef)) then
          internalerror(2026080801);
        secondpass(left);
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
        location_reset(location,LOC_FPUREGISTER,OS_F32);
        location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_FMUL_S,
          location.register,left.location.register,left.location.register));
      end;


    procedure ts32inlinenode.second_sqrt_real;
      begin
        if not(is_single(resultdef)) then
          internalerror(2026080802);
        secondpass(left);
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
        location_reset(location,LOC_FPUREGISTER,OS_F32);
        location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FSQRT_S,location.register,left.location.register));
      end;


begin
   cinlinenode:=ts32inlinenode;
end.
