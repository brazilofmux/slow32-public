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
        { fpu_slow32: native FABS.S / FMUL.S / FSQRT.S / FCVT.L.S for f32.
          Round and f64 stay softfloat (DBT can still intercept libcalls). }
        function first_abs_real: tnode; override;
        function first_sqr_real: tnode; override;
        function first_sqrt_real: tnode; override;
        function first_trunc_real: tnode; override;
        procedure second_abs_real; override;
        procedure second_sqr_real; override;
        procedure second_sqrt_real; override;
        procedure second_trunc_real; override;
      protected
        function use_native_f32: boolean;
        function use_native_f64: boolean;
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
      hlcgobj,cgutils,cgobj,rgobj,tgobj,cgcpu;


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


    function ts32inlinenode.use_native_f64: boolean;
      begin
        if left.nodetype=callparan then
          result:=is_double(tcallparanode(left).left.resultdef)
        else
          result:=is_double(left.resultdef);
        result:=result and
          (FPUSLOW32_DOUBLE in fpu_capabilities[current_settings.fputype]) and
          not(cs_fp_emulation in current_settings.moduleswitches);
      end;


    function ts32inlinenode.first_abs_real: tnode;
      begin
        if use_native_f32 then
          begin
            result:=nil;
            expectloc:=LOC_FPUREGISTER;
          end
        else if use_native_f64 then
          begin
            result:=nil;
            expectloc:=LOC_REGISTER;
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
        else if use_native_f64 then
          begin
            result:=nil;
            expectloc:=LOC_REGISTER;
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
        else if use_native_f64 then
          begin
            result:=nil;
            expectloc:=LOC_REGISTER;
          end
        else
          result:=inherited first_sqrt_real;
      end;


    procedure ts32inlinenode.second_abs_real;
      var
        list: TAsmList;
        cgs: tcgs32;
      begin
        list:=current_asmdata.CurrAsmList;
        secondpass(left);
        if is_single(resultdef) then
          begin
            hlcg.location_force_fpureg(list,left.location,left.resultdef,true);
            location_reset(location,LOC_FPUREGISTER,OS_F32);
            location.register:=cg.getfpuregister(list,location.size);
            list.concat(taicpu.op_reg_reg(A_FABS_S,location.register,left.location.register));
          end
        else if is_double(resultdef) then
          begin
            hlcg.location_force_reg(list,left.location,left.resultdef,left.resultdef,true);
            cgs:=tcgs32(cg);
            cgs.a_alloc_evenpair(list,NR_F64PAIR_A);
            cgs.a_load_reg64_evenpair(list,
              left.location.register64.reglo,left.location.register64.reghi,NR_F64PAIR_A);
            list.concat(taicpu.op_reg_reg(A_FABS_D,NR_F64PAIR_A,NR_F64PAIR_A));
            location_reset(location,LOC_REGISTER,OS_64);
            location.register64.reglo:=cg.getintregister(list,OS_32);
            location.register64.reghi:=cg.getintregister(list,OS_32);
            cgs.a_load_evenpair_reg64(list,NR_F64PAIR_A,
              location.register64.reglo,location.register64.reghi);
            cgs.a_dealloc_evenpair(list,NR_F64PAIR_A);
          end
        else
          internalerror(2026071505);
      end;


    procedure ts32inlinenode.second_sqr_real;
      var
        list: TAsmList;
        cgs: tcgs32;
      begin
        list:=current_asmdata.CurrAsmList;
        secondpass(left);
        if is_single(resultdef) then
          begin
            hlcg.location_force_fpureg(list,left.location,left.resultdef,true);
            location_reset(location,LOC_FPUREGISTER,OS_F32);
            location.register:=cg.getfpuregister(list,location.size);
            list.concat(taicpu.op_reg_reg_reg(A_FMUL_S,
              location.register,left.location.register,left.location.register));
          end
        else if is_double(resultdef) then
          begin
            hlcg.location_force_reg(list,left.location,left.resultdef,left.resultdef,true);
            cgs:=tcgs32(cg);
            cgs.a_alloc_evenpair(list,NR_F64PAIR_A);
            cgs.a_alloc_evenpair(list,NR_F64PAIR_C);
            cgs.a_load_reg64_evenpair(list,
              left.location.register64.reglo,left.location.register64.reghi,NR_F64PAIR_A);
            list.concat(taicpu.op_reg_reg_reg(A_FMUL_D,NR_F64PAIR_C,NR_F64PAIR_A,NR_F64PAIR_A));
            location_reset(location,LOC_REGISTER,OS_64);
            location.register64.reglo:=cg.getintregister(list,OS_32);
            location.register64.reghi:=cg.getintregister(list,OS_32);
            cgs.a_load_evenpair_reg64(list,NR_F64PAIR_C,
              location.register64.reglo,location.register64.reghi);
            cgs.a_dealloc_evenpair(list,NR_F64PAIR_C);
            cgs.a_dealloc_evenpair(list,NR_F64PAIR_A);
          end
        else
          internalerror(2026080801);
      end;


    procedure ts32inlinenode.second_sqrt_real;
      var
        list: TAsmList;
        cgs: tcgs32;
      begin
        list:=current_asmdata.CurrAsmList;
        secondpass(left);
        if is_single(resultdef) then
          begin
            hlcg.location_force_fpureg(list,left.location,left.resultdef,true);
            location_reset(location,LOC_FPUREGISTER,OS_F32);
            location.register:=cg.getfpuregister(list,location.size);
            list.concat(taicpu.op_reg_reg(A_FSQRT_S,location.register,left.location.register));
          end
        else if is_double(resultdef) then
          begin
            hlcg.location_force_reg(list,left.location,left.resultdef,left.resultdef,true);
            cgs:=tcgs32(cg);
            cgs.a_alloc_evenpair(list,NR_F64PAIR_A);
            cgs.a_load_reg64_evenpair(list,
              left.location.register64.reglo,left.location.register64.reghi,NR_F64PAIR_A);
            list.concat(taicpu.op_reg_reg(A_FSQRT_D,NR_F64PAIR_A,NR_F64PAIR_A));
            location_reset(location,LOC_REGISTER,OS_64);
            location.register64.reglo:=cg.getintregister(list,OS_32);
            location.register64.reghi:=cg.getintregister(list,OS_32);
            cgs.a_load_evenpair_reg64(list,NR_F64PAIR_A,
              location.register64.reglo,location.register64.reghi);
            cgs.a_dealloc_evenpair(list,NR_F64PAIR_A);
          end
        else
          internalerror(2026080802);
      end;


    function ts32inlinenode.first_trunc_real: tnode;
      begin
        { Trunc returns Int64; FCVT.L.S / FCVT.L.D write an even pair. }
        if use_native_f32 or use_native_f64 then
          begin
            result:=nil;
            expectloc:=LOC_REGISTER;
          end
        else
          result:=inherited first_trunc_real;
      end;


    procedure ts32inlinenode.second_trunc_real;
      var
        list: TAsmList;
        cgs: tcgs32;
      begin
        list:=current_asmdata.CurrAsmList;
        cgs:=tcgs32(cg);
        secondpass(left);

        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        if location.size in [OS_S64,OS_64] then
          begin
            location.register64.reglo:=cg.getintregister(list,OS_32);
            location.register64.reghi:=cg.getintregister(list,OS_32);
          end
        else
          location.register:=cg.getintregister(list,location.size);

        cgs.a_alloc_evenpair(list,NR_F64PAIR_A);
        if is_single(left.resultdef) or
           ((left.nodetype=callparan) and is_single(tcallparanode(left).left.resultdef)) then
          begin
            hlcg.location_force_fpureg(list,left.location,left.resultdef,true);
            list.concat(taicpu.op_reg_reg(A_FCVT_L_S,NR_F64PAIR_A,left.location.register));
          end
        else
          begin
            hlcg.location_force_reg(list,left.location,left.resultdef,left.resultdef,true);
            cgs.a_alloc_evenpair(list,NR_F64PAIR_B);
            cgs.a_load_reg64_evenpair(list,
              left.location.register64.reglo,left.location.register64.reghi,NR_F64PAIR_B);
            list.concat(taicpu.op_reg_reg(A_FCVT_L_D,NR_F64PAIR_A,NR_F64PAIR_B));
            cgs.a_dealloc_evenpair(list,NR_F64PAIR_B);
          end;
        if location.size in [OS_S64,OS_64] then
          cgs.a_load_evenpair_reg64(list,NR_F64PAIR_A,
            location.register64.reglo,location.register64.reghi)
        else
          cg.a_load_reg_reg(list,OS_32,OS_32,NR_F64PAIR_A,location.register);
        cgs.a_dealloc_evenpair(list,NR_F64PAIR_A);
      end;


begin
   cinlinenode:=ts32inlinenode;
end.
