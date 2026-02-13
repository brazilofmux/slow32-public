{
    Basic Processor information for SLOW-32

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

Unit CPUInfo;

{$i fpcdefs.inc}

Interface

  uses
    globtype;

{$I cpuinfo.inc}

Type
   { possible supported processors for this target }
   tcputype =
      (cpu_none,
       cpu_slow32
      );

   { No microcontroller support for now }
   tcontrollertype =
     (ct_none
     );

   tcontrollerdatatype = record
      controllertypestr, controllerunitstr: string[20];
      cputype: tcputype; fputype: tfputype;
      flashbase, flashsize, srambase, sramsize, eeprombase, eepromsize, bootbase, bootsize: dword;
   end;


Const
   ControllerSupport = false;

   {$PUSH}
    {$WARN 3177 OFF}
   embedded_controllers : array [tcontrollertype] of tcontrollerdatatype =
   (
      (controllertypestr:''; controllerunitstr:''; cputype:cpu_none; fputype:fpu_none; flashbase:0; flashsize:0; srambase:0; sramsize:0)
   );
   {$POP}

   { Size of native extended floating point type }
   extended_size = 12;
   { target cpu string (used by compiler options) }
   target_cpu_string = 'slow32';

   { calling conventions supported by the code generator }
   supported_calling_conventions : tproccalloptions = [
     pocall_internproc,
     pocall_safecall,
     pocall_stdcall,
     pocall_cdecl,
     pocall_cppdecl,
     pocall_mwpascal
   ];

   cputypestr : array[tcputype] of string[24] = ('',
     'SLOW32'
   );

   { Supported optimizations }
   supported_optimizerswitches = genericlevel1optimizerswitches+
                                 genericlevel2optimizerswitches+
                                 genericlevel3optimizerswitches-
                                 [cs_opt_level1,cs_opt_level2,cs_opt_level3]+
                                 [{$ifndef llvm}cs_opt_regvar,{$endif}cs_opt_loopunroll,cs_opt_nodecse,
                                  cs_opt_tailrecursion,cs_opt_reorder_fields,cs_opt_fastmath,
                                  cs_opt_stackframe];

   level1optimizerswitches = genericlevel1optimizerswitches;
   level2optimizerswitches = genericlevel2optimizerswitches + level1optimizerswitches + [{$ifndef llvm}cs_opt_regvar,{$endif}cs_opt_nodecse,cs_opt_tailrecursion,cs_opt_consts];
   level3optimizerswitches = genericlevel3optimizerswitches + level2optimizerswitches;
   level4optimizerswitches = genericlevel4optimizerswitches + level3optimizerswitches + [cs_opt_stackframe];

 const
   cpu_capabilities : array[tcputype] of set of tcpuflags =
     ( { cpu_none      } [],
       { cpu_slow32    } [CPUS32_HAS_MUL]
     );

Implementation

end.
