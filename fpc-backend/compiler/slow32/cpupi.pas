{
    Copyright (c) 2002 by Florian Klaempfl

    This unit contains the CPU specific part of tprocinfo for SLOW-32

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

{ This unit contains the CPU specific part of tprocinfo. }
unit cpupi;

{$i fpcdefs.inc}

  interface

    uses
       cutils,globtype,
       cgbase,aasmdata,
       procinfo,cpuinfo,psub;

    type
      ts32procinfo = class(tcgprocinfo)
          stackframesize : aint;
          stackpaddingreg: TSuperRegister;

          needs_frame_pointer: boolean;
          constructor create(aparent: tprocinfo); override;
          procedure set_first_temp_offset;override;
          function calc_stackframe_size:longint;override;
      end;


  implementation

    uses
       globals,systems,
       cpubase,
       tgobj,
       symconst,symtype,symsym,symcpu,paramgr,
       cgutils,
       cgobj,
       defutil,
       aasmcpu;


    constructor ts32procinfo.create(aparent: tprocinfo);
      begin
        inherited create(aparent);
        maxpushedparasize := 0;
      end;


    procedure ts32procinfo.set_first_temp_offset;
      begin
        if (po_nostackframe in procdef.procoptions) then
          begin
             tg.setfirsttemp(maxpushedparasize);
             exit;
          end;

        if tg.direction = -1 then
          tg.setfirsttemp(-(1+12)*4)
        else
          tg.setfirsttemp(maxpushedparasize);
      end;


    function ts32procinfo.calc_stackframe_size:longint;
      begin
        maxpushedparasize:=align(maxpushedparasize,max(current_settings.alignment.localalignmin,4));
        { No FPU register saving for SLOW-32 (floats in GPRs) }
        result:=Align(tg.direction*tg.lasttemp,max(current_settings.alignment.localalignmin,4))+maxpushedparasize;
      end;


begin
   cprocinfo:=ts32procinfo;
end.
