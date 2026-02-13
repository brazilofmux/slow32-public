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
        { SLOW-32 has no hardware FPU, so all float intrinsics use helpers.
          Override first_* to return nil only if we had HW support;
          since we don't, just inherit everything. }
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


begin
   cinlinenode:=ts32inlinenode;
end.
