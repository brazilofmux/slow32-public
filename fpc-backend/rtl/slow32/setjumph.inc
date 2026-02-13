{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2025 by the Free Pascal development team.

    SetJmp/Longjmp declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
   { SLOW-32 register ABI:
     r31=lr (link register / return address)
     r30=fp (frame pointer)
     r11-r28=s0-s17 (callee-saved)
     r29=sp (stack pointer)
     No FPU registers to save. }
   jmp_buf = record
    lr,fp,
    s0,s1,s2,s3,s4,s5,s6,s7,
    s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,
    sp: dword;
   end;
   pjmp_buf = ^jmp_buf;

function setjmp(var S : jmp_buf) : longint;[external name 'FPC_SETJMP'];
procedure longjmp(var S : jmp_buf;value : longint);[external name 'FPC_LONGJMP'];
