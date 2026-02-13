{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2025 by the Free Pascal development team.

    SetJmp and LongJmp implementation for exception handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ SLOW-32 register mapping:
  r31=lr, r30=fp, r11-r28=s0-s17, r29=sp
  a0=r3 (first argument / return value)
  No FPU registers to save. }

function fpc_setjmp(var S : jmp_buf) : longint;[Public, alias : 'FPC_SETJMP'];compilerproc;assembler;nostackframe;
  asm
    stw r3, r31, 0     { lr }
    stw r3, r30, 4     { fp }
    stw r3, r11, 8     { s0 }
    stw r3, r12, 12    { s1 }
    stw r3, r13, 16    { s2 }
    stw r3, r14, 20    { s3 }
    stw r3, r15, 24    { s4 }
    stw r3, r16, 28    { s5 }
    stw r3, r17, 32    { s6 }
    stw r3, r18, 36    { s7 }
    stw r3, r19, 40    { s8 }
    stw r3, r20, 44    { s9 }
    stw r3, r21, 48    { s10 }
    stw r3, r22, 52    { s11 }
    stw r3, r23, 56    { s12 }
    stw r3, r24, 60    { s13 }
    stw r3, r25, 64    { s14 }
    stw r3, r26, 68    { s15 }
    stw r3, r27, 72    { s16 }
    stw r3, r28, 76    { s17 }
    stw r3, r29, 80    { sp }
    addi r3, r0, 0     { return 0 }
end;


procedure fpc_longjmp(var S : jmp_buf;value : longint);[Public, alias : 'FPC_LONGJMP'];compilerproc;assembler;nostackframe;
  asm
    ldw r31, r3, 0     { lr }
    ldw r30, r3, 4     { fp }
    ldw r11, r3, 8     { s0 }
    ldw r12, r3, 12    { s1 }
    ldw r13, r3, 16    { s2 }
    ldw r14, r3, 20    { s3 }
    ldw r15, r3, 24    { s4 }
    ldw r16, r3, 28    { s5 }
    ldw r17, r3, 32    { s6 }
    ldw r18, r3, 36    { s7 }
    ldw r19, r3, 40    { s8 }
    ldw r20, r3, 44    { s9 }
    ldw r21, r3, 48    { s10 }
    ldw r22, r3, 52    { s11 }
    ldw r23, r3, 56    { s12 }
    ldw r24, r3, 60    { s13 }
    ldw r25, r3, 64    { s14 }
    ldw r26, r3, 68    { s15 }
    ldw r27, r3, 72    { s16 }
    ldw r28, r3, 76    { s17 }
    ldw r29, r3, 80    { sp }

    { if value == 0, return 1; else return value }
    bne r4, r0, .Lret
    addi r4, r0, 1
  .Lret:
    addi r3, r4, 0
    jalr r0, r31, 0
  end;
