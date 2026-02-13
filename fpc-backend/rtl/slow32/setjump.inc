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
    stw r31, 0(r3)     { lr }
    stw r30, 4(r3)     { fp }
    stw r11, 8(r3)     { s0 }
    stw r12, 12(r3)    { s1 }
    stw r13, 16(r3)    { s2 }
    stw r14, 20(r3)    { s3 }
    stw r15, 24(r3)    { s4 }
    stw r16, 28(r3)    { s5 }
    stw r17, 32(r3)    { s6 }
    stw r18, 36(r3)    { s7 }
    stw r19, 40(r3)    { s8 }
    stw r20, 44(r3)    { s9 }
    stw r21, 48(r3)    { s10 }
    stw r22, 52(r3)    { s11 }
    stw r23, 56(r3)    { s12 }
    stw r24, 60(r3)    { s13 }
    stw r25, 64(r3)    { s14 }
    stw r26, 68(r3)    { s15 }
    stw r27, 72(r3)    { s16 }
    stw r28, 76(r3)    { s17 }
    stw r29, 80(r3)    { sp }
    addi r3, r0, 0     { return 0 }
end;


procedure fpc_longjmp(var S : jmp_buf;value : longint);[Public, alias : 'FPC_LONGJMP'];compilerproc;assembler;nostackframe;
  asm
    ldw r31, 0(r3)     { lr }
    ldw r30, 4(r3)     { fp }
    ldw r11, 8(r3)     { s0 }
    ldw r12, 12(r3)    { s1 }
    ldw r13, 16(r3)    { s2 }
    ldw r14, 20(r3)    { s3 }
    ldw r15, 24(r3)    { s4 }
    ldw r16, 28(r3)    { s5 }
    ldw r17, 32(r3)    { s6 }
    ldw r18, 36(r3)    { s7 }
    ldw r19, 40(r3)    { s8 }
    ldw r20, 44(r3)    { s9 }
    ldw r21, 48(r3)    { s10 }
    ldw r22, 52(r3)    { s11 }
    ldw r23, 56(r3)    { s12 }
    ldw r24, 60(r3)    { s13 }
    ldw r25, 64(r3)    { s14 }
    ldw r26, 68(r3)    { s15 }
    ldw r27, 72(r3)    { s16 }
    ldw r28, 76(r3)    { s17 }
    ldw r29, 80(r3)    { sp }

    { if value == 0, return 1; else return value }
    bne r4, r0, .Lret
    addi r4, r0, 1
  .Lret:
    addi r3, r4, 0
    jalr r0, r31, 0
  end;
