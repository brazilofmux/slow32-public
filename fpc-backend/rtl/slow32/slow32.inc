{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2025 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    SLOW-32

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************
                       stack frame related stuff
****************************************************************************}

{$IFNDEF INTERNAL_BACKTRACE}
{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;nostackframe;
  asm
    { r30 = fp (frame pointer) }
    addi r3, r30, 0
  end;
{$ENDIF not INTERNAL_BACKTRACE}


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : pointer;assembler;nostackframe;
  asm
    { r29 = sp (stack pointer) }
    addi r3, r29, 0
  end;


{****************************************************************************
                       fpu exception related stuff
****************************************************************************}

{ SLOW-32 uses soft-float, no hardware FPU CSRs }

{$define FPC_SYSTEM_HAS_SYSINITFPU}
procedure SysInitFPU;
begin
  softfloat_exception_flags:=[];
  softfloat_exception_mask:=[exPrecision,exUnderflow];
end;


{$define FPC_SYSTEM_HAS_SYSRESETFPU}
Procedure SysResetFPU;
begin
  softfloat_exception_flags:=[];
end;


{****************************************************************************
                       memory barrier operations
****************************************************************************}

{$define FPC_SYSTEM_HAS_MEM_BARRIER}

procedure ReadBarrier;{$if defined(SYSTEMINLINE)}inline;{$endif}
  begin
    { SLOW-32 is single-core embedded, barriers are no-ops }
  end;


procedure ReadDependencyBarrier;{$if defined(SYSTEMINLINE)}inline;{$endif}
  begin
  end;


procedure ReadWriteBarrier;{$if defined(SYSTEMINLINE)}inline;{$endif}
  begin
  end;


procedure WriteBarrier;{$if defined(SYSTEMINLINE)}inline;{$endif}
  begin
  end;


{****************************************************************************
                       stack frame related stuff
****************************************************************************}

{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer;addr:pointer=nil):pointer;assembler;
  asm
    { return address is at framebp - 4 }
    ldw r3, -4(r3)
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;assembler;
  asm
    { previous frame pointer is at framebp - 8 }
    ldw r3, -8(r3)
  end;


{****************************************************************************
                       atomic operations
****************************************************************************}

{ SLOW-32 has no hardware atomics -- simple non-atomic fallbacks
  (single-core embedded, so these are safe) }

{$define FPC_SYSTEM_HAS_ATOMIC_DEC_32}
function fpc_atomic_dec_32 (var Target: longint) : longint; assembler; nostackframe;
  asm
    ldw r4, 0(r3)
    addi r4, r4, -1
    stw r4, 0(r3)
    addi r3, r4, 0
  end;


{$define FPC_SYSTEM_HAS_ATOMIC_INC_32}
function fpc_atomic_inc_32 (var Target: longint) : longint; assembler; nostackframe;
  asm
    ldw r4, 0(r3)
    addi r4, r4, 1
    stw r4, 0(r3)
    addi r3, r4, 0
  end;


{$define FPC_SYSTEM_HAS_ATOMIC_XCHG_32}
function fpc_atomic_xchg_32 (var Target: longint;Source : longint) : longint; assembler; nostackframe;
  asm
    ldw r5, 0(r3)
    stw r4, 0(r3)
    addi r3, r5, 0
  end;


{$define FPC_SYSTEM_HAS_ATOMIC_CMP_XCHG_32}
function fpc_atomic_cmp_xchg_32 (var Target: longint; NewValue: longint; Comparand: longint) : longint; [public,alias:'FPC_ATOMIC_CMP_XCHG_32']; assembler; nostackframe;
  asm
    ldw r6, 0(r3)
    bne r6, r5, .LFail
    stw r4, 0(r3)
  .LFail:
    addi r3, r6, 0
  end;


{$define FPC_SYSTEM_HAS_ATOMIC_ADD_32}
function fpc_atomic_add_32 (var Target: longint;Value: longint) : longint; assembler; nostackframe;
  asm
    ldw r5, 0(r3)
    add r5, r5, r4
    stw r5, 0(r3)
    addi r3, r5, 0
  end;


{$define FPC_SYSTEM_HAS_DECLOCKED_LONGINT}
  function declocked(var l: longint) : boolean; inline;
  begin
    Result:=InterLockedDecrement(l) = 0;
  end;


{$define FPC_SYSTEM_HAS_INCLOCKED_LONGINT}
  procedure inclocked(var l: longint); inline;
  begin
    InterLockedIncrement(l);
  end;
