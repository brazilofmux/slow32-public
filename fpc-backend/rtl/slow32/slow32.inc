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
    ldw r3, r3, -4
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;assembler;
  asm
    { previous frame pointer is at framebp - 8 }
    ldw r3, r3, -8
  end;


{****************************************************************************
                       atomic operations
****************************************************************************}

{ SLOW-32 has no hardware atomics -- simple non-atomic fallbacks
  (single-core embedded, so these are safe) }

{$define FPC_SYSTEM_HAS_ATOMIC_DEC_32}
function fpc_atomic_dec_32 (var Target: longint) : longint; assembler; nostackframe;
  asm
    ldw r4, r3, 0
    addi r4, r4, -1
    stw r3, r4, 0
    addi r3, r4, 0
  end;


{$define FPC_SYSTEM_HAS_ATOMIC_INC_32}
function fpc_atomic_inc_32 (var Target: longint) : longint; assembler; nostackframe;
  asm
    ldw r4, r3, 0
    addi r4, r4, 1
    stw r3, r4, 0
    addi r3, r4, 0
  end;


{$define FPC_SYSTEM_HAS_ATOMIC_XCHG_32}
function fpc_atomic_xchg_32 (var Target: longint;Source : longint) : longint; assembler; nostackframe;
  asm
    ldw r5, r3, 0
    stw r3, r4, 0
    addi r3, r5, 0
  end;


{$define FPC_SYSTEM_HAS_ATOMIC_CMP_XCHG_32}
function fpc_atomic_cmp_xchg_32 (var Target: longint; NewValue: longint; Comparand: longint) : longint; [public,alias:'FPC_ATOMIC_CMP_XCHG_32']; assembler; nostackframe;
  asm
    ldw r6, r3, 0
    bne r6, r5, .LFail
    stw r3, r4, 0
  .LFail:
    addi r3, r6, 0
  end;


{$define FPC_SYSTEM_HAS_ATOMIC_ADD_32}
function fpc_atomic_add_32 (var Target: longint;Value: longint) : longint; assembler; nostackframe;
  asm
    ldw r5, r3, 0
    add r5, r5, r4
    stw r3, r5, 0
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


{****************************************************************************
                       Heap manager (delegates to C malloc/free)
****************************************************************************}

{$ifdef FPC_HAS_FEATURE_HEAP}
{$define FPC_SYSTEM_HAS_SYSINITMEMORYMANAGER}

function c_malloc(size: ptruint): pointer; cdecl; external name 'malloc';
procedure c_free(p: pointer); cdecl; external name 'free';
function c_realloc(p: pointer; size: ptruint): pointer; cdecl; external name 'realloc';
function c_calloc(unitsize, unitcount: ptruint): pointer; cdecl; external name 'calloc';

const
  heap_headersize = sizeof(ptruint);

function S32GetMem(size: ptruint): pointer;
begin
  result := c_malloc(size + heap_headersize);
  if result <> nil then
  begin
    pptruint(result)^ := size;
    inc(result, heap_headersize);
  end;
end;

function S32FreeMem(p: pointer): ptruint;
begin
  if p <> nil then
    dec(p, heap_headersize);
  c_free(p);
  result := 0;
end;

function S32FreeMemSize(p: pointer; size: ptruint): ptruint;
begin
  if size <= 0 then
    exit(0);
  if p <> nil then
    if size <> pptruint(p - heap_headersize)^ then
      runerror(204);
  result := S32FreeMem(p);
end;

function S32AllocMem(size: ptruint): pointer;
begin
  result := c_calloc(size + heap_headersize, 1);
  if result <> nil then
  begin
    pptruint(result)^ := size;
    inc(result, heap_headersize);
  end;
end;

function S32ReAllocMem(var p: pointer; size: ptruint): pointer;
begin
  if size = 0 then
  begin
    if p <> nil then
    begin
      dec(p, heap_headersize);
      c_free(p);
      p := nil;
    end;
  end
  else
  begin
    inc(size, heap_headersize);
    if p = nil then
      p := c_malloc(size)
    else
    begin
      dec(p, heap_headersize);
      p := c_realloc(p, size);
    end;
    if p <> nil then
    begin
      pptruint(p)^ := size - heap_headersize;
      inc(p, heap_headersize);
    end;
  end;
  result := p;
end;

function S32MemSize(p: pointer): ptruint;
begin
  result := pptruint(p - heap_headersize)^;
end;

function S32GetHeapStatus: THeapStatus;
begin
  fillchar(result, sizeof(result), 0);
end;

function S32GetFPCHeapStatus: TFPCHeapStatus;
begin
  fillchar(result, sizeof(result), 0);
end;

const
  S32MemoryManager: TMemoryManager = (
    NeedLock: false;
    GetMem: @S32GetMem;
    FreeMem: @S32FreeMem;
    FreeMemSize: @S32FreeMemSize;
    AllocMem: @S32AllocMem;
    ReallocMem: @S32ReAllocMem;
    MemSize: @S32MemSize;
    InitThread: nil;
    DoneThread: nil;
    RelocateHeap: nil;
    GetHeapStatus: @S32GetHeapStatus;
    GetFPCHeapStatus: @S32GetFPCHeapStatus;
  );

procedure SysInitMemoryManager;
begin
  SetMemoryManager(S32MemoryManager);
end;
{$endif FPC_HAS_FEATURE_HEAP}


{****************************************************************************
                       Console I/O support
****************************************************************************}

{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
{$define FPC_SYSTEM_HAS_SYSINITSTDIO}

{ C library functions from libc_mmio.s32a }
function c_open(pathname: PAnsiChar; flags: longint): longint; cdecl; external name 'open';
function c_close(fd: longint): longint; cdecl; external name 'close';
function c_read(fd: longint; buf: pointer; count: longint): longint; cdecl; external name 'read';
function c_write(fd: longint; buf: pointer; count: longint): longint; cdecl; external name 'write';
function c_lseek(fd: longint; offset: longint; whence: longint): longint; cdecl; external name 'lseek';
function c_unlink(pathname: PAnsiChar): longint; cdecl; external name 'unlink';
function c_rename(oldpath: PAnsiChar; newpath: PAnsiChar): longint; cdecl; external name 'rename';
function c_ftruncate(fd: longint; length: longint): longint; cdecl; external name 'ftruncate';

const
  { SLOW-32 open flags (from fcntl.h) }
  S32_O_RDONLY = $01;
  S32_O_WRONLY = $02;
  S32_O_RDWR   = $03;
  S32_O_APPEND = $04;
  S32_O_CREAT  = $08;
  S32_O_TRUNC  = $10;

  { lseek whence }
  S32_SEEK_SET = 0;
  S32_SEEK_CUR = 1;
  S32_SEEK_END = 2;

{ Console TextRec handlers — direct path to libc_mmio }

procedure Slow32_Write(var t: TextRec);
var
  i: longint;
begin
  if t.BufPos = 0 then exit;
  i := c_write(t.Handle, t.BufPtr, t.BufPos);
  if i <> longint(t.BufPos) then
    InOutRes := 101;
  t.BufPos := 0;
end;

procedure Slow32_Read(var t: TextRec);
begin
  t.BufEnd := c_read(t.Handle, t.BufPtr, t.BufSize);
  t.BufPos := 0;
end;

procedure Slow32_Close(var t: TextRec);
begin
  { Don't actually close stdin/stdout/stderr }
  if (t.Handle > 2) then
    c_close(t.Handle);
  t.Handle := UnusedHandle;
end;

{ rtl_do_* wrappers — file I/O through sysfile.inc delegation }

function slow32_do_write(h: longint; addr: pointer; len: longint): longint;
begin
  result := c_write(h, addr, len);
  if result < 0 then
    InOutRes := 101
  else
    InOutRes := 0;
end;

function slow32_do_read(h: longint; addr: pointer; len: longint): longint;
begin
  result := c_read(h, addr, len);
  if result < 0 then
    InOutRes := 101
  else
    InOutRes := 0;
end;

procedure slow32_do_close(handle: longint);
begin
  if c_close(handle) < 0 then
    InOutRes := 6
  else
    InOutRes := 0;
end;

procedure slow32_do_erase(p: PAnsiChar);
begin
  if c_unlink(p) < 0 then
    InOutRes := 2
  else
    InOutRes := 0;
end;

procedure slow32_do_rename(p1, p2: PAnsiChar);
begin
  if c_rename(p1, p2) < 0 then
    InOutRes := 2
  else
    InOutRes := 0;
end;

procedure slow32_do_open(var f; p: PAnsiChar; flags: longint);
var
  oflags: longint;
  fd: longint;
begin
  { Close existing handle if needed (bit 16 = skip close check) }
  if (flags and $10000) = 0 then
    case FileRec(f).mode of
      fminput, fmoutput, fminout: slow32_do_close(FileRec(f).Handle);
      fmclosed: ;
    else
      begin
        InOutRes := 102;
        exit;
      end;
    end;

  FileRec(f).Handle := UnusedHandle;

  { Map FPC access mode (bits 0-1) to SLOW-32 flags }
  case (flags and 3) of
    0: begin oflags := S32_O_RDONLY; FileRec(f).mode := fminput; end;
    1: begin oflags := S32_O_WRONLY; FileRec(f).mode := fmoutput; end;
    2: begin oflags := S32_O_RDWR;   FileRec(f).mode := fminout; end;
  end;

  if (flags and $1000) = $1000 then
    oflags := oflags or S32_O_CREAT or S32_O_TRUNC
  else if (flags and $100) = $100 then
    oflags := oflags or S32_O_APPEND;

  { Empty name -> map to stdio handles }
  if p[0] = #0 then
  begin
    case FileRec(f).mode of
      fminput:           FileRec(f).Handle := StdInputHandle;
      fminout, fmoutput: FileRec(f).Handle := StdOutputHandle;
      fmappend: begin
        FileRec(f).Handle := StdOutputHandle;
        FileRec(f).mode := fmoutput;
      end;
    end;
    exit;
  end;

  fd := c_open(p, oflags);
  if fd < 0 then
  begin
    InOutRes := 2;
    FileRec(f).mode := fmclosed;
  end
  else
  begin
    FileRec(f).Handle := fd;
    InOutRes := 0;
  end;
end;

procedure slow32_do_seek(handle, pos: longint);
begin
  if c_lseek(handle, pos, S32_SEEK_SET) < 0 then
    InOutRes := 101
  else
    InOutRes := 0;
end;

function slow32_do_seekend(handle: longint): longint;
begin
  result := c_lseek(handle, 0, S32_SEEK_END);
  if result < 0 then InOutRes := 101
  else InOutRes := 0;
end;

function slow32_do_filepos(handle: longint): longint;
begin
  result := c_lseek(handle, 0, S32_SEEK_CUR);
  if result < 0 then InOutRes := 101
  else InOutRes := 0;
end;

function slow32_do_filesize(handle: longint): longint;
var
  curpos: longint;
begin
  curpos := c_lseek(handle, 0, S32_SEEK_CUR);
  result := c_lseek(handle, 0, S32_SEEK_END);
  if result >= 0 then
    c_lseek(handle, curpos, S32_SEEK_SET);
  if result < 0 then InOutRes := 101
  else InOutRes := 0;
end;

procedure slow32_do_truncate(handle, pos: longint);
begin
  if c_ftruncate(handle, pos) < 0 then
    InOutRes := 101
  else
    InOutRes := 0;
end;

function slow32_do_isdevice(handle: longint): boolean;
begin
  { Handles 0-2 are stdin/stdout/stderr }
  result := (handle >= 0) and (handle <= 2);
end;

{ SysInitStdIO — wire up all I/O and initialize console TextRecs }

procedure SysInitStdIO;
begin
  { Wire up file I/O through rtl_do_* function pointers }
  rtl_do_write := @slow32_do_write;
  rtl_do_read := @slow32_do_read;
  rtl_do_close := @slow32_do_close;
  rtl_do_open := @slow32_do_open;
  rtl_do_seek := @slow32_do_seek;
  rtl_do_seekend := @slow32_do_seekend;
  rtl_do_filepos := @slow32_do_filepos;
  rtl_do_filesize := @slow32_do_filesize;
  rtl_do_truncate := @slow32_do_truncate;
  rtl_do_erase := @slow32_do_erase;
  rtl_do_rename := @slow32_do_rename;
  rtl_do_isdevice := @slow32_do_isdevice;

  { Initialize Output }
  FillChar(Output, SizeOf(TextRec), 0);
  TextRec(Output).Handle := StdOutputHandle;
  TextRec(Output).Mode := fmOutput;
  TextRec(Output).BufSize := TextRecBufSize;
  TextRec(Output).BufPtr := @TextRec(Output).Buffer;
  TextRec(Output).InOutFunc := @Slow32_Write;
  TextRec(Output).FlushFunc := @Slow32_Write;
  TextRec(Output).CloseFunc := @Slow32_Close;
  TextRec(Output).LineEnd := #10;

  { Initialize ErrOutput }
  FillChar(ErrOutput, SizeOf(TextRec), 0);
  TextRec(ErrOutput).Handle := StdErrorHandle;
  TextRec(ErrOutput).Mode := fmOutput;
  TextRec(ErrOutput).BufSize := TextRecBufSize;
  TextRec(ErrOutput).BufPtr := @TextRec(ErrOutput).Buffer;
  TextRec(ErrOutput).InOutFunc := @Slow32_Write;
  TextRec(ErrOutput).FlushFunc := @Slow32_Write;
  TextRec(ErrOutput).CloseFunc := @Slow32_Close;
  TextRec(ErrOutput).LineEnd := #10;

  { Initialize Input }
  FillChar(Input, SizeOf(TextRec), 0);
  TextRec(Input).Handle := StdInputHandle;
  TextRec(Input).Mode := fmInput;
  TextRec(Input).BufSize := TextRecBufSize;
  TextRec(Input).BufPtr := @TextRec(Input).Buffer;
  TextRec(Input).InOutFunc := @Slow32_Read;
  TextRec(Input).CloseFunc := @Slow32_Close;
  TextRec(Input).LineEnd := #10;

  InOutRes := 0;
end;
{$endif FPC_HAS_FEATURE_CONSOLEIO}
