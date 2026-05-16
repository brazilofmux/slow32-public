{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by the Free Pascal development team.

    System unit for SLOW-32 (embedded target, MMIO-hosted)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit System;

{*****************************************************************************}
                                    interface
{*****************************************************************************}

{$define FPC_SYSTEM_HAS_STACKTOP}

{$define FPC_IS_SYSTEM}
{$define HAS_CMDLINE}

{$define DISABLE_NO_THREAD_MANAGER}
{ Do not use the generic memory manager; slow32.inc provides one that
  delegates to libc_mmio's malloc/free. }
{$define HAS_MEMORYMANAGER}
{$define FPC_NO_DEFAULT_HEAP}

{$define FPC_ANSI_TEXTFILEREC}

{ make output and stdout as well as input and stdin equal to save memory }
{$define FPC_STDOUT_TRUE_ALIAS}

{$I check.inc}

{$I systemh.inc}

const
{$ifdef FPC_HAS_FEATURE_TEXTIO}
  LineEnding = #10;
{$endif FPC_HAS_FEATURE_TEXTIO}
{$ifdef FPC_HAS_FEATURE_FILEIO}
  LFNSupport = true;
  DirectorySeparator = '/';
  DriveSeparator = ':';
  ExtensionSeparator = '.';
  PathSeparator = ':';
  AllowDirectorySeparators : set of AnsiChar = ['\','/'];
  AllowDriveSeparators : set of AnsiChar = [':'];
{$endif FPC_HAS_FEATURE_FILEIO}

{$ifdef FPC_HAS_FEATURE_EXITCODE}
  maxExitCode = 255;
{$endif FPC_HAS_FEATURE_EXITCODE}

{$ifdef FPC_HAS_FEATURE_FILEIO}
  MaxPathLen = 1024;
  AllFilesMask = '*';

  UnusedHandle    = -1;
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = 2;

  FileNameCaseSensitive : boolean = true;
  FileNameCasePreserving: boolean = true;
{$endif FPC_HAS_FEATURE_FILEIO}

{$ifdef FPC_HAS_FEATURE_TEXTIO}
  CtrlZMarksEOF: boolean = false;

  sLineBreak = LineEnding;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsLF;
{$endif FPC_HAS_FEATURE_TEXTIO}

type
  trtl_do_close = procedure (handle : longint);
  trtl_do_erase = procedure (p : PAnsiChar);
  trtl_do_rename = procedure (p1,p2 : PAnsiChar);
  trtl_do_write = function (h: longint; addr: pointer; len: longint) : longint;
  trtl_do_read = function (h: longint; addr: pointer; len: longint) : longint;
  trtl_do_filepos = function (handle: longint) : longint;
  trtl_do_seek = procedure (handle, pos: longint);
  trtl_do_seekend = function (handle: longint):longint;
  trtl_do_filesize = function (handle : longint) : longint;
  trtl_do_truncate = procedure (handle, pos: longint);
  trtl_do_open = procedure (var f;p:PAnsiChar;flags:longint);
  trtl_do_isdevice = function (handle: longint): boolean;

var
  rtl_do_close : trtl_do_close = nil;
  rtl_do_erase : trtl_do_erase = nil;
  rtl_do_rename : trtl_do_rename  = nil;
  rtl_do_write : trtl_do_write = nil;
  rtl_do_read : trtl_do_read = nil;
  rtl_do_filepos : trtl_do_filepos = nil;
  rtl_do_seek : trtl_do_seek = nil;
  rtl_do_seekend : trtl_do_seekend = nil;
  rtl_do_filesize : trtl_do_filesize = nil;
  rtl_do_truncate : trtl_do_truncate = nil;
  rtl_do_open : trtl_do_open = nil;
  rtl_do_isdevice : trtl_do_isdevice = nil;

{$ifdef FPC_HAS_FEATURE_COMMANDARGS}
var
  argc: LongInt = 0;
  argv: PPAnsiChar = nil;
  envp: PPAnsiChar = nil;
  cmdline: PAnsiChar = nil;
{$endif FPC_HAS_FEATURE_COMMANDARGS}

{$ifndef FPUNONE}
{$ifdef FPC_HAS_FEATURE_SOFTFPU}

{$define fpc_softfpu_interface}
{$i softfpu.pp}
{$undef fpc_softfpu_interface}

{$endif FPC_HAS_FEATURE_SOFTFPU}
{$endif FPUNONE}

{*****************************************************************************}
                                 implementation
{*****************************************************************************}

const calculated_cmdline:PAnsiChar=nil;

{$ifndef FPUNONE}
{$ifdef FPC_HAS_FEATURE_SOFTFPU}

{$define fpc_softfpu_implementation}
{$i softfpu.pp}
{$undef fpc_softfpu_implementation}

{$define FPC_SYSTEM_HAS_float64}
{$define FPC_SYSTEM_HAS_float32}
{$define FPC_SYSTEM_HAS_flag}
{$define FPC_SYSTEM_HAS_extractFloat64Frac0}
{$define FPC_SYSTEM_HAS_extractFloat64Frac1}
{$define FPC_SYSTEM_HAS_extractFloat64Exp}
{$define FPC_SYSTEM_HAS_extractFloat64Frac}
{$define FPC_SYSTEM_HAS_extractFloat64Sign}
{$define FPC_SYSTEM_HAS_ExtractFloat32Frac}
{$define FPC_SYSTEM_HAS_extractFloat32Exp}
{$define FPC_SYSTEM_HAS_extractFloat32Sign}

{$endif FPC_HAS_FEATURE_SOFTFPU}
{$endif FPUNONE}

{$define FPC_SYSTEM_EXIT_NO_RETURN}
{$I system.inc}

{*****************************************************************************
                       Misc. System Dependent Functions
*****************************************************************************}

var
 _stack_top: record end; external name '_stack_top';

function StackTop: pointer;
begin
  StackTop:=@_stack_top;
end;


procedure haltproc;cdecl;external name '_haltproc';

procedure System_exit;noreturn;external name '_haltproc';


{$ifdef FPC_HAS_FEATURE_PROCESSES}
function GetProcessID: SizeUInt;
begin
  GetProcessID := 0;
end;
{$endif}


{$ifdef FPC_HAS_FEATURE_COMMANDARGS}
{ argc/argv/envp are populated in the initialisation block below via
  __slow32_fetch_args / __slow32_fetch_envp (declared in slow32.inc). }

Function ParamCount: Longint;
Begin
  if argc<=0 then
    ParamCount:=0
  else
    ParamCount:=argc-1;
End;


function paramstr(l: longint) : shortstring;
var
  p: PAnsiChar;
  i: longint;
begin
  paramstr := '';
  if (argv=nil) or (l<0) or (l>=argc) then
    exit;
  p := PPAnsiChar(argv)[l];
  if p=nil then
    exit;
  i := 0;
  while (i<high(shortstring)) and (p[i]<>#0) do
    inc(i);
  SetLength(paramstr, i);
  if i>0 then
    Move(p^, paramstr[1], i);
end;
{$endif FPC_HAS_FEATURE_COMMANDARGS}

{$ifdef FPC_HAS_FEATURE_RANDOM}
procedure randomize();
begin
  RandSeed := 63458;
end;
{$endif FPC_HAS_FEATURE_RANDOM}

{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

{$ifdef FPC_HAS_FEATURE_STACKCHECK}

function CheckInitialStkLen(stklen : SizeUInt) : SizeUInt;inline;
begin
  result := stklen;
end;

var
  initialstkptr : record end; external name '_stack_top';
{$endif FPC_HAS_FEATURE_STACKCHECK}

begin
  { FPU (hard or soft) is initialized from fpc_cpuinit, which is included
    per-cpu unconditionally. }

{$ifdef FPC_HAS_FEATURE_HEAP}
  SysInitMemoryManager;
{$endif FPC_HAS_FEATURE_HEAP}

{$ifdef FPC_HAS_FEATURE_COMMANDARGS}
  Slow32InitCmdLine;
{$endif FPC_HAS_FEATURE_COMMANDARGS}

{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
  IsConsole := TRUE;
  SysInitStdIO;
{$endif FPC_HAS_FEATURE_CONSOLEIO}

{$ifdef FPC_HAS_FEATURE_STACKCHECK}
  StackLength := CheckInitialStkLen(initialStkLen);
  StackBottom := @initialstkptr - StackLength;
{$endif FPC_HAS_FEATURE_STACKCHECK}

{$ifdef FPC_HAS_FEATURE_EXCEPTIONS}
  { SysInitExceptions only writes nil to bss-resident pointers; rely on
    BSS zero-init and skip the call. }
{$endif FPC_HAS_FEATURE_EXCEPTIONS}

{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
  InOutRes:=0;
{$endif FPC_HAS_FEATURE_CONSOLEIO}

{$ifdef FPC_HAS_FEATURE_THREADING}
  InitSystemThreads;
{$endif FPC_HAS_FEATURE_THREADING}
end.
