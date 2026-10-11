{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by the Free Pascal development team

    FPU control for SLOW-32.  The CPU has no control register: f32
    arithmetic rounds to nearest, f64 and everything else is soft.  The
    modes and the exception mask are kept as plain state so Get/Set pairs
    round-trip (the test suite checks that); they do not change results.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

var
  s32_roundmode: TFPURoundingMode = rmNearest;
  s32_precisionmode: TFPUPrecisionMode = pmDouble;
  s32_exceptionmask: TFPUExceptionMask = [exDenormalized,exOverflow,exPrecision];

function GetRoundMode: TFPURoundingMode;
begin
  GetRoundMode:=s32_roundmode;
end;

function SetRoundMode(const RoundMode: TFPURoundingMode): TFPURoundingMode;
begin
  SetRoundMode:=s32_roundmode;
  s32_roundmode:=RoundMode;
end;

function GetPrecisionMode: TFPUPrecisionMode;
begin
  GetPrecisionMode:=s32_precisionmode;
end;

function SetPrecisionMode(const Precision: TFPUPrecisionMode): TFPUPrecisionMode;
begin
  SetPrecisionMode:=s32_precisionmode;
  s32_precisionmode:=Precision;
end;

function GetExceptionMask: TFPUExceptionMask;
begin
  GetExceptionMask:=s32_exceptionmask;
end;

function SetExceptionMask(const Mask: TFPUExceptionMask): TFPUExceptionMask;
begin
  SetExceptionMask:=s32_exceptionmask;
  s32_exceptionmask:=Mask;
end;

procedure ClearExceptions(RaisePending: Boolean);
begin
end;
