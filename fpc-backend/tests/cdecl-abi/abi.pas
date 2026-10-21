{ The Pascal half of the cdecl ABI test (GitHub issue 98): call the C
  functions in cside.c with known values, and let C call back into
  cdecl functions here.  Doubles align to a register pair and leave a
  hole the next word fills; int64 never aligns; varargs pack. }
program abi;

procedure c_int64(a: longint; b: int64); cdecl; external name 'c_int64';
procedure c_double(a: longint; b: double); cdecl; external name 'c_double';
procedure c_hole(a: longint; b: double; c, d: longint); cdecl; external name 'c_hole';
procedure c_mix(a: longint; b: double; c: int64; d: single; e: double); cdecl; external name 'c_mix';
procedure c_spill(a, b, c, d, e, f, g: longint; h: double; i: longint); cdecl; external name 'c_spill';
procedure c_straddle(a, b, c, d, e, f, g: longint; h: int64); cdecl; external name 'c_straddle';
procedure c_holestraddle(a, b, c, d, e, f, g: longint; h: double; i: int64); cdecl; external name 'c_holestraddle';
procedure c_fixedvar(a: longint; b: double); cdecl; varargs; external name 'c_fixedvar';
procedure c_printf(fmt: pchar); cdecl; varargs; external name 'c_printf';
procedure c_callbacks; cdecl; external name 'c_callbacks';

function p_mix(a: longint; b: double; c: int64; d: single; e: double): double; cdecl; public name 'p_mix';
begin
  p_mix := a + b * 10 + c / 1000000000 + d * 100 + e * 1000;
end;

function p_int64(a: longint; b: int64; c: longint): int64; cdecl; public name 'p_int64';
begin
  p_int64 := b + a + c * 1000;
end;

function p_spill(a, b, c, d, e, f, g: longint; h: double; i: longint): double; cdecl; public name 'p_spill';
begin
  p_spill := a + b + c + d + e + f + g + h + i * 100;
end;

var
  bigint: int64;
  dvar: double;
begin
  bigint := 8589934595;      { $00000002_00000003 }
  dvar := 2.75;
  c_int64(1, bigint);
  c_double(7, dvar);
  c_hole(1, 2.5, 3, 4);
  c_mix(1, 2.5, bigint, 4.25, 5.125);
  c_spill(1, 2, 3, 4, 5, 6, 7, 8.5, 9);
  c_straddle(1, 2, 3, 4, 5, 6, 7, bigint);
  c_holestraddle(1, 2, 3, 4, 5, 6, 7, 8.5, bigint);
  c_fixedvar(1, 2.5, 3, 4.5, bigint);
  c_printf('c_printf %d %.4f %lld %d'#10, 1, 1.5, bigint, 4);
  c_callbacks;
end.
