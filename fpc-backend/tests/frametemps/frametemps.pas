{ Frame temporaries must lie below the register save area (GitHub issue
  114).  Temps started at fp-52, room for thirteen saved registers; a
  function that keeps more than thirteen callee-saved registers live
  across a call -- integers in r11-r19 and singles in F20-F28 -- saved
  them where its own locals then lived, so the caller got back clobbered
  registers.  Every value here is checked after the call. }
program frametemps;

var
  sink: longint;

procedure quiet(n: longint);
begin
  sink := sink + n;
end;

function inner(x: longint): longint;
var
  a, b, c, d, e, f, g, h, i: longint;
  p, q, r, s, t, u, v, w: single;
  buf: array[0..63] of longint;
  k: longint;
begin
  a := x + 1; b := x + 2; c := x + 3; d := x + 4; e := x + 5;
  f := x + 6; g := x + 7; h := x + 8; i := x + 9;
  p := x * 0.5; q := x * 0.25; r := x * 0.125; s := x * 2.0;
  t := x * 4.0; u := x * 8.0; v := x * 0.75; w := x * 1.5;
  for k := 0 to 63 do
    buf[k] := k * 1000003;
  quiet(a);
  quiet(round(p));
  for k := 0 to 63 do
    buf[k] := buf[k] + k;
  inner := a + b + c + d + e + f + g + h + i + buf[7] + buf[63]
           + round(p + q + r + s + t + u + v + w);
end;

function outer(x: longint): longint;
var
  a, b, c, d, e, f, g, h, i: longint;
  p, q, r, s, t, u, v, w: single;
  z: longint;
begin
  a := x * 11; b := x * 13; c := x * 17; d := x * 19; e := x * 23;
  f := x * 29; g := x * 31; h := x * 37; i := x * 41;
  p := x * 0.5; q := x * 0.25; r := x * 0.125; s := x * 2.0;
  t := x * 4.0; u := x * 8.0; v := x * 0.75; w := x * 1.5;
  z := inner(x);
  writeln('inner ', z);
  writeln('ints  ', a, ' ', b, ' ', c, ' ', d, ' ', e, ' ', f, ' ', g, ' ', h, ' ', i);
  writeln('reals ', p:0:3, ' ', q:0:3, ' ', r:0:3, ' ', s:0:3, ' ', t:0:3, ' ', u:0:3, ' ', v:0:3, ' ', w:0:3);
  outer := z + a + i + round(p + w);
end;

begin
  writeln('outer ', outer(3));
  writeln('outer ', outer(7));
  writeln('sink  ', sink);
end.
