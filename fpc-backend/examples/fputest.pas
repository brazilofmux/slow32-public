program fputest;

var
  a, b, c: single;
  d, e: double;
  i: longint;
  u: cardinal;
  fails: longint;

procedure check(cond: boolean; const name: string);
begin
  if cond then
    writeln('PASS ', name)
  else
    begin
      writeln('FAIL ', name);
      inc(fails);
    end;
end;

begin
  fails := 0;

  { f32 arithmetic (native FADD.S/FSUB.S/FMUL.S/FDIV.S) }
  a := 1.5;
  b := 2.25;
  c := a + b;
  check(c = 3.75, 'f32 add');
  c := b - a;
  check(c = 0.75, 'f32 sub');
  c := a * b;
  check(c = 3.375, 'f32 mul');
  c := b / a;
  check(c = 1.5, 'f32 div');

  { f32 comparisons (FEQ.S/FLT.S/FLE.S) }
  check(a < b, 'f32 lt');
  check(b > a, 'f32 gt');
  check(a <= 1.5, 'f32 le');
  check(a >= 1.5, 'f32 ge');
  check(a <> b, 'f32 ne');

  { unary minus (FNEG.S) and abs (FABS.S) }
  c := -a;
  check(c = -1.5, 'f32 neg');
  check(abs(c) = 1.5, 'f32 abs');

  { int -> single (FCVT.S.W / FCVT.S.WU) }
  i := -7;
  c := i;
  check(c = -7.0, 'int32 to f32');
  u := 3000000000;
  c := u;
  check(c = 3.0E9, 'uint32 to f32');

  { single <-> double (softfloat helpers) }
  d := a;
  check(d = 1.5, 'f32 to f64');
  e := 0.125;
  c := e;
  check(c = 0.125, 'f64 to f32');

  { f64 soft arithmetic still works }
  d := 1.0;
  e := 3.0;
  d := d / e;
  check((d > 0.333) and (d < 0.334), 'f64 soft div');
  d := -d;
  check(d < 0.0, 'f64 neg');

  { mixed expression: promotes f32 to f64 }
  d := a * 2.0 + b;
  check(d = 5.25, 'mixed f32/f64 expr');

  { writeln of reals (str_real, soft f64 path) }
  writeln('single value: ', a);
  writeln('double value: ', d);

  if fails = 0 then
    writeln('ALL TESTS PASSED');
  ExitCode := fails;
end.
