{ fibonacci.pas - Recursive vs iterative Fibonacci on SLOW-32
  Demonstrates recursion, iteration, and function calls. }
program fibonacci;

procedure putchar(c: Char); external name 'putchar';

procedure write_ln;
begin
  putchar(Chr(10));
end;

procedure write_str(const s: ShortString);
var
  i: Integer;
begin
  for i := 1 to Length(s) do
    putchar(s[i]);
end;

procedure write_int(n: LongInt);
var
  buf: array[0..11] of Char;
  i, len: Integer;
  u: LongWord;
begin
  if n = 0 then begin putchar('0'); exit; end;
  if n < 0 then begin putchar('-'); u := LongWord(-n); end
  else u := LongWord(n);
  len := 0;
  while u > 0 do begin
    buf[len] := Chr(Ord('0') + (u mod 10));
    u := u div 10;
    Inc(len);
  end;
  for i := len - 1 downto 0 do
    putchar(buf[i]);
end;

{ Recursive Fibonacci }
function fib_rec(n: Integer): LongInt;
begin
  if n <= 1 then
    fib_rec := n
  else
    fib_rec := fib_rec(n - 1) + fib_rec(n - 2);
end;

{ Iterative Fibonacci }
function fib_iter(n: Integer): LongInt;
var
  a, b, tmp: LongInt;
  i: Integer;
begin
  if n <= 1 then begin fib_iter := n; exit; end;
  a := 0;
  b := 1;
  for i := 2 to n do
  begin
    tmp := a + b;
    a := b;
    b := tmp;
  end;
  fib_iter := b;
end;

var
  i: Integer;
begin
  write_str('Fibonacci (recursive):');
  write_ln;
  for i := 0 to 20 do
  begin
    write_str('  fib(');
    write_int(i);
    write_str(') = ');
    write_int(fib_rec(i));
    write_ln;
  end;

  write_ln;
  write_str('Fibonacci (iterative):');
  write_ln;
  for i := 0 to 46 do
  begin
    write_str('  fib(');
    write_int(i);
    write_str(') = ');
    write_int(fib_iter(i));
    write_ln;
  end;

  write_ln;
  write_str('Verify: fib_rec(20) = fib_iter(20) ? ');
  if fib_rec(20) = fib_iter(20) then
    write_str('YES')
  else
    write_str('NO');
  write_ln;
end.
