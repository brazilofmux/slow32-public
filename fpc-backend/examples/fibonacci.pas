{ fibonacci.pas - Recursive vs iterative Fibonacci on SLOW-32
  Demonstrates recursion, iteration, and function calls. }
program fibonacci;

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
  WriteLn('Fibonacci (recursive):');
  for i := 0 to 20 do
    WriteLn('  fib(', i, ') = ', fib_rec(i));

  WriteLn;
  WriteLn('Fibonacci (iterative):');
  for i := 0 to 46 do
    WriteLn('  fib(', i, ') = ', fib_iter(i));

  WriteLn;
  Write('Verify: fib_rec(20) = fib_iter(20) ? ');
  if fib_rec(20) = fib_iter(20) then
    WriteLn('YES')
  else
    WriteLn('NO');
end.
