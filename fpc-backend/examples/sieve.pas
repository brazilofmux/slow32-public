{ sieve.pas - Sieve of Eratosthenes on SLOW-32
  Demonstrates arrays, boolean logic, and nested loops. }
program sieve;

const
  LIMIT = 1000;

var
  is_prime: array[2..LIMIT] of Boolean;
  i, j, count, col: Integer;

begin
  WriteLn('Sieve of Eratosthenes: primes up to ', LIMIT);
  WriteLn;

  { Initialize all as prime }
  for i := 2 to LIMIT do
    is_prime[i] := True;

  { Sieve }
  i := 2;
  while i * i <= LIMIT do
  begin
    if is_prime[i] then
    begin
      j := i * i;
      while j <= LIMIT do
      begin
        is_prime[j] := False;
        j := j + i;
      end;
    end;
    Inc(i);
  end;

  { Print primes in columns }
  count := 0;
  col := 0;
  for i := 2 to LIMIT do
  begin
    if is_prime[i] then
    begin
      { Right-justify in 6-char field }
      if i < 10 then Write('     ')
      else if i < 100 then Write('    ')
      else if i < 1000 then Write('   ');
      Write(i);

      Inc(count);
      Inc(col);
      if col = 10 then
      begin
        WriteLn;
        col := 0;
      end;
    end;
  end;

  if col <> 0 then
    WriteLn;

  WriteLn;
  WriteLn('Total primes found: ', count);
end.
