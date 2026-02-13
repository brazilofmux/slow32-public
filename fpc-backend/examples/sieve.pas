{ sieve.pas - Sieve of Eratosthenes on SLOW-32
  Demonstrates arrays, boolean logic, and nested loops. }
program sieve;

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

const
  LIMIT = 1000;

var
  is_prime: array[2..LIMIT] of Boolean;
  i, j, count, col: Integer;

begin
  write_str('Sieve of Eratosthenes: primes up to ');
  write_int(LIMIT);
  write_ln;
  write_ln;

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
      if i < 10 then write_str('     ')
      else if i < 100 then write_str('    ')
      else if i < 1000 then write_str('   ');
      write_int(i);

      Inc(count);
      Inc(col);
      if col = 10 then
      begin
        write_ln;
        col := 0;
      end;
    end;
  end;

  if col <> 0 then
    write_ln;

  write_ln;
  write_str('Total primes found: ');
  write_int(count);
  write_ln;
end.
