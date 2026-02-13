{ io_helpers.pas - Basic I/O routines for SLOW-32 embedded target
  Since the embedded system unit has no Write/WriteLn, we build
  output from putchar. }
program io_helpers;

procedure putchar(c: Char); external name 'putchar';

procedure write_char(c: Char);
begin
  putchar(c);
end;

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
  neg: Boolean;
  u: LongWord;
begin
  if n = 0 then
  begin
    putchar('0');
    exit;
  end;

  neg := n < 0;
  if neg then
    u := LongWord(-n)
  else
    u := LongWord(n);

  len := 0;
  while u > 0 do
  begin
    buf[len] := Chr(Ord('0') + (u mod 10));
    u := u div 10;
    Inc(len);
  end;

  if neg then
    putchar('-');

  for i := len - 1 downto 0 do
    putchar(buf[i]);
end;

{ --- Demo --- }
begin
  write_str('Testing I/O helpers:');
  write_ln;

  write_str('  Positive: ');
  write_int(12345);
  write_ln;

  write_str('  Negative: ');
  write_int(-9876);
  write_ln;

  write_str('  Zero: ');
  write_int(0);
  write_ln;

  write_str('  Max LongInt: ');
  write_int(2147483647);
  write_ln;

  write_str('Done.');
  write_ln;
end.
