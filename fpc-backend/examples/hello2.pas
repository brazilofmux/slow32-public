program hello2;

procedure putchar(c: Char); external name 'putchar';

var
  msg: array[0..12] of Char;
  i: Integer;
begin
  msg[0]  := 'H';
  msg[1]  := 'e';
  msg[2]  := 'l';
  msg[3]  := 'l';
  msg[4]  := 'o';
  msg[5]  := ',';
  msg[6]  := ' ';
  msg[7]  := 'P';
  msg[8]  := 'a';
  msg[9]  := 's';
  msg[10] := 'c';
  msg[11] := 'a';
  msg[12] := 'l';

  for i := 0 to 12 do
    putchar(msg[i]);
  putchar(Chr(10));
end.
