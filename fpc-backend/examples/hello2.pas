{ hello2.pas - WriteLn demo with strings, integers, and a short string variable }
program hello2;
var
  msg: string[20];
  i: Integer;
begin
  msg := 'Hello, Pascal';
  WriteLn(msg);

  Write('Counting: ');
  for i := 1 to 5 do
  begin
    if i > 1 then Write(', ');
    Write(i);
  end;
  WriteLn;
end.
