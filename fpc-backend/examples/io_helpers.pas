{ io_helpers.pas - Native Write/WriteLn I/O demo on SLOW-32
  Demonstrates string, integer, char, and boolean output. }
program io_helpers;
begin
  WriteLn('Testing native I/O:');

  Write('  Positive: ');
  WriteLn(12345);

  Write('  Negative: ');
  WriteLn(-9876);

  Write('  Zero: ');
  WriteLn(0);

  Write('  Max LongInt: ');
  WriteLn(2147483647);

  Write('  Char: ');
  WriteLn('A');

  WriteLn('Done.');
end.
