{ strings.pas - String operations on SLOW-32
  Tests ShortString and AnsiString: concat, copy, pos, length,
  comparison, delete, insert, str, val, char indexing. }
program strings;

var
  pass, fail: Integer;

procedure check(const name: string; ok: Boolean);
begin
  Write('  ', name, ': ');
  if ok then
  begin
    WriteLn('PASS');
    Inc(pass);
  end
  else
  begin
    WriteLn('FAIL');
    Inc(fail);
  end;
end;

{ --- ShortString tests --- }

procedure test_shortstr_concat;
var
  a, b, c: string[40];
begin
  WriteLn('ShortString concat:');
  a := 'Hello';
  b := ', World';
  c := a + b;
  check('basic concat', c = 'Hello, World');
  c := a + b + '!';
  check('triple concat', c = 'Hello, World!');
  c := '' + a;
  check('empty + str', c = 'Hello');
end;

procedure test_shortstr_length;
var
  s: string[50];
begin
  WriteLn('ShortString length:');
  s := 'ABCDE';
  check('length=5', Length(s) = 5);
  s := '';
  check('empty length=0', Length(s) = 0);
  s := 'X';
  check('single char length=1', Length(s) = 1);
end;

procedure test_shortstr_copy;
var
  s, r: string[40];
begin
  WriteLn('ShortString copy:');
  s := 'Hello, World';
  r := Copy(s, 1, 5);
  check('copy first 5', r = 'Hello');
  r := Copy(s, 8, 5);
  check('copy middle', r = 'World');
  r := Copy(s, 1, 100);
  check('copy past end', r = 'Hello, World');
end;

procedure test_shortstr_pos;
var
  s: string[40];
begin
  WriteLn('ShortString pos:');
  s := 'abcdefgabc';
  check('find "cd"', Pos('cd', s) = 3);
  check('find "abc"', Pos('abc', s) = 1);
  check('not found', Pos('xyz', s) = 0);
  check('find single char', Pos('g', s) = 7);
end;

procedure test_shortstr_compare;
var
  a, b: string[20];
begin
  WriteLn('ShortString comparison:');
  a := 'apple';
  b := 'banana';
  check('apple < banana', a < b);
  check('banana > apple', b > a);
  a := 'same';
  b := 'same';
  check('equal', a = b);
  check('not less when equal', not (a < b));
  a := 'abc';
  b := 'abcd';
  check('shorter < longer prefix', a < b);
end;

procedure test_shortstr_delete;
var
  s: string[40];
begin
  WriteLn('ShortString delete:');
  s := 'Hello, World';
  Delete(s, 6, 7);
  check('delete middle', s = 'Hello');
  s := 'ABCDE';
  Delete(s, 1, 2);
  check('delete from start', s = 'CDE');
  s := 'ABCDE';
  Delete(s, 4, 10);
  check('delete past end', s = 'ABC');
end;

procedure test_shortstr_insert;
var
  s: string[40];
begin
  WriteLn('ShortString insert:');
  s := 'HelloWorld';
  Insert(', ', s, 6);
  check('insert middle', s = 'Hello, World');
  s := 'World';
  Insert('Hello ', s, 1);
  check('insert at start', s = 'Hello World');
  s := 'Hello';
  Insert('!', s, 6);
  check('insert at end', s = 'Hello!');
end;

procedure test_shortstr_str_val;
var
  s: string[20];
  n: LongInt;
  code: Integer;
begin
  WriteLn('ShortString str/val:');
  Str(12345, s);
  check('str positive', s = '12345');
  Str(-42, s);
  check('str negative', s = '-42');
  Str(0, s);
  check('str zero', s = '0');
  Val('9876', n, code);
  check('val valid', (n = 9876) and (code = 0));
  Val('-100', n, code);
  check('val negative', (n = -100) and (code = 0));
  Val('abc', n, code);
  check('val invalid', code <> 0);
end;

procedure test_shortstr_indexing;
var
  s: string[20];
begin
  WriteLn('ShortString char indexing:');
  s := 'ABCDE';
  check('s[1]=A', s[1] = 'A');
  check('s[3]=C', s[3] = 'C');
  check('s[5]=E', s[5] = 'E');
  s[3] := 'X';
  check('write s[3]:=X', s = 'ABXDE');
end;

{ --- AnsiString tests --- }

procedure test_ansistr_concat;
var
  a, b, c: AnsiString;
begin
  WriteLn('AnsiString concat:');
  a := 'Hello';
  b := ', World';
  c := a + b;
  check('basic concat', c = 'Hello, World');
  c := a + b + '!';
  check('triple concat', c = 'Hello, World!');
end;

procedure test_ansistr_length;
var
  s: AnsiString;
begin
  WriteLn('AnsiString length:');
  s := 'ABCDEFGHIJ';
  check('length=10', Length(s) = 10);
  s := '';
  check('empty length=0', Length(s) = 0);
end;

procedure test_ansistr_copy;
var
  s, r: AnsiString;
begin
  WriteLn('AnsiString copy:');
  s := 'Hello, World';
  r := Copy(s, 1, 5);
  check('copy first 5', r = 'Hello');
  r := Copy(s, 8, 5);
  check('copy middle', r = 'World');
end;

procedure test_ansistr_pos;
var
  s: AnsiString;
begin
  WriteLn('AnsiString pos:');
  s := 'The quick brown fox';
  check('find "quick"', Pos('quick', s) = 5);
  check('find "fox"', Pos('fox', s) = 17);
  check('not found', Pos('dog', s) = 0);
end;

procedure test_ansistr_setlength;
var
  s: AnsiString;
begin
  WriteLn('AnsiString setlength:');
  s := 'Hello';
  SetLength(s, 3);
  check('truncate to 3', (Length(s) = 3) and (s = 'Hel'));
  SetLength(s, 6);
  check('extend to 6', Length(s) = 6);
  { first 3 chars preserved after extend }
  check('preserved prefix', (s[1] = 'H') and (s[2] = 'e') and (s[3] = 'l'));
end;

procedure test_ansistr_compare;
var
  a, b: AnsiString;
begin
  WriteLn('AnsiString comparison:');
  a := 'alpha';
  b := 'beta';
  check('alpha < beta', a < b);
  check('beta > alpha', b > a);
  a := 'match';
  b := 'match';
  check('equal', a = b);
end;

procedure test_ansistr_loop_build;
var
  s: AnsiString;
  i: Integer;
begin
  WriteLn('AnsiString loop build:');
  s := '';
  for i := 1 to 5 do
  begin
    if i > 1 then
      s := s + ', ';
    Str(i * 10, s);
    { Str overwrites s, so we need a different approach }
  end;
  { Use concat approach instead }
  s := '';
  for i := 1 to 5 do
  begin
    if i > 1 then
      s := s + '-';
    s := s + Chr(Ord('A') + i - 1);
  end;
  check('loop result', s = 'A-B-C-D-E');
  check('loop length', Length(s) = 9);
end;

begin
  pass := 0;
  fail := 0;

  WriteLn('=== String Operations Test ===');
  WriteLn;

  { ShortString tests }
  test_shortstr_concat;
  test_shortstr_length;
  test_shortstr_copy;
  test_shortstr_pos;
  test_shortstr_compare;
  test_shortstr_delete;
  test_shortstr_insert;
  test_shortstr_str_val;
  test_shortstr_indexing;

  WriteLn;

  { AnsiString tests }
  test_ansistr_concat;
  test_ansistr_length;
  test_ansistr_copy;
  test_ansistr_pos;
  test_ansistr_setlength;
  test_ansistr_compare;
  test_ansistr_loop_build;

  WriteLn;
  WriteLn('=== Results: ', pass, ' passed, ', fail, ' failed ===');
end.
