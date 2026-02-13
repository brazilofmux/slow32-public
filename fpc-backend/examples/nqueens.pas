{ nqueens.pas - N-Queens solver on SLOW-32
  Demonstrates backtracking recursion, arrays, boolean logic,
  and formatted board output. }
program nqueens;

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
  MAX_N = 12;

var
  board_size: Integer;
  queens: array[0..MAX_N-1] of Integer;  { queens[row] = column }
  solutions: LongInt;
  first_solution: array[0..MAX_N-1] of Integer;
  found_first: Boolean;

function abs_val(x: Integer): Integer;
begin
  if x < 0 then abs_val := -x
  else abs_val := x;
end;

function is_safe(row, col: Integer): Boolean;
var
  r: Integer;
begin
  is_safe := True;
  for r := 0 to row - 1 do
  begin
    { Same column? }
    if queens[r] = col then begin is_safe := False; exit; end;
    { Same diagonal? }
    if abs_val(queens[r] - col) = (row - r) then begin is_safe := False; exit; end;
  end;
end;

procedure solve(row: Integer);
var
  col, i: Integer;
begin
  if row = board_size then
  begin
    Inc(solutions);
    if not found_first then
    begin
      for i := 0 to board_size - 1 do
        first_solution[i] := queens[i];
      found_first := True;
    end;
    exit;
  end;

  for col := 0 to board_size - 1 do
  begin
    if is_safe(row, col) then
    begin
      queens[row] := col;
      solve(row + 1);
    end;
  end;
end;

procedure print_board(const q: array of Integer; n: Integer);
var
  r, c: Integer;
begin
  { Top border }
  write_str('  +');
  for c := 0 to n - 1 do
    write_str('---+');
  write_ln;

  for r := 0 to n - 1 do
  begin
    write_int(r + 1);
    write_str(' |');
    for c := 0 to n - 1 do
    begin
      if q[r] = c then
        write_str(' Q |')
      else
        write_str('   |');
    end;
    write_ln;

    write_str('  +');
    for c := 0 to n - 1 do
      write_str('---+');
    write_ln;
  end;

  { Column labels }
  write_str('   ');
  for c := 0 to n - 1 do
  begin
    putchar(' ');
    putchar(Chr(Ord('a') + c));
    write_str('  ');
  end;
  write_ln;
end;

var
  n: Integer;
begin
  write_str('N-Queens Solver');
  write_ln;
  write_str('===============');
  write_ln;
  write_ln;

  for n := 1 to 10 do
  begin
    board_size := n;
    solutions := 0;
    found_first := False;
    solve(0);

    write_str('  N=');
    write_int(n);
    write_str(': ');
    write_int(solutions);
    write_str(' solution');
    if solutions <> 1 then putchar('s');
    write_ln;
  end;

  { Show the first 8-Queens solution }
  write_ln;
  write_str('First 8-Queens solution:');
  write_ln;
  board_size := 8;
  solutions := 0;
  found_first := False;
  solve(0);
  print_board(first_solution, 8);
end.
