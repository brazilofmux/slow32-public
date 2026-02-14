{ nqueens.pas - N-Queens solver on SLOW-32
  Demonstrates backtracking recursion, arrays, boolean logic,
  and formatted board output. }
program nqueens;

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
  Write('  +');
  for c := 0 to n - 1 do
    Write('---+');
  WriteLn;

  for r := 0 to n - 1 do
  begin
    Write(r + 1, ' |');
    for c := 0 to n - 1 do
    begin
      if q[r] = c then
        Write(' Q |')
      else
        Write('   |');
    end;
    WriteLn;

    Write('  +');
    for c := 0 to n - 1 do
      Write('---+');
    WriteLn;
  end;

  { Column labels }
  Write('   ');
  for c := 0 to n - 1 do
    Write(' ', Chr(Ord('a') + c), '  ');
  WriteLn;
end;

var
  n: Integer;
begin
  WriteLn('N-Queens Solver');
  WriteLn('===============');
  WriteLn;

  for n := 1 to 10 do
  begin
    board_size := n;
    solutions := 0;
    found_first := False;
    solve(0);

    Write('  N=', n, ': ', solutions, ' solution');
    if solutions <> 1 then Write('s');
    WriteLn;
  end;

  { Show the first 8-Queens solution }
  WriteLn;
  WriteLn('First 8-Queens solution:');
  board_size := 8;
  solutions := 0;
  found_first := False;
  solve(0);
  print_board(first_solution, 8);
end.
