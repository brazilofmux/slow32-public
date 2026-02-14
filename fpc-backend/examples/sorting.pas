{ sorting.pas - Quicksort on SLOW-32
  Demonstrates records, arrays, recursion, and pointer-like indexing. }
program sorting;

const
  N = 20;

type
  TArray = array[0..N-1] of LongInt;

var
  data: TArray;

procedure swap(var a, b: LongInt);
var
  tmp: LongInt;
begin
  tmp := a;
  a := b;
  b := tmp;
end;

procedure quicksort(var arr: TArray; lo, hi: Integer);
var
  pivot: LongInt;
  i, j: Integer;
begin
  if lo >= hi then exit;

  { Median-of-three pivot }
  pivot := arr[(lo + hi) div 2];
  i := lo;
  j := hi;

  while i <= j do
  begin
    while arr[i] < pivot do Inc(i);
    while arr[j] > pivot do Dec(j);
    if i <= j then
    begin
      swap(arr[i], arr[j]);
      Inc(i);
      Dec(j);
    end;
  end;

  quicksort(arr, lo, j);
  quicksort(arr, i, hi);
end;

procedure print_array(const arr: TArray; count: Integer);
var
  i: Integer;
begin
  Write('[');
  for i := 0 to count - 1 do
  begin
    if i > 0 then Write(', ');
    Write(arr[i]);
  end;
  Write(']');
end;

{ Simple linear congruential RNG }
var
  rng_state: LongWord;

function rand_next: LongInt;
begin
  rng_state := rng_state * 1103515245 + 12345;
  rand_next := LongInt((rng_state shr 16) and $7FFF);
end;

function is_sorted(const arr: TArray; count: Integer): Boolean;
var
  i: Integer;
begin
  is_sorted := True;
  for i := 0 to count - 2 do
    if arr[i] > arr[i + 1] then
    begin
      is_sorted := False;
      exit;
    end;
end;

var
  i: Integer;

begin
  WriteLn('Quicksort demo (', N, ' elements)');
  WriteLn;

  { Generate pseudo-random data }
  rng_state := 42;
  for i := 0 to N - 1 do
    data[i] := rand_next mod 1000;

  Write('Before: ');
  print_array(data, N);
  WriteLn;

  quicksort(data, 0, N - 1);

  Write('After:  ');
  print_array(data, N);
  WriteLn;

  Write('Sorted? ');
  if is_sorted(data, N) then
    WriteLn('YES')
  else
    WriteLn('NO');

  { Sort already-sorted array (tests best-case behavior) }
  WriteLn;
  WriteLn('Re-sorting already sorted array...');
  quicksort(data, 0, N - 1);
  Write('Still sorted? ');
  if is_sorted(data, N) then
    WriteLn('YES')
  else
    WriteLn('NO');

  { Reverse-sorted input }
  WriteLn;
  Write('Reverse input: ');
  for i := 0 to N - 1 do
    data[i] := (N - i) * 5;
  print_array(data, N);
  WriteLn;

  quicksort(data, 0, N - 1);
  Write('Sorted:        ');
  print_array(data, N);
  WriteLn;
end.
