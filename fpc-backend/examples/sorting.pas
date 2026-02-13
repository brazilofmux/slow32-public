{ sorting.pas - Quicksort on SLOW-32
  Demonstrates records, arrays, recursion, and pointer-like indexing. }
program sorting;

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
  write_str('[');
  for i := 0 to count - 1 do
  begin
    if i > 0 then write_str(', ');
    write_int(arr[i]);
  end;
  write_str(']');
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
  write_str('Quicksort demo (');
  write_int(N);
  write_str(' elements)');
  write_ln;
  write_ln;

  { Generate pseudo-random data }
  rng_state := 42;
  for i := 0 to N - 1 do
    data[i] := rand_next mod 1000;

  write_str('Before: ');
  print_array(data, N);
  write_ln;

  quicksort(data, 0, N - 1);

  write_str('After:  ');
  print_array(data, N);
  write_ln;

  write_str('Sorted? ');
  if is_sorted(data, N) then
    write_str('YES')
  else
    write_str('NO');
  write_ln;

  { Sort already-sorted array (tests best-case behavior) }
  write_ln;
  write_str('Re-sorting already sorted array...');
  write_ln;
  quicksort(data, 0, N - 1);
  write_str('Still sorted? ');
  if is_sorted(data, N) then
    write_str('YES')
  else
    write_str('NO');
  write_ln;

  { Reverse-sorted input }
  write_ln;
  write_str('Reverse input: ');
  for i := 0 to N - 1 do
    data[i] := (N - i) * 5;
  print_array(data, N);
  write_ln;

  quicksort(data, 0, N - 1);
  write_str('Sorted:        ');
  print_array(data, N);
  write_ln;
end.
