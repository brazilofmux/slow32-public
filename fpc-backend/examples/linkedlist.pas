{ linkedlist.pas - Linked list with static node pool on SLOW-32
  Demonstrates records, pointers, and list manipulation.
  Uses a static pool since the embedded RTL has no heap manager. }
program linkedlist;

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
  POOL_SIZE = 32;

type
  PNode = ^TNode;
  TNode = record
    value: LongInt;
    next:  PNode;
  end;

var
  pool: array[0..POOL_SIZE-1] of TNode;
  pool_used: Integer;
  head: PNode;

function alloc_node: PNode;
begin
  if pool_used >= POOL_SIZE then
  begin
    write_str('ERROR: node pool exhausted');
    write_ln;
    alloc_node := nil;
    exit;
  end;
  alloc_node := @pool[pool_used];
  Inc(pool_used);
end;

procedure list_push(var h: PNode; v: LongInt);
var
  n: PNode;
begin
  n := alloc_node;
  if n = nil then exit;
  n^.value := v;
  n^.next := h;
  h := n;
end;

procedure list_append(var h: PNode; v: LongInt);
var
  n, p: PNode;
begin
  n := alloc_node;
  if n = nil then exit;
  n^.value := v;
  n^.next := nil;
  if h = nil then
    h := n
  else
  begin
    p := h;
    while p^.next <> nil do
      p := p^.next;
    p^.next := n;
  end;
end;

function list_length(h: PNode): Integer;
var
  count: Integer;
begin
  count := 0;
  while h <> nil do
  begin
    Inc(count);
    h := h^.next;
  end;
  list_length := count;
end;

function list_sum(h: PNode): LongInt;
var
  s: LongInt;
begin
  s := 0;
  while h <> nil do
  begin
    s := s + h^.value;
    h := h^.next;
  end;
  list_sum := s;
end;

procedure list_print(h: PNode);
var
  first: Boolean;
begin
  first := True;
  while h <> nil do
  begin
    if not first then write_str(' -> ');
    write_int(h^.value);
    first := False;
    h := h^.next;
  end;
end;

procedure list_reverse(var h: PNode);
var
  prev, curr, tmp: PNode;
begin
  prev := nil;
  curr := h;
  while curr <> nil do
  begin
    tmp := curr^.next;
    curr^.next := prev;
    prev := curr;
    curr := tmp;
  end;
  h := prev;
end;

var
  i: Integer;
begin
  pool_used := 0;
  head := nil;

  write_str('Building list by appending 1..10:');
  write_ln;
  for i := 1 to 10 do
    list_append(head, i);

  write_str('  ');
  list_print(head);
  write_ln;

  write_str('  Length: ');
  write_int(list_length(head));
  write_ln;

  write_str('  Sum: ');
  write_int(list_sum(head));
  write_ln;

  write_ln;
  write_str('Reversing:');
  write_ln;
  list_reverse(head);
  write_str('  ');
  list_print(head);
  write_ln;

  write_ln;
  write_str('Pushing 99 and 42 to front:');
  write_ln;
  list_push(head, 99);
  list_push(head, 42);
  write_str('  ');
  list_print(head);
  write_ln;

  write_str('  Length: ');
  write_int(list_length(head));
  write_ln;

  write_str('  Sum: ');
  write_int(list_sum(head));
  write_ln;
end.
