{ linkedlist.pas - Linked list with dynamic allocation on SLOW-32
  Demonstrates records, pointers, and list manipulation using New/Dispose. }
program linkedlist;

type
  PNode = ^TNode;
  TNode = record
    value: LongInt;
    next:  PNode;
  end;

var
  head: PNode;

procedure list_push(var h: PNode; v: LongInt);
var
  n: PNode;
begin
  New(n);
  n^.value := v;
  n^.next := h;
  h := n;
end;

procedure list_append(var h: PNode; v: LongInt);
var
  n, p: PNode;
begin
  New(n);
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
    if not first then Write(' -> ');
    Write(h^.value);
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

procedure list_free(var h: PNode);
var
  tmp: PNode;
begin
  while h <> nil do
  begin
    tmp := h;
    h := h^.next;
    Dispose(tmp);
  end;
end;

var
  i: Integer;
begin
  head := nil;

  WriteLn('Building list by appending 1..10:');
  for i := 1 to 10 do
    list_append(head, i);

  Write('  ');
  list_print(head);
  WriteLn;

  Write('  Length: ');
  WriteLn(list_length(head));

  Write('  Sum: ');
  WriteLn(list_sum(head));

  WriteLn;
  WriteLn('Reversing:');
  list_reverse(head);
  Write('  ');
  list_print(head);
  WriteLn;

  WriteLn;
  WriteLn('Pushing 99 and 42 to front:');
  list_push(head, 99);
  list_push(head, 42);
  Write('  ');
  list_print(head);
  WriteLn;

  Write('  Length: ');
  WriteLn(list_length(head));

  Write('  Sum: ');
  WriteLn(list_sum(head));

  list_free(head);
end.
