{ 64-bit shifts by a variable count (GitHub issue 98).  Free Pascal's
  fpc_shl_qword/fpc_shr_qword take the count modulo 64, and the native
  targets do the same, so a count of 64 is 0 and 96 is 32.  The SLOW-32
  code generator used the count unmasked, and its shift-by-32 in the
  wide path is a shift by 0 on this machine: q shl 64 printed the low
  half moved up, 12884901888 here, not q.  Also the small counts, which
  were right, and shifts of an int64. }
program shift64;
var
  q, r: qword;
  s, t: int64;
  n: longint;
begin
  q := 8589934595;                     { high 2, low 3 }
  n := 64; r := q shl n; writeln('shl 64  ', r);
  n := 0;  r := q shl n; writeln('shl 0   ', r);
  n := 64; r := q shr n; writeln('shr 64  ', r);
  n := 65; r := q shl n; writeln('shl 65  ', r);
  n := 96; r := q shr n; writeln('shr 96  ', r);
  n := 1;  r := q shl n; writeln('shl 1   ', r);
  n := 33; r := q shr n; writeln('shr 33  ', r);
  n := 32; r := q shl n; writeln('shl 32  ', r);
  n := 31; r := q shl n; writeln('shl 31  ', r);
  n := 63; r := q shl n; writeln('shl 63  ', r);
  n := 63; r := q shr n; writeln('shr 63  ', r);
  s := -8589934595;
  n := 64; t := s shl n; writeln('ishl 64 ', t);
  n := 64; t := s shr n; writeln('ishr 64 ', t);
  n := 4;  t := s shr n; writeln('ishr 4  ', t);
  n := 36; t := s shl n; writeln('ishl 36 ', t);
end.
