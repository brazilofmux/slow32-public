! CHARACTER arrays in the array-valued intrinsics (Fortran 90): RESHAPE
! with PAD, TRANSPOSE, PACK, UNPACK with a scalar and an array FIELD,
! CSHIFT, EOSHIFT (blanks by default, a BOUNDARY), SPREAD of a scalar and
! of a section, MERGE elementwise and of scalars; constructors in
! expressions and I/O lists, implied-DO and substrings among the values;
! array expressions as actuals (C // '+', PACK(...), a constructor),
! their length the elements'; LEN_TRIM and MAXVAL over them.
program f90charav1
  implicit none
  character(len=3) :: c(6), m(2,3), t(3,2), p(6)
  character(len=5) :: w(4)
  logical :: k(6)
  integer :: i, n(4)
  c = (/ 'abc', 'def', 'ghi', 'jkl', 'mno', 'pqr' /)
  m = reshape(c, (/ 2, 3 /))
  print *, m
  t = transpose(m)
  print *, t
  print *, reshape(c(1:4), (/ 3, 2 /), pad = (/ 'XYZ' /))
  k = c > 'ggg'
  print *, pack(c, k), size(pack(c, k))
  p = unpack(c(1:3), k, '---')
  print *, p
  p = unpack(c(4:6), .not. k, c)
  print *, p
  print *, cshift(c, 2)
  print *, eoshift(c, -2)
  print *, '[', eoshift(c, 3, '***'), ']'
  print *, spread('ab', 1, 3), spread(c(1:2), 2, 2)
  print *, merge(c, 'zzz', k)
  print *, merge('yes', 'no ', c(1) == 'abc')
  w = (/ 'one  ', 'two  ', 'three', 'four ' /) // ''
  print *, (/ 'aa', 'bb', 'cc' /) // '!'
  print *, (/ (c(i)(1:1), i = 1, 6) /)
  call show(c // '+', 6)
  call show(pack(c, k), count(k))
  call show((/ 'A', 'B' /), 2)
  n = len_trim(w)
  print *, n
  print *, cshift(w(:)(1:2), 1)
  print *, maxval(len_trim(w))
contains
  subroutine show(x, k)
    integer :: k
    character(len=*) :: x(k)
    print *, k, len(x), '[', x, ']'
  end subroutine
end program
