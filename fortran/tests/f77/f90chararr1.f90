! CHARACTER array assignment and expressions (Fortran 90): a scalar to
! every element, whole arrays and sections (strided, reversed) of other
! lengths, padded or truncated; concatenation, comparison, COUNT/ANY/ALL
! and WHERE over CHARACTER arrays; a substring of each element of a
! section, as a value and as the target (C(:)(3:4) = ...); overlap of the
! target with the right side (C = C(5:1:-1)); rank 2; LEN_TRIM and INDEX
! elementwise.
program f90chararr1
  implicit none
  character(len=4) :: c(5), d(5), e(3,2)
  character(len=6) :: w(5)
  character(len=2) :: s(5)
  logical :: l(5)
  integer :: n(5), i
  c = 'ab'
  print *, c
  d = "x"
  c(2:4) = 'wxyz'
  print *, '[', c, ']'
  do i = 1, 5
    d(i) = char(ichar('a') + i - 1) // char(ichar('A') + i - 1)
  end do
  w = d // '!'
  print *, '[', w, ']'
  w = 'q' // d(5:1:-1)
  print *, '[', w, ']'
  s = d
  print *, '[', s, ']'
  c(1:5:2) = d(3:5)
  print *, '[', c, ']'
  c = c(5:1:-1)
  print *, '[', c, ']'
  c(:)(3:4) = '12'
  print *, '[', c, ']'
  s = c(:)(2:3)
  print *, '[', s, ']'
  l = c > 'b'
  print *, l
  print *, count(c == 'aA12'), any(d == 'cC'), all(d /= 'zz')
  where (d > 'bB') c = 'big'
  print *, '[', c, ']'
  where (d > 'cC')
    c = d
  elsewhere
    c(:)(1:1) = '-'
  end where
  print *, '[', c, ']'
  e = 'ee'
  e(2,:) = d(1:2)
  e(:,2)(2:2) = '#'
  print *, '[', e, ']'
  n = len_trim(w)
  print *, n
  n = index(c, 'g')
  print *, n
end program
