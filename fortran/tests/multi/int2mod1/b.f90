! An INTEGER*2 or LOGICAL*1 module FUNCTION's result in an output list,
! from another file: the .mod carries the result's width.
program p
  use m
  integer*2 k, j
  logical*1 a, b
  k = 21
  print *, hf(k), 5, lf(1)
  open (9, file='int2mod1.dat', form='unformatted', status='unknown')
  write (9) hf(k), hf(k + k)
  write (9) lf(1), lf(-1), hf(k)
  rewind 9
  read (9) k, j
  print *, k, j
  read (9) a, b, k
  print *, a, b, k
  close (9, status='delete')
  if (k /= 42 .or. j /= 84) stop 1
  stop 0
end program
