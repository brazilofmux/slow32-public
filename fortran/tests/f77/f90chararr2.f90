! CHARACTER arrays (Fortran 90), continued: constructors (the target among
! their values); sections and expressions in WRITE and PRINT lists, a
! substring of each element among them; an internal WRITE from the
! substrings of a section; an assumed-length dummy array assigned whole
! and by section; CHAR of an INTEGER array.
program f90chararr2
  implicit none
  character(len=3) :: c(5)
  character(len=8) :: line
  integer :: n(5), i
  c = (/ 'abc', 'def', 'ghi', 'jkl', 'mno' /)
  print *, c(2:4)
  print *, c // '.'
  print *, (c(i)(1:1), i = 1, 5)
  c(2:5) = c(1:4)
  print *, c
  c(:)(2:3) = c(:)(1:2)
  print *, c
  n = (/ 1, 2, 3, 4, 5 /)
  c = char(64 + n) // char(96 + n)
  print *, c
  call sub(c, 5)
  print *, c
  line = 'zyxwvuts'
  c = line(2:4)
  print *, c
  write (line, '(5A1)') c(:)(1:1)
  print *, line
contains
  subroutine sub(a, k)
    integer :: k
    character(len=*) :: a(k)
    a = a // '*'
    a(k:1:-2) = 'R'
  end subroutine
end program
