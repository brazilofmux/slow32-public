! Implied DOs nested in constructors and I/O lists, their items component
! arrays, sections and array-valued references in the DO variables, their
! limits too; in WHERE masks, actual arguments and READ lists.
program s4
  implicit none
  type t
    integer :: n
    integer :: v(3)
  end type
  type(t) :: s(3)
  integer :: i, j, a(12), b(3), m(2,3)
  character(3) :: c(3)
  do i = 1, 3
    s(i)%n = i
    s(i)%v = (/ i, 10*i, 100*i /)
  end do
  print *, (s(i)%v, i = 1, 3)
  print *, (sum(s(i)%v), i = 1, 3)
  print *, ((s(i)%v(j:j+1), j = 1, 2), i = 1, 3)
  a = (/ (((/ i, j /), j = 1, 2), i = 1, 3) /)
  print *, a
  a = (/ ((i*j, j = 1, size((/ (0, j = 1, i) /))), i = 1, 3), 0, 0, 0, 0, 0, 0 /)
  print *, a
  print *, (sum((/ (j, j = 1, i) /)), i = 1, 4)
  print *, (maxval(s(i)%v) - i, sum((/ i, i /)) , i = 1, 3), 99
  b = (/ (size(pack(s(i)%v, s(i)%v > 15)), i = 1, 3) /)
  print *, b
  m = reshape((/ (cshift((/ i, i+1 /), 1), i = 1, 5, 2) /), (/ 2, 3 /))
  print *, m
  c = (/ (repeat(achar(64+i), 3), i = 1, 3) /)
  print *, (c(i:i), i = 3, 1, -1)
  print *, ((/ (c(j)(1:1), j = 1, i) /), i = 1, 3)
  write (*, '(3I5)') (s(i)%v, i = 3, 1, -1)
  print *, (dot_product(s(i)%v, (/ 1, 1, 1 /)), i = 1, size((/ 1, 2 /)))
  where ((/ (sum(s(i)%v) > 200, i = 1, 3) /)) b = -b
  print *, b
  call sub((/ (sum((/ i, j /)), i = 1, 3) /))
  j = 7
  call sub((/ (sum((/ i, j /)), i = 1, 3) /))
  open (9, status='scratch')
  write (9, *) 1, 2, 3, 4, 5, 6, 7, 8, 9
  rewind 9
  read (9, *) (s(i)%v, i = 1, 3)
  print *, s(2)%v, s(3)%v(3)
  rewind 9
  read (9, *) (s(4-i)%v(2:3), i = 1, 3)
  print *, (s(i)%v, i = 1, 3)
contains
  subroutine sub(x)
    integer :: x(:)
    print *, x, size(x)
  end subroutine
end program
