! Arrays of structures in I/O lists and array assignment.
program p1
  implicit none
  type pt
    integer :: x, y
  end type pt
  type(pt) :: a(3), b(3)
  integer :: i
  a = (/ (pt(i, 10*i), i = 1, 3) /)
  b = a
  print *, b(2)%x, b(3)%y
  print *, a%x + a%y
  b(1:2) = a(2:3)
  print *, b
  b = a(3:1:-1)
  print *, b%x
  open (10, status='scratch')
  write (10, *) 7, 8, 9, 10, 11, 12
  rewind (10)
  read (10, *) b
  print *, b
  print *, sum(b%x), maxval(a%y)
end program p1
