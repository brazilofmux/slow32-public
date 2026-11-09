program p
  type buf
    integer :: n
    real, pointer :: d(:)
  end type
  type(buf) :: b
  real, pointer :: q(:)
  allocate(b%d(3))
  b%d = (/ 1.5, 2.5, 3.5 /)
  print *, b%d
  print *, size(b%d), sum(b%d), b%d(2)
  q => b%d
  print *, size(q), sum(q), maxval(b%d)
  b%d(2:3) = 9.0
  print *, b%d(2:3), lbound(b%d), ubound(b%d)
end program
