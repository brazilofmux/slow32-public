! loop controls: every form that is one
module m
  integer :: mi
  real :: mx
end module
subroutine sub(i, x, d, n)
  integer :: i, n
  real :: x
  double precision :: d
  do i = 1, n
  end do
  print *, i
  do x = 1.0, 2.0, 0.25
  end do
  print *, x
  do d = 1.0d0, 0.0d0, -0.25d0
  end do
  print *, d
  print *, (i, i = n, 1, -1), i
  print *, (x * 2, x = 0.5, 2.0, 0.5), x
  print *, (d, d = 3, 1, -1), d
end subroutine
program p
  use m
  integer :: i, j, k, a(5), c
  integer*2 :: h
  integer*2 :: hh(4)
  real :: x, r(4)
  double precision :: d
  common /blk/ ci, cx
  integer :: ci
  real :: cx
  save
  call sub(i, x, d, 3)
  print *, i, x, d
  do mi = 1, 3
  end do
  do mx = 1, 2
  end do
  print *, mi, mx
  do ci = 5, 1, -2
  end do
  do cx = 0.0, 1.0, 0.5
  end do
  print *, ci, cx
  do h = 1, 4
    hh(h) = h * 2
  end do
  print *, h, hh
  print *, (hh(h), h = 4, 1, -1), h
  write (*, '(a, 4f6.2)') 'none', (r(i), i = 1, 0)
  r = (/ (i * 0.5, i = 1, 4) /)
  write (*, '(4f6.2)') (r(i), i = 1, 4)
  ! mixed types in the control
  do i = 1.9, 4.2d0, 1.1
    print *, i
  end do
  do x = 1, 2
    print *, x
  end do
  do d = 1, 2.5, 0.5
    print *, d
  end do
  print *, (x, x = 1, 3), (d, d = 0.5, 1)
  ! zero trips, negative steps, a step that does not divide
  c = 0
  do i = 5, 1
    c = c + 1
  end do
  print *, c, i
  do i = 1, 10, 4
    c = c + 1
  end do
  print *, c, i
  do x = 2.0, 1.0
    c = c + 1
  end do
  print *, c, x
  print *, (i, i = 3, 1), i
  ! an array constructor's variable is its own
  i = 77
  a = (/ (i * i, i = 1, 5) /)
  print *, a, i
  h = 5
  hh = (/ (h, h = 4, 1, -1) /)
  print *, hh, h
  ! nested, inner bounds on the outer
  print *, ((i * 10 + j, j = 1, i), i = 1, 3)
  print *, ((x + i, x = 0.25, 0.5, 0.25), i = 1, 2)
  read (*, *) (a(k), k = 1, 2)
  print *, a(1), a(2), k
end program
