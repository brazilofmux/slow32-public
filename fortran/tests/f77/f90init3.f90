program p
  type pt
    real :: x, y
  end type
  type(pt), parameter :: origin = pt(0., 0.)
  type(pt) :: q = pt(1.5, -2.)
  type(pt), parameter :: corners(2) = (/ pt(0., 1.), pt(1., 0.) /)
  print *, origin, q
  print *, corners(2)%x
end program
