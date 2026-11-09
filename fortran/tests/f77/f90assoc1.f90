! ASSOCIATED with a TARGET that is an array or a section of one (F90
! 13.13.13): the same base, extents and strides.
program p4
  implicit none
  integer, target :: a(10), b(3,4)
  integer, pointer :: p(:), q(:,:)
  integer :: i
  a = (/ (i, i = 1, 10) /)
  p => a(2:6)
  print *, associated(p, a(2:6)), associated(p, a(2:5)), associated(p, a(3:7)), associated(p, a)
  p => a(1:10:2)
  print *, associated(p, a(1:10:2)), associated(p, a(1:5)), associated(p, a(1:9:2))
  q => b(2:3, 1:2)
  print *, associated(q, b(2:3, 1:2)), associated(q, b(1:2, 1:2)), associated(q, b)
  q => b
  print *, associated(q, b)
end program p4
